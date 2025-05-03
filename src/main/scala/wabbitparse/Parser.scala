package wabbitparse

import scala.collection.immutable.NumericRange
import scala.language.{higherKinds, implicitConversions, postfixOps}

case class Parser[E, +T](run: Stream[E] => List[(List[E], T, Stream[E])])

object Parser {
  // ############ Basic parsers ############
  def success[E, A](a: A): Parser[E, A] =
    Parser[E, A] { s => List((Nil, a, s)) }
  def unit[E]: Parser[E, Unit] = success(())
  def fail[E]: Parser[E, Nothing] =
    Parser[E, Nothing] { s => Nil }

  def any[E]: Parser[E, E] = Parser[E, E] { s =>
    if (s.isEmpty) Nil else List((List(s.head), s.head, s.tail)) }
  def eof[E]: Parser[E, Unit] = Parser[E, Unit] { s =>
    if (s.isEmpty) List((Nil, (), s)) else Nil }

  def one[E](e: E)(implicit eq: Equiv[E]): Parser[E, E] = Parser[E, E] { s =>
    if (s.isEmpty) Nil
    else if (eq.equiv(s.head, e)) List((List(s.head), s.head, s.tail))
    else Nil
  }

  def surface[E, A](p: => Parser[E, A]): Parser[E, List[E]] = Parser[E, List[E]] { s =>
    p.run(s).map { case (c, a, r) => (c, c, r) }
  }

  // ############ Core combinators ############
  def map[E, A, B](pa: => Parser[E, A])(f: A => B): Parser[E, B] =
    Parser[E, B] { s => pa.run(s).map { case (c, a, r) => (c, f(a), r) } }
  def zip[E, A, B](pa: => Parser[E, A], pb: => Parser[E, B]): Parser[E, (A, B)] =
    Parser[E, (A, B)] { s0 =>
      for {
        (c1, a, s1) <- pa.run(s0)
        (c2, b, s2) <- pb.run(s1)
      } yield (c1 ++ c2, (a, b), s2)
    }

  /** Parses `p` and returns its result if the given predicate is satisfied. */
  def filter[E, A](p: => Parser[E, A])(f: A => Boolean): Parser[E, A] =
    Parser[E, A] { s =>
      for {
        (c, a, r) <- p.run(s)
        if f(a)
      } yield (c, a, r)
    }

  def flatMap[E, A, B](pa: => Parser[E, A])(f: A => Parser[E, B]): Parser[E, B] = Parser[E, B] { s0 =>
    for {
      (c1, a, s1) <- pa.run(s0)
      (c2, b, s2) <- f(a).run(s1)
    } yield (c1 ++ c2, b, s2)
  }

  def sum[E, A](pa: => Parser[E, A], pb: => Parser[E, A]): Parser[E, A] =
    Parser[E, A] { s => pa.run(s) ++ pb.run(s) }

  def not[E, A](pa: Parser[E, A]): Parser[E, Unit] = Parser[E, Unit] { s =>
    val r = pa.run(s)
    if (r.isEmpty) List((Nil, (), s))
    else Nil
  }

  // ############ Derived combinators & parsers ###########
  case class ~[+A, +B](_1: A, _2: B)
  def product[E, A, B](pa: => Parser[E, A], pb: => Parser[E, B]): Parser[E, A ~ B] =
    map(zip(pa, pb)){ case (a, b) => new ~(a, b) }

  def opt[E, A](pa: => Parser[E, A]): Parser[E, Option[A]] =
    sum(map(pa)(Option.apply[A]), success(None))

  def between[E, O, A, C](o: => Parser[E, O], p: => Parser[E, A], c: => Parser[E, C]): Parser[E, A] =
    map(zip(zip(o, p), c)) { case ((_, x), _) => x }
  def followedBy[E, A, C](p: => Parser[E, A], c: => Parser[E, C]): Parser[E, A] =
    map(zip(p, c)) { case (x, _) => x }
  def precededBy[E, O, A](o: => Parser[E, O], p: => Parser[E, A]): Parser[E, A] =
    map(zip(o, p)) { case (_, x) => x }

  private def mkList[A](pair: ~[A, List[A]]) = pair match { case a ~ b => a :: b }
  def rep[E, A](pa: => Parser[E, A]): Parser[E, List[A]] =
    sum(rep1(pa), success(Nil))
  def rep1[E, A](first: => Parser[E, A], rest: => Parser[E, A]) : Parser[E, List[A]] =
    map(product(first, rep(rest)))(mkList[A])
  def rep1[E, A](p: => Parser[E, A]) : Parser[E, List[A]] =
    rep1(p, p)

  private def mkListOpt[A, S](triple: A ~ Option[List[A]]) = triple match {
    case x ~ Some(xs) => x :: xs
    case x ~ None => List(x)
  }
  def sep[E, A, S](p: => Parser[E, A], s: => Parser[E, S]): Parser[E, List[A]] =
    sum(sep1(p, s), success(Nil))
  def sep1[E, A, S](p: => Parser[E, A], s: => Parser[E, S]) : Parser[E, List[A]] =
    map(product(p, opt(precededBy(s, sep(p, s)))))(mkListOpt[A, S])

  def times[E, A](p: => Parser[E, A], n: Int): Parser[E, List[A]] =
    if (n <= 0) success(Nil)
    else map(product(p, times(p, n - 1)))(mkList[A])

  def string[E](str: List[E])(implicit eq: Equiv[E]): Parser[E, List[E]] = str match {
    case x :: xs => map(product(one(x), string(xs)))(mkList[E])
    case Nil => success(Nil)
  }
  def string(str: String): Parser[Char, String] =
    map(string(str.toList)){ list: List[Char] => list mkString "" }

  def oneOf[E](list: List[E])(implicit eq: Equiv[E]): Parser[E, E] = list match {
    case x :: xs => sum(one(x), oneOf(xs))
    case Nil => fail
  }
  def oneOf(str: String): Parser[Char, Char] = oneOf(str.toList)

  def noneOf[E](list: List[E])(implicit eq: Equiv[E]): Parser[E, E] =
    filter(any[E]) { x: E => !list.exists(y => eq.equiv(x, y)) }
  def noneOf(str: String): Parser[Char, Char] =
    filter(any[Char]) { x => !str.contains(x) }

  def range[E](lower: E, upper:E)(implicit ord: Ordering[E]): Parser[E, E] =
    filter(any[E]) { e: E => ord.lteq(lower, e) && ord.lteq(e, upper) }

  implicit class OpWrapper[E, A](parser: => Parser[E, A]) {
    def |(other: => Parser[E, A]): Parser[E, A] = sum(parser, other)
    def ^^[B](f: A => B): Parser[E, B] = map(parser)(f)
    def ^^^[B](f: => B): Parser[E, B] = map(parser)(x => f)

    def ~[B](other: => Parser[E, B]): Parser[E, ~[A, B]] = product(parser, other)
    def ~>[B](other: => Parser[E, B]): Parser[E, B] = precededBy(parser, other)
    def <~[B](other: => Parser[E, B]): Parser[E, A] = followedBy(parser, other)

    def unary_! : Parser[E, Unit] = not(parser)
    def ? : Parser[E, Option[A]] = opt(parser)
    def + : Parser[E, List[A]] = rep1(parser)
    def * : Parser[E, List[A]] = rep(parser)
    def *(n: Int) : Parser[E, List[A]] = times(parser, n)
  }

  implicit def string2parser(str: String): Parser[Char, String] = string(str)
  implicit def range2parser(r: NumericRange[Char]): Parser[Char, Char] = oneOf(r.toList)
}
