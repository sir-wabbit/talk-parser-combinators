package wabbitparse

import scala.collection.immutable.NumericRange
import scala.language.implicitConversions

trait ParserT {
  case class Parser[E, +T](run: Stream[E] => List[(List[E], T, Stream[E])])

  // ################################################################
  // Basic parsers.
  // ################################################################
  /** Consumes no input and succeeds with value `a`. */
  def success[E, A](a: A): Parser[E, A] =
    Parser[E, A] { s => List((Nil, a, s))}

  /** Consumes no input and succeeds returning `()`. */
  def unit[E]: Parser[E, Unit] = success(())

  /** Consumes no input and fails. */
  def fail[E]: Parser[E, Nothing] = Parser[E, Nothing] { s => Nil }

  /** Consumes a single character. */
  def any[E]: Parser[E, E] =
    Parser[E, E] { s =>
      if (s.isEmpty) Nil
      else List((List(s.head), s.head, s.tail))
    }
  /** Succeeds only if the stream is empty. */
  def eof[E]: Parser[E, Unit] =
    Parser[E, Unit] { s =>
      if (s.isEmpty) List((Nil, (), s))
      else Nil
    }

  /** Consumes a single character equivalent to `e`. */
  def one[E](e: E)(implicit eq: Equiv[E]): Parser[E, E] =
    Parser[E, E] { s =>
      if (s.isEmpty) Nil
      else if (eq.equiv(e, s.head)) List((List(s.head), s.head, s.tail))
      else Nil
    }

  /** Returns the surface-level representation of the current parsed value. */
  def surface[E, A](p: => Parser[E, A]): Parser[E, List[E]] =
    Parser[E, List[E]] { s =>
      for { (c, a, r) <- p.run(s) } yield (c, c, r)
    }

  // ################################################################
  // Core combinators.
  // ################################################################
  /** Parses `pa` and the applies `f` to its result. */
  def map[E, A, B](pa: => Parser[E, A])(f: A => B): Parser[E, B] =
    Parser[E, B] { s => for { (c, a, r) <- pa.run(s) } yield (c, f(a), r) }

  /** Parses `pa` followed by `pb` and then zips their results. */
  def zip[E, A, B](pa: => Parser[E, A], pb: => Parser[E, B]): Parser[E, (A, B)] =
    Parser[E, (A, B)] { s =>
      for {
        (c1, a, r1) <- pa.run(s)
        (c2, b, r2) <- pb.run(r1)
      } yield (c1 ++ c2, (a, b), r2)
    }

  /** Parses `p` and returns its result if the given predicate is satisfied. */
  def filter[E, A](p: => Parser[E, A])(f: A => Boolean): Parser[E, A] =
    Parser[E, A] { s =>
      p.run(s).filter { case (c, a, r) => f(a) }
    }

  /** Parses `pa` to get its result `x`, then parses `f(x)`. */
  def flatMap[E, A, B](pa: => Parser[E, A])(f: A => Parser[E, B]): Parser[E, B] =
    Parser[E, B] { s =>
      for {
        (c1, a, r1) <- pa.run(s)
        (c2, b, r2) <- f(a).run(r1)
      } yield (c1 ++ c2, b, r2)
    }
  /** Runs two parsers in parallel, combines their results. */
  def sum[E, A](pa: => Parser[E, A], pb: => Parser[E, A]): Parser[E, A] =
    Parser[E, A] { s =>
      pa.run(s) ++ pb.run(s)
    }
  /** Succeeds only if the given parser fails. */
  def not[E, A](p: => Parser[E, A]): Parser[E, Unit] =
    Parser[E, Unit] { s =>
      if (p.run(s).isEmpty) unit.run(s)
      else fail.run(s)
    }

  // ################################################################
  // Derived combinators & parsers.
  // ################################################################
  /** Same as zip, but easier to match. */
  case class ~[+A, +B](_1: A, _2: B)
  def product[E, A, B](pa: => Parser[E, A], pb: => Parser[E, B]): Parser[E, A ~ B] = ???

  /** Parses o, followed by p and c. Returns the value returned by p. */
  def between[E, O, A, C](o: => Parser[E, O], p: => Parser[E, A], c: => Parser[E, C]): Parser[E, A] = ???
  /** Parses p, followed by c. Returns the value returned by c. */
  def followedBy[E, A, C](p: => Parser[E, A], c: => Parser[E, C]): Parser[E, A] = ???
  /** Parses o, followed by p. Returns the value returned by p. */
  def precededBy[E, O, A](o: => Parser[E, O], p: => Parser[E, A]): Parser[E, A] = ???

  /** If `pa` succeeds returns `Some(result)`, otherwise returns `None`. */
  def opt[E, A](pa: => Parser[E, A]): Parser[E, Option[A]] = ???

  /** Parses `p` zero or more times. */
  def rep[E, A](p: => Parser[E, A]): Parser[E, List[A]] = ???
  /** Parses `first` followed by zero or more occurrences of `rest`. */
  def rep1[E, A](first: => Parser[E, A], rest: => Parser[E, A]) : Parser[E, List[A]] = ???
  /** Parses `p` one or more times. */
  def rep1[E, A](p: => Parser[E, A]) : Parser[E, List[A]] = ???

  /** Parses `p` zero or more times separated by `s`. */
  def sep[E, A, S](p: => Parser[E, A], s: => Parser[E, S]): Parser[E, List[A]] = ???
  /** Parses `p` one or more times separated by `s`. */
  def sep1[E, A, S](p: => Parser[E, A], s: => Parser[E, S]) : Parser[E, List[A]] = ???

  /** Parses `p` the given number of times `n`. */
  def times[E, A](p: => Parser[E, A], n: Int): Parser[E, List[A]] = ???

  /** Matches a consecutive list of elements. */
  def string[E](str: List[E])(implicit eq: Equiv[E]): Parser[E, List[E]] = ???
  /** Matches a string of characters. */
  def string(str: String): Parser[Char, String] = ???

  /** Matches one of several elements. */
  def oneOf[E](list: List[E])(implicit eq: Equiv[E]): Parser[E, E] = ???
  /** Matches one of characters. */
  def oneOf(str: String): Parser[Char, Char] = ???

  /** Matches a single element not in the given set. */
  def noneOf[E](list: List[E])(implicit eq: Equiv[E]): Parser[E, E] = ???
  /** Matches a single character not in the given set. */
  def noneOf(str: String): Parser[Char, Char] = ???

  /** Matches an element out of a specified inclusive range. */
  def range[E](lower: E, upper:E)(implicit ord: Ordering[E]): Parser[E, E] = ???

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
