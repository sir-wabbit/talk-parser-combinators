package wabbitparse

import scala.language.higherKinds

object Parser0 {
  /**
   * A parser that takes a string of characters and
   * returns an optional parsed object together
   * with the unconsumed part of the string.
   */
  type Parser0[+T] = String => Option[(T, String)]

  /**
   * A parser that takes a string of characters and
   * returns a list of possible parses together
   * with the unconsumed part of the stream.
   */
  type Parser1[+T] = String => List[(T, String)]

  /**
   * A parser that takes a list of elements of type E and
   * returns an optional parsed object of type T together
   * with the unconsumed part of the stream.
   */
  type Parser2[E, +T] = List[E] => Option[(T, List[E])]

  /**
   * A parser that takes a stream of elements of type E and
   * returns a list of possible parses together with
   * the unconsumed part of the stream.
   */
  case class Parser3[E, +T](run: Stream[E] => List[(T, Stream[E])])

  /**
   * A parser that takes a stream of elements of type E and
   * returns a list of possible parses together with
   * the consumed and unconsumed part of the stream.
   */
  case class Parser4[E, +T](run: Stream[E] => List[(List[E], T, Stream[E])])

  /**
   * ...
   */
  type Parser5[Stream[+_], Elem, Result[+_], +T] = Stream[Elem] => Result[(T, Stream[Elem])]

  /**
   * ...
   */
  case class Parser6[State, Stream[+_], Elem, Buffer[+_], Result[+_], +T]
  (run: (State, Stream[Elem]) => Result[(Buffer[Elem], State, T, Stream[Elem])])

  /**
   * ...
   */
  case class Parser7[E, T]
  (parser: Stream[E] => List[(T, Stream[E])],
   matcher: Stream[E] => List[(List[E], Stream[T])])
}
