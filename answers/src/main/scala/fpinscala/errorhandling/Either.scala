package fpinscala.errorhandling


//hide std library `Option` and `Either`, since we are writing our own in this chapter
import scala.{Option => _, Either => _}

sealed trait Either[+E, +A] {
  /** 练习： 4.6 1/4
    * 实现 map
    */
  def map[B](f: A => B): Either[E, B] =
    this match {
      case Right(a) => Right(f(a))
      case Left(e) => Left(e)
    }

  /** 练习： 4.6 2/4
    * 实现 flatMap
    */
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
    this match {
      case Left(e) => Left(e)
      case Right(a) => f(a)
    }

  /** 练习： 4.6 3/4
    * 实现 orElse
    */
  def orElse[EE >: E, AA >: A](b: => Either[EE, AA]): Either[EE, AA] =
    this match {
      case Left(_) => b
      case Right(a) => Right(a)
    }

  /** 练习： 4.6 4/4
    * 实现 map2
    */
  def map2[EE >: E, B, C](that: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for {a <- this; b <- that} yield f(a, b)
}
case class Left[+E](get: E) extends Either[E, Nothing]
case class Right[+A](get: A) extends Either[Nothing, A]

object Either {
  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty)
      Left("mean of empty list!")
    else
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] =
    try Right(x / y)
    catch {
      case e: Exception => Left(e)
    }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch {
      case e: Exception => Left(e)
    }

  /** 练习 4.7 1/2
    * 实现traverse
    * 第一回合
    */
  def traverse[E, A, B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    es match {
      case Nil => Right(Nil)
      case h :: t => (f(h) map2 traverse(t)(f)) (_ :: _)
    }

  /** 练习 4.7 1/2
    * 实现traverse
    * 第二回合
    */
  def traverse_1[E, A, B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    es.foldRight[Either[E, List[B]]](Right(Nil))((a, b) => f(a).map2(b)(_ :: _))

  /** 练习 4.7 2/2
    * 实现sequence, 一根杆子插过去，返回第一个Error。
    */
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    traverse(es)(x => x)

  /** 练习 4.8
    * 设计并实现一个可以处理多个错误的数据结构。
    * There are a number of variations on `Option` and `Either`. If we want to accumulate multiple errors, a simple
    * approach is a new data type that lets us keep a list of errors in the data constructor that represents failures:
    * *
    * trait Partial[+A,+B]
    * case class Errors[+A](get: Seq[A]) extends Partial[A,Nothing]
    * case class Success[+B](get: B) extends Partial[Nothing,B]
    * *
    * There is a type very similar to this called `Validation` in the Scalaz library. You can implement `map`, `map2`,
    * `sequence`, and so on for this type in such a way that errors are accumulated when possible (`flatMap` is unable to
    * accumulate errors--can you see why?). This idea can even be generalized further--we don't need to accumulate failing
    * values into a list; we can accumulate values using any user-supplied binary function.
    * *
    * It's also possible to use `Either[List[E],_]` directly to accumulate errors, using different implementations of
    * helper functions like `map2` and `sequence`.
    */
}