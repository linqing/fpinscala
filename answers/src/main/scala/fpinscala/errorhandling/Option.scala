package fpinscala.errorhandling

//hide std library `Option` and `Either`, since we are writing our own in this chapter
import scala.{Option => _, Either => _}

sealed trait Option[+A] {
  /** 练习 4.1 1/5
    * 实现map
    */
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  /** 练习 4.1 2/5
    * 实现getOrElse
    */
  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  /** 练习 4.1 3/5
    * 实现flatMap
    * 第一版： 使用map和getOrElse实现。
    */
  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f) getOrElse None

  /** 练习 4.1 3/5
    * 实现flatMap
    *
    * Of course, we can also implement `flatMap` with explicit pattern matching.
    * 第二版： 使用Pattern Matching实现。
    */
  def flatMap_1[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(a) => f(a)
  }


  /** 练习 4.1 4/5
    * 实现orElse
    * 第一版： 用map实现。
    */
  def orElse[B >: A](ob: => Option[B]): Option[B] =
    this map (Some(_)) getOrElse ob

  /** 练习 4.1 2/5
    * 实现orElse
    * Again, we can implement this with explicit pattern matching.
    * 第二版：显式Pattern matching实现
    */
  def orElse_1[B >: A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case _ => this
  }

  /** 练习 4.1 4/5
    * 实现filter
    * 第一版： 用pattern matching实现。
    */
  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) if f(a) => this
    case _ => None
  }

  /** 练习 4.1 4/5
    * 实现filter
    * This can also be defined in terms of `flatMap`.
    * 第二版： 用flatMap实现。
    */
  def filter_1(f: A => Boolean): Option[A] =
    flatMap(a => if (f(a)) Some(a) else None)
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    val y: Int = throw new Exception("fail!")
    try {
      val x = 42 + 5
      x + y
    }
    // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern
    // that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
    catch {
      case _: Exception => 43
    }
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      // A thrown Exception can be given any type; here we're annotating it with the type `Int`
      x + ((throw new Exception("fail!")): Int)
    }
    catch {
      case _: Exception => 43
    }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  /** 练习： 4.2
    * 用flatMap实现求方差函数variance
    * https://en.wikipedia.org/wiki/Variance
    * 用flatMap处理： 撸函数嵌套调用撸函数。
    */
  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))

  /** 练习： 4.3
    * // a bit later in the chapter we'll learn nicer syntax for
    * // writing functions like this
    * 实现map2函数： 两个都就绪了才能撸，差一个都撸不成。
    */
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap (aa => b map (bb => f(aa, bb)))

  /** 练习：4.4
    * Here's an explicit recursive version:
    * 实现sequence函数。一串撸=>撸一串, 遇到任何一个Nil，整个结果就是Nil。
    * 第一版： 用Pattern Matching实现。
    */
  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a match {
      case Nil => Some(Nil)
      case h :: t => h flatMap (hh => sequence(t) map (hh :: _))
    }

  /** 练习：4.4
    * It can also be implemented using `foldRight` and `map2`. The type annotation on `foldRight` is needed here; otherwise
    * Scala wrongly infers the result type of the fold as `Some[Nil.type]` and reports a type error (try it!). This is an
    * unfortunate consequence of Scala using subtyping to encode algebraic data types.
    * 实现sequence函数。
    * 第二版： 用foldRight实现。
    */
  def sequence_1[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight[Option[List[A]]](Some(Nil))((x, y) => map2(x, y)(_ :: _))

  /** 练习： 4.5
    * 实现traverse： 用一个可能走不通的函数，在一个串里面走啊走，走到走不通了就返回。
    * 返回已经走过的列表结果。
    * 第一回合： 用pattern match加map2实现。
    */
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a match {
      case Nil => Some(Nil)
      case h :: t => map2(f(h), traverse(t)(f))(_ :: _)
    }

  /** 练习： 4.5
    * 实现traverse： 用一个可能走不通的函数，在一个串里面走啊走，走到走不通了就返回。
    * 返回已经走过的列表结果。
    * 第二回合： 用pattern match加map2实现。
    */
  def traverse_1[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight[Option[List[B]]](Some(Nil))((h, t) => map2(f(h), t)(_ :: _))

  /** 练习： 4.5
    * 第三回合： 用traverse来实现sequence。
    */
  def sequenceViaTraverse[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(x => x)
}
