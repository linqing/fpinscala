package fpinscala.laziness

import Stream._

trait Stream[+A] {

  // 练习：5.1
  // The natural recursive solution
  // 自然的递归解决方案
  def toListRecursive: List[A] = this match {
    case Cons(h, t) => h() :: t().toListRecursive
    case _ => List()
  }

  /** 练习：5.1
    * The above solution will stack overflow for large streams, since it's
    * not tail-recursive. Here is a tail-recursive implementation. At each
    * step we cons onto the front of the `acc` list, which will result in the
    * reverse of the stream. Then at the end we reverse the result to get the
    * correct order again.
    * 如果steam很大的话，上面的"自然递归解决方案"不是tail-recursive的，会导致stackoverflow。
    * 这里是一个tail-recursive的实现。
    * 注意：这里最后调用了一个reverse
    */
  def toList: List[A] = {
    @annotation.tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h, t) => go(t(), h() :: acc)
      case _ => acc
    }

    go(this, List()).reverse
  }

  /** 练习：5.1
    * In order to avoid the `reverse` at the end, we could write it using a
    * mutable list buffer and an explicit loop instead. Note that the mutable
    * list buffer never escapes our `toList` method, so this function is
    * still _pure_.
    * 为了避免上一个解决方案中，最后一个reverse，这里我们用buffer来处理。
    */
  def toListFast: List[A] = {
    val buf = new collection.mutable.ListBuffer[A]

    @annotation.tailrec
    def go(s: Stream[A]): List[A] = s match {
      case Cons(h, t) =>
        buf += h()
        go(t())
      case _ => buf.toList
    }

    go(this)
  }

  /** 练习：5.2
    * Create a new Stream[A] from taking the n first elements from this. We can achieve that by recursively
    * calling take on the invoked tail of a cons cell. We make sure that the tail is not invoked unless
    * we need to, by handling the special case where n == 1 separately. If n == 0, we can avoid looking
    * at the stream at all.
    */
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  /**
    * Create a new Stream[A] from this, but ignore the n first elements. This can be achieved by recursively calling
    * drop on the invoked tail of a cons cell. Note that the implementation is also tail recursive.
    */
  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  /** 练习：5.3
    * It's a common Scala style to write method calls without `.` notation, as in `t() takeWhile f`.
    */
  def takeWhile(f: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if f(h()) => cons(h(), t() takeWhile f)
    case _ => empty
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  /** 练习：5.4
    * Since `&&` is non-strict in its second argument, this terminates the traversal as soon as a nonmatching element is found.
    */
  def forAll(f: A => Boolean): Boolean =
    foldRight(true)((a, b) => f(a) && b)

  /** 练习：5.5
    * 用foldRight来实现takeWhile
    */
  def takeWhile_1(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) =>
      if (f(h)) cons(h, t)
      else empty)

  /** 练习：5.6 难
    * 用foldRight来实现headOption
    */
  def headOption: Option[A] =
    foldRight(None: Option[A])((h, _) => Some(h))

  /** 练习：5.7
    * 实现map
    */
  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((h, t) => cons(f(h), t))

  /** 练习：5.7
    * 实现filter
    */
  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) =>
      if (f(h)) cons(h, t)
      else t)

  /** 练习：5.7
    * 实现append
    */
  def append[B >: A](s: => Stream[B]): Stream[B] =
    foldRight(s)((h, t) => cons(h, t))

  /** 练习：5.7
    * 实现flatMap
    */
  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h, t) => f(h) append t)

  /** 练习：5.13
    * 用unfold来实现map
    */
  def mapViaUnfold[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h, t) => Some((f(h()), t()))
      case _ => None
    }

  /** 练习：5.13
    * 用unfold来实现take
    */
  def takeViaUnfold(n: Int): Stream[A] =
    unfold((this, n)) {
      case (Cons(h, t), 1) => Some((h(), (empty, 0)))
      case (Cons(h, t), n) if n > 1 => Some((h(), (t(), n - 1)))
      case _ => None
    }

  /** 练习：5.13
    * 用unfold来实现takeWhile
    */
  def takeWhileViaUnfold(f: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if f(h()) => Some((h(), t()))
      case _ => None
    }

  /** 练习：5.13
    * 用unfold来实现zipWith
    */
  def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) =>
        Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    }

  // special case of `zipWith`
  def zip[B](s2: Stream[B]): Stream[(A, B)] =
    zipWith(s2)((_, _))

  /** 练习：5.13
    * 用unfold来实现zipAll
    */
  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    zipWithAll(s2)((_, _))

  def zipWithAll[B, C](s2: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] =
    Stream.unfold((this, s2)) {
      case (Empty, Empty) => None
      case (Cons(h, t), Empty) => Some(f(Some(h()), Option.empty[B]) -> (t(), empty[B]))
      case (Empty, Cons(h, t)) => Some(f(Option.empty[A], Some(h())) -> (empty[A] -> t()))
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(Some(h1()), Some(h2())) -> (t1() -> t2()))
    }

  /** 练习：5.14 难
    * `s startsWith s2` when corresponding elements of `s` and `s2` are all equal, until the point that `s2` is exhausted.
    * If `s` is exhausted first, or we find an element that doesn't match, we terminate early. Using non-strictness,
    * we can compose these three separate logical steps--the zipping, the termination when the second stream is exhausted,
    * and the termination if a nonmatching element is found or the first stream is exhausted.
    * 用之前已经实现的函数来实现startsWith
    */
  def startsWith[A](s: Stream[A]): Boolean =
    zipAll(s).takeWhile(!_._2.isEmpty) forAll {
      case (h, h2) => h == h2
    }

  /** 练习：5.15
    * The last element of `tails` is always the empty `Stream`, so we handle this as a special case, by appending it to the output.
    * 用unfold来实现tails
    */
  def tails: Stream[Stream[A]] =
    unfold(this) {
      case Empty => None
      case s => Some((s, s drop 1))
    } append Stream(empty)

  def hasSubsequence[A](s: Stream[A]): Boolean =
    tails exists (_ startsWith s)

  /** 练习： 5.16 难
    * The function can't be implemented using `unfold`, since `unfold` generates elements of the `Stream` from left to right.
    * It can be implemented using `foldRight` though.
    *
    * The implementation is just a `foldRight` that keeps the accumulated value and the stream of intermediate results,
    * which we `cons` onto during each iteration. When writing folds, it's common to have more state in the fold than is
    * needed to compute the result. Here, we simply extract the accumulated list once finished.
    * 推广`tails`，生成一个scanRight，这个scanRight函数有点像foldRight. 但是每个fold step的阶段值都记录下来作为stream元素。
    * scala> Stream(1, 2, 3).scanRight(_ + _).toList
    * res0: List[Int] = List(6, 5, 3, 0)
    */
  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, Stream(z)))((a, p0) => {
      // p0 is passed by-name and used in by-name args in f and cons. So use lazy val to ensure only one evaluation...
      lazy val p1 = p0
      val b2 = f(a, p1._1)
      (b2, cons(b2, p1._2))
    })._2

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  /** 练习：5.8
    * This is more efficient than `cons(a, constant(a))` since it's just
    * one object referencing itself.
    * 对`ones`做轻度推广，对一个给定值，生成一个无限Stream。
    */
  def constant[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }

  /** 练习：5.9
    * 生成一个从指定值`n`开始，n, n+1, n+2 ...的一个无限stream
    */
  def from(n: Int): Stream[Int] =
    cons(n, from(n + 1))

  /** 练习: 5.10
    * 生成一个菲波拉契数列[0, 1, 1, 2, 3, 5, 8, ...]无限stream
    */
  val fibs: Stream[Int] = {
    def go(f0: Int, f1: Int): Stream[Int] =
      cons(f0, go(f1, f0 + f1))

    go(0, 1)
  }

  /** 练习: 5.11
    * 生成一个比5.8, 5.9, 5.10更广义的一个stream构造函数。
    * 输入一个初始状态`z`和一个用于生成一下个值和状态的函数`f`，返回一个无限stream。
    */
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((h, s)) => cons(h, unfold(s)(f))
      case None => empty
    }

  /**
    * The below two implementations use `fold` and `map` functions in the Option class to implement unfold,
    * thereby doing away with the need to manually pattern match as in the above solution.
    */
  def unfoldViaFold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z).fold(empty[A])((p: (A, S)) => cons(p._1, unfold(p._2)(f)))

  def unfoldViaMap[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z).map((p: (A, S)) => cons(p._1, unfold(p._2)(f))).getOrElse(empty[A])

  /** 练习：5.12
    * Scala provides shorter syntax when the first action of a function literal is to match on an expression.
    * The function passed to `unfold` in `fibsViaUnfold` is equivalent to `p => p match { case (f0,f1) => ... }`,
    * but we avoid having to choose a name for `p`, only to pattern match on it.
    * 用unfold来实现fibs
    */
  val fibsViaUnfold: Stream[Int] =
    unfold((0, 1)) { case (f0, f1) => Some((f0, (f1, f0 + f1))) }

  /** 练习：5.12
    * 用unfold来实现from
    */
  def fromViaUnfold(n: Int): Stream[Int] =
    unfold(n)(n => Some((n, n + 1)))

  /** 练习：5.12
    * 用unfold来实现constant
    */
  def constantViaUnfold[A](a: A): Stream[A] =
    unfold(a)(_ => Some((a, a)))


  /** 练习：5.12
    * 用unfold来实现ones
    */
  // could also of course be implemented as constant(1)
  val onesViaUnfold: Stream[Int] = unfold(1)(_ => Some((1, 1)))
}
