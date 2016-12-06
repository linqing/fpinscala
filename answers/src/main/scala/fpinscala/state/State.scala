package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed)
      // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  /** 练习： 6.1
    * // We need to be quite careful not to skew the generator.
    * // Since `Int.Minvalue` is 1 smaller than `-(Int.MaxValue)`,
    * // it suffices to increment the negative numbers by 1 and make them positive.
    * // This maps Int.MinValue to Int.MaxValue and -1 to 0.
    * 用RNG.nextInt来生成一个函数，返回0~Int.MaxValue(2147483647)之间的一个随机整数。
    * 注意：这里有一个极端情况，Int.MinValue(-2147483648)没有对应的正数, 代码中需要考虑这个问题.
    */
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 1) else i, r)
  }

  /** 练习：6.2
    * // We generate an integer >= 0 and divide it by one higher than the
    * // maximum. This is just one possible solution.
    * 写一个函数，随机生成[0~1)之间的Double--不包括1。
    * 提示：你可以用Int.MaxValue来取得最大正整数，你可以用x.toDouble来将整数转换为Double。
    */
  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), r)
  }

  def boolean(rng: RNG): (Boolean, RNG) =
    rng.nextInt match {
      case (i, rng2) => (i % 2 == 0, rng2)
    }

  /** 练习：6.3
    * 生成随机(Int, Double): double范围【0 ~ 1)，Int范围0~Int.MaxValue.
    */
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)
    ((i, d), r2)
  }

  /** 练习：6.3
    * 生成随机(Double, Int): double范围【0 ~ 1).，Int范围0~Int.MaxValue.
    */
  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)
  }

  /** 练习：6.3
    * 生成随机(Double, Double, Double): double范围【0 ~ 1).
    */
  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  /** 练习：6.4
    * // A simple recursive solution
    * 生成List，里面包含`count`个随机整数。
    * 简单递归方案
    */
  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    if (count == 0)
      (List(), rng)
    else {
      val (x, r1) = rng.nextInt
      val (xs, r2) = ints(count - 1)(r1)
      (x :: xs, r2)
    }

  /** 练习：6.4
    * // A tail-recursive solution
    * 生成List，里面包含`count`个随机整数。
    * 尾递归方案
    */
  def ints2(count: Int)(rng: RNG): (List[Int], RNG) = {
    def go(count: Int, r: RNG, xs: List[Int]): (List[Int], RNG) =
      if (count == 0)
        (xs, r)
      else {
        val (x, r2) = r.nextInt
        go(count - 1, r2, x :: xs)
      }

    go(count, rng, List())
  }

  // There is something terribly repetitive about passing the RNG along
  // every time. What could we do to eliminate some of this duplication
  // of effort?
  // 好可怕呀！每个操作都要把RNG传来传去！！
  // 怎么才能避免搞死人的，把状态传来传去的工作呢？
  // 画外音：当然是搞点代码来专门做这种传来传去的事情了！

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  /** 练习： 6.5
    * 用map来实现随机double。
    */
  val _double: Rand[Double] =
    map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))


  /** ------------章6节4---------------------------------------------
    * 此乃Action第一次出场！
    * --------------------------------------------------------------- */
  /** 练习： 6.6
    * 实现map2.
    * 用规定动作`f`：(A, B) => C，让两个action，`Rand[A]`和`Rand[B]`灵肉合一为`Rand[C]`。
    */
  // This implementation of map2 passes the initial RNG to the first argument
  // and the resulting RNG to the second argument. It's not necessarily wrong
  // to do this the other way around, since the results are random anyway.
  // We could even pass the initial RNG to both `f` and `g`, but that might
  // have unexpected results. E.g. if both arguments are `RNG.int` then we would
  // always get two of the same `Int` in the result. When implementing functions
  // like this, it's important to consider how we would test them for
  // correctness.
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
  rng => {
    val (a, r1) = ra(rng)
    val (b, r2) = rb(r1)
    (f(a, b), r2)
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] =
    both(int, double)

  val randDoubleInt: Rand[(Double, Int)] =
    both(double, int)

  /** 练习: 6.7
    * // In `sequence`, the base case of the fold is a `unit` action that returns
    * // the empty list. At each step in the fold, we accumulate in `acc`
    * // and `f` is the current element in the list.
    * // `map2(f, acc)(_ :: _)` results in a value of type `Rand[List[A]]`
    * // We map over that to prepend (cons) the element onto the accumulated list.
    * //
    * // We are using `foldRight`. If we used `foldLeft` then the values in the
    * // resulting list would appear in reverse order. It would be arguably better
    * // to use `foldLeft` followed by `reverse`. What do you think?
    * 将一串List[Rand[Action]]撸到一起，成为随机串Rand[List[Action]].
    */
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))

  // It's interesting that we never actually need to talk about the `RNG` value
  // in `sequence`. This is a strong hint that we could make this function
  // polymorphic in that type.

  def _ints(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  /** 练习：6.8
    * 实现flatMap。
    */
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, r1) = f(rng)
      g(a)(r1) // We pass the new state along
    }

  def nonNegativeLessThan(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }
  }

  /** 练习: 6.9
    * 用flatMap重新实现map
    */
  def _map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  /** 练习: 6.9
    * 用flatMap重新实现map2
    */
  def _map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))
}

import State._

case class State[S, +A](run: S => (A, S)) {

  /** 练习：6.10
    * 总结归纳出map的定义，并在State中实现之。
    */
  def map[B](f: A => B): State[S, B] =
    flatMap(a => unit(f(a)))

  /** 练习：6.10
    * 总结归纳出map2的定义，并在State中实现之。
    */
  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))


  /** 练习：6.10
    * 总结归纳出flatMap的定义，并在State中实现之。
    */
  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, s1) = run(s)
    f(a).run(s1)
  })
}

object State {
  type Rand[A] = State[RNG, A]

  /** 练习：6.10
    * 总结归纳出unit的定义，并在State中实现之。
    */
  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

  // The idiomatic solution is expressed via foldRight
  def sequenceViaFoldRight[S, A](sas: List[State[S, A]]): State[S, List[A]] =
    sas.foldRight(unit[S, List[A]](List()))((f, acc) => f.map2(acc)(_ :: _))

  // This implementation uses a loop internally and is the same recursion
  // pattern as a left fold. It is quite common with left folds to build
  // up a list in reverse order, then reverse it at the end.
  // (We could also use a collection.mutable.ListBuffer internally.)
  def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] = {
    def go(s: S, actions: List[State[S, A]], acc: List[A]): (List[A], S) =
      actions match {
        case Nil => (acc.reverse, s)
        case h :: t => h.run(s) match {
          case (a, s2) => go(s2, t, a :: acc)
        }
      }

    State((s: S) => go(s, sas, List()))
  }

  // We can also write the loop using a left fold. This is tail recursive like the
  // previous solution, but it reverses the list _before_ folding it instead of after.
  // You might think that this is slower than the `foldRight` solution since it
  // walks over the list twice, but it's actually faster! The `foldRight` solution
  // technically has to also walk the list twice, since it has to unravel the call
  // stack, not being tail recursive. And the call stack will be as tall as the list
  // is long.
  def sequenceViaFoldLeft[S, A](l: List[State[S, A]]): State[S, List[A]] =
  l.reverse.foldLeft(unit[S, List[A]](List()))((acc, f) => f.map2(acc)(_ :: _))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get // Gets the current state and assigns it to `s`.
    _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
  } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
}

/** 练习： 6.11 可选，难
  * 为了练习上面学到的State处理技巧，实现一个自动机，对`糖果贩卖机`进行模拟。
  * 这个糖果贩卖机有两种类型的输入：
  *   1. 投币：
  *   2. 拧一下把手取糖果
  *   机器有三个状态量：锁定状态，剩余糖果数量，收纳的硬币数量。
  */
sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy {
  def update: Input => Machine => Machine =
    (i: Input) => (s: Machine) =>
      (i, s) match {
        case (_, Machine(_, 0, _)) => s
        case (Coin, Machine(false, _, _)) => s
        case (Turn, Machine(true, _, _)) => s
        case (Coin, Machine(true, candy, coin)) =>
          Machine(false, candy, coin + 1)
        case (Turn, Machine(false, candy, coin)) =>
          Machine(true, candy - 1, coin)
      }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- sequence(inputs map (modify[Machine] _ compose update))
    s <- get
  } yield (s.coins, s.candies)
}

