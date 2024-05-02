package fpinscala.exercises.state

import util.chaining.scalaUtilChainingOps
import fpinscala.exercises.state.RNG.sequence

trait RNG:
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.

object RNG:
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG:
    def nextInt: (Int, RNG) =
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng =>
      val (a, rng2) = s(rng)
      (f(a), rng2)

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (x, newRng) = rng.nextInt
    val absX = if (x == Int.MinValue) 0  else Math.abs(x)
    absX -> newRng
  }

  def double(rng: RNG): (Double, RNG) = {
    val (absX, newRng) = nonNegativeInt(rng)
    val between0And1 = absX.toDouble / Int.MaxValue.toDouble
    between0And1 -> newRng
  }

  val doubleViaMap: Rand[Double] = map(nonNegativeInt)(_.toDouble / Int.MaxValue.toDouble)

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (absX, rng2) = nonNegativeInt(rng)
    val (x, rng3) = double(rng2)
    (absX -> x, rng3)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (x, rng2) = double(rng)
    val (absX, rng3) = nonNegativeInt(rng2)
    (x -> absX, rng3)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (x1, rng2) = double(rng)
    val (x2, rng3) = double(rng2)
    val (x3, rng4) = double(rng3)
    (x1, x2, x3) -> rng4
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    List.fill(count)(0).foldRight(List.empty[Int] -> rng){
      case (_ ,(list, rng)) =>
        val (x, newRng) = rng.nextInt
        (x :: list, newRng)
    }
  }

  def intsBySequence(count: Int): Rand[List[Int]] = sequence(List.fill(count)(int))

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, rng2) = ra(rng)
    val (b, rng3) = rb(rng2)
    (f(a,b), rng3)
  }

  def sequence[A](rs: List[Rand[A]]): Rand[List[A]] = rng => {
    rs.foldRight(List.empty[A] -> rng){
      case (rand, (list, rng)) =>
        val (e, nextRng) = rand(rng)
        (e :: list, nextRng)
    }
  }

  def flatMap[A, B](r: Rand[A])(f: A => Rand[B]): Rand[B] = rng => {
    val (e, rng2) = r(rng)
    f(e)(rng2)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = flatMap(nonNegativeInt){ i =>
    val mod = i % n
    if (i + (n-1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
  }

  def mapViaFlatMap[A, B](r: Rand[A])(f: A => B): Rand[B] = flatMap(r)(a => unit(f(a)))

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    flatMap(ra)(a => mapViaFlatMap(rb)(b => f(a,b)))
  }

opaque type State[S, +A] = S => (A, S)

object State:
  extension [S, A](underlying: State[S, A])
    def run(s: S): (A, S) = underlying(s)

    def get: State[S, S] = s => (s, s)

    def set(s: S): State[S, Unit] = _ => ((), s)

    def modify(f: S => S): State[S, Unit] = for {
      s <- get
      _ <- set(f(s))
    } yield ()

    def map[B](f: A => B): State[S, B] = flatMap(a => unit(f(a)))

    def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = flatMap(a => sb.map(f(a,_)))

    def flatMap[B](f: A => State[S, B]): State[S, B] = s => {
      val (a, newS) = run(s)
      f(a).run(newS)
    }

  def apply[S, A](f: S => (A, S)): State[S, A] = f

  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def sequence[S, A](states: List[State[S, A]]): State[S, List[A]] = 
    states.foldRight(unit[S, List[A]](Nil))((state, acc) => state.map2(acc)(_ :: _))

enum Input:
  case Coin, Turn

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy {

  val emptyTransition: PartialFunction[Machine, Machine] = {
    case mach @ Machine(_, candies, _) if (candies < 1) => mach
  }

  val coinTransitions: PartialFunction[Machine, Machine] = {
    case mach @ Machine(true, candies, coins) if (candies > 0) =>
      println(s"candies > 0:$mach")
      throw new Exception("Not matching were expected")
      mach.copy(locked = false, coins = coins + 1)
    case mach @ Machine(false, _, coins) => println(s"candies > 0:$mach")
      throw new Exception("Not matching were expected")
      mach.copy(coins = coins + 1)
    case mach: Machine => throw new Exception("Not matching were expected")
  }

  val turnTransitions: PartialFunction[Machine, Machine] = {
    case mach @ Machine(false, candies, _) if (candies > 0) => mach.copy(locked = true, candies = candies - 1)
    case mach: Machine => mach
  }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    val startState = State.unit[Machine, Unit](())
    val finalState = inputs.foldRight(startState) { (input, accState) =>
      input match {
        case Input.Coin => accState.modify(coinTransitions)
        case Input.Turn => accState.modify(turnTransitions)
      }
    }
    for {
      machine <- finalState.get
    } yield machine.coins -> machine.candies
  }
}
