package fpinscala.exercises.state

trait RNG:
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.

object RNG:
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG:

    def nextInt: (Int, RNG) =
      val newSeed =
        (seed * 0x5deece66dL + 0xbL) & 0xffffffffffffL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n =
        (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
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
    val (number, newRng) = rng.nextInt
    val nonNegative = if (number < 0) {
      number * -1
    } else {
      number
    }
    (nonNegative, newRng)
  }

  def double(rng: RNG): (Double, RNG) = {
    // old version
    // val (number, newRng) = nonNegativeInt(rng)
    // (number / Int.MaxValue.toDouble, newRng)

    map(nonNegativeInt)(_.toDouble / Int.MaxValue)(rng)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, rng2) = rng.nextInt
    val (d, rng3) = double(rng2)
    ((i, d), rng3)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), rng2) = intDouble(rng)
    ((d, i), rng2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)
    ((d1, d2, d3), rng4)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    // old version
    /*
    def go(current: Int, acc: List[Int], currentRng: RNG): (List[Int], RNG) = {
      if (current <= 0) {
        (acc, currentRng)
      } else {
        val (number, newRng) = currentRng.nextInt
        go(current - 1, number :: acc, newRng)
      }
    }
    go(count, List.empty, rng)
     */
    sequence(List.fill(count)(int))(rng)
  }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng =>
      {
        val (a, rng1) = ra(rng)
        val (b, rng2) = rb(rng1)
        (f(a, b), rng2)
      }
  }

  def sequence[A](rs: List[Rand[A]]): Rand[List[A]] = {
    rs.foldRight(unit(List.empty[A]))((a, acc) => map2(a, acc)(_ :: _))
  }

  def flatMap[A, B](r: Rand[A])(f: A => Rand[B]): Rand[B] = {
    rng =>
      {
        val (value, newRng) = r(rng)
        val result          = f(value)
        result(newRng)
      }
  }

  def mapViaFlatMap[A, B](r: Rand[A])(f: A => B): Rand[B] = {
    flatMap(r)(a => rng => (f(a), rng))
  }

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    flatMap(ra) { a =>
      flatMap(rb) { b => rng => (f(a, b), rng) }
    }
  }

opaque type State[S, +A] = S => (A, S)

object State {

  def sequence[S, A](ls: List[State[S, A]]): State[S, List[A]] = {
    ls.foldRight[State[S, List[A]]](unit(List.empty[A])) { (elem, acc) =>
      elem.flatMap { elm =>
        acc.map(l => elm :: l)
      }
    }
  }

  def unit[S, A](a: A): State[S, A] = apply(s => (a, s))

  extension [S, A](underlying: State[S, A])
    def run(s: S): (A, S) = underlying(s)

    def map[B](f: A => B): State[S, B] = {
      flatMap(a => state => (f(a), state))
    }

    def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = {
      flatMap { a =>
        sb.flatMap { b => newState => (f(a, b), newState) }
      }
    }

    def flatMap[B](f: A => State[S, B]): State[S, B] = {
      state =>
        {
          val (value, newState)   = run(state)
          val (value2, newState2) = f(value).run(newState)
          (value2, newState2)
        }
    }

  def apply[S, A](f: S => (A, S)): State[S, A] = f

  def set[S](s: S): State[S, Unit] = _ => ((), s)

  def get[S]: State[S, S] = s => (s, s)

  def modify[S](f: S => S): State[S, Unit] = s => ((), f(s))
}

enum Input:
  case Coin, Turn

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy {

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    val stateChanges = inputs.map { input =>
      State.modify[Machine] { currentState =>
        (input, currentState) match {

          case (Input.Coin, Machine(true, candies, coins)) if candies > 0 =>
            currentState.copy(locked = false, coins = coins + 1)

          case (Input.Turn, Machine(false, candies, coins)) if candies > 0 =>
            currentState.copy(locked = true, candies = candies - 1)

          case input =>
            currentState
        }
      }
    }

    for {
      simulated    <- State.sequence(stateChanges)
      finalMachine <- State.get
    } yield (finalMachine.coins, finalMachine.candies)
  }
}
