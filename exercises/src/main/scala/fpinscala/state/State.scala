package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def boolean: Rand[Boolean] = {
    ???
  }

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, rng2) = rng.nextInt
    if (n >= 0) (n, rng2)
    else (-n - 1, rng2)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = RNG.nonNegativeInt(rng)
    ((i.toDouble / Int.MaxValue + 1), r)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)
    ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def go(n: Int, acc: List[Int], rng: RNG): (List[Int], RNG) = {
      if (n == 0) (acc, rng)
      else {
        val (i, r) = rng.nextInt
        go(n - 1, i :: acc, r)
      }
    }
    go(count, Nil, rng)
  }

  def doubleViaMap: Rand[Double] = {
    map(nonNegativeInt)(i => i.toDouble / (Int.MaxValue + 1))
  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (a, r1) = ra(rng)
      val (b, r2) = rb(r1)
      (f(a, b), r2)
    }
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    //fs match {
    //  case r :: rs => map2(r, sequence(rs))(_ :: _)
    //  case Nil => unit(Nil)
    //}
    fs.foldRight[Rand[List[A]]](unit(List()))((a, b) => map2(a, b)(_ :: _))
  }

  def intsViaSequence(count: Int): Rand[List[Int]] = {
    sequence(List.fill(count)(int))
  }

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng => {
      val (a, r1) = f(rng)
      g(a)(r1)
    }
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt){ i =>
      val mod = i % n
      if (i + (n - 1) - mod >= 0)
        unit(mod)
      else
        nonNegativeLessThan(n)
    }
  }

  def mapViaFlatMap[A, B](r: Rand[A])(f: A => B): Rand[B] = {
    flatMap(r){ a =>
      unit(f(a))
    }
  }

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C) = {
    flatMap(ra){ a =>
      map(rb)(b => f(a, b))
    }
  }
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = {
    State(s => {
      val (a, s2) = run(s)
      (f(a), s2)
    })
  }
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] = {
    State(s => {
      val (a, s2) = run(s)
      val (b, s3) = sb.run(s2)
      (f(a, b), s3)
    })
  }
  def flatMap[B](f: A => State[S, B]): State[S, B] = {
    State(s => {
      val (a, s2) = run(s)
      f(a).run(s2)
    })
  }
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    val stateChanges: List[State[Machine, Unit]] = inputs.map(i => modify[Machine](m => m))
    for {
      _ <- sequence(stateChanges)
      m <- get
    } yield {(m.coins, m.candies)}
  }

  def unit[S, A](a: A): State[S, A] = {
    State(s => (a, s))
  }
  // g:(State[S, A], State[S, List[A]) => State[S, List[A]]
  def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] = {
    sas.foldRight[State[S, List[A]]](unit(List()))((sa, sas) => sa.map2(sas)(_ :: _))
  }
  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  def get[S]: State[S, S] = State(s => (s, s))
  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
}
