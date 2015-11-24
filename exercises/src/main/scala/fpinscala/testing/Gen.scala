package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{Executors,ExecutorService}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

trait Prop {
  def run: TestCases => Result
  def check: Boolean
  def &&(p: Prop): Prop = {
    new Prop {
      override def check: Boolean = this.check && p.check
    }
  }
}

object Prop {
  type TestCases = Int
  type SuccessCount = Int
  type FailedCase = String
  sealed trait Result {
    def isFalsified: Boolean
  }
  case object Passed extends Result {
    override def isFalsified: Boolean = false
  }
  case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
    override def isFalsified: Boolean = true
  }

  def apply(run: (TestCases, RNG) => Result): Prop = new Prop {
    override def run: (TestCases, RNG) => Result = run
    override def check: Boolean = ???
  }

  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
}

object Gen {
  //def unit[A](a: => A): Gen[A] = new Gen[A] {
  //  override def sample: State[RNG, A] = State.unit(a)
  //}

  def unit[A](a: => A): Gen[A] = Gen[A](State.unit(a))

  def boolean: Gen[Boolean] = new Gen[Boolean] {
    override def sample: State[RNG, Boolean] = State(RNG.boolean)
  }

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
    new Gen[List[A]] {
      override def sample: State[RNG, List[A]] = {
        State.sequence(List.fill(n)(g.sample))
      }
    }
  }
  def choose(start: Int, endExclusive: Int): Gen[Int] = {
    new Gen[Int] {
      override def sample: State[RNG, Int] = {
        State(RNG.int).map(i => start + i % (endExclusive - start))
      }
    }
  }
  def apply[A](sample: State[RNG, A]): Gen[A] = new Gen[A] {
    override def sample: State[RNG, State[RNG, A]] = sample
  }

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    Gen.boolean.flatMap(b => if (b) g1 else g2)

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    (g1, g2) match {
      case ((ga1, d1), (ga2, d2)) => {
        Gen(State(RNG.double).flatMap(d => if (d1 < d) ga1.sample else ga2.sample))
      }
    }
  }
}

trait Gen[A] {
  def sample: State[RNG, A]
  def map[B](f: A => B): Gen[B] = {
    Gen(this.sample.map(f))
  }
  def flatMap[B](f: A => Gen[B]): Gen[B] = {
    Gen(sample.flatMap(a => f(a).sample))
  }

  def listOfN(size: Gen[Int]): Gen[List[A]] = {
    size.flatMap(i => Gen.listOfN(i, this))
  }
}

trait SGen[+A] {

}

