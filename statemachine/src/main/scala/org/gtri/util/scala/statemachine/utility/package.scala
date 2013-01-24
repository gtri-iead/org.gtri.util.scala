package org.gtri.util.scala.statemachine

import org.gtri.util.scala.statemachine.statemachine._
import annotation.tailrec

package object utility {
  def forceDone[I,O,A](result: Result[I,O,A]) : Done[I,O,A] = {
    result.fold(
      ifContinue = { r => r.state(EndOfInput) },
      ifSuccess = { r => r },
      ifFailure = { r => r }
    )
  }

  def fold[I,O,A](lhs : Continue[I,O,A], rhs : Result[I,O,A]) : Result[I,O,A] = {
    rhs.fold(
      ifContinue = { rhs =>
      // Both continue, accumulate some output
        Continue(rhs.state, rhs.output ++ lhs.output)
      },
      ifSuccess = { rhs =>
      // lhs = Continue, rhs = Success, accumulate output return Success
        Success(rhs.state, rhs.output ++ lhs.output, rhs.overflow)
      },
      ifFailure = { rhs =>
      //TODO: handle recover
      // lhs = Continue, rhs = Failure, accumulate output return Failure
        Failure(rhs.state, rhs.output ++ lhs.output, rhs.overflow)
      }
    )
  }


  def run[I,O,A](state: State.Continue[I,O,A], input: Seq[I]) : Result[I,O,A] = run(Continue(state), input)
  @tailrec def run[I,O,A](current: Result[I,O,A], input: Seq[I]) : Result[I,O,A] = {
    input match {
      case Nil => current
      case head :: tail =>
        current match {
          case r : Continue[I,O,A]  =>  run(fold(r, r.state(head)), tail)
          case r : Success[I,O,A]   =>  r
          case r : Failure[I,O,A]   =>  r
          //TODO: handle recover
        }
      // Can't tail optimize current.fold
    }
  }

  def doneCompose[A,B,C,D,ZZ](r0: Done[A,B,ZZ], r1: Done[B,C,D]) : Done[A,C,D] = {
    r0.fold(
      ifSuccess = { r0 =>
        r1.fold(
          ifSuccess = { r1 => Success(State.Success(r1.state.value),r1.output) },
          ifFailure = { r1 => Failure(new State.Failure,r1.output)}
        )
      },
      ifFailure = { r0 => Failure(new State.Failure,r1.output)}
    )
  }

  def continueCompose[A,B,C,D,ZZ](r0: Continue[A,B,ZZ], r1: Continue[B,C,D]) : Continue[A,C,D] = {
    Continue(
      new State.Continue[A,C,D] {

        override def apply(xs : Seq[A]) = {
          val nextR0 : Result[A,B,ZZ] = r0.state(xs)
          val nextR1  : Result[B,C,D] = r1.state(nextR0.output)
          compose(nextR0, nextR1)
        }

        def apply(x: A) = {
          val nextR0 : Result[A,B,ZZ] = r0.state(x)
          val nextR1  : Result[B,C,D] = r1.state(nextR0.output)
          compose(nextR0, nextR1)
        }

        def apply(x: EndOfInput) = {
          val eoiR0 : Done[A,B,ZZ] = r0.state(x)
          val nextR1 : Result[B,C,D] = r1.state(eoiR0.output)
          val eoiR1 : Done[B,C,D] = forceDone(nextR1)
          doneCompose(eoiR0, eoiR1)
        }
      },

      r1.output
    )
  }

  def compose[A,B,C,D,ZZ](r0 : Result[A,B,ZZ], r1 : Result[B,C,D]) : Result[A,C,D] = {
    r0.fold(
      ifContinue = { r0 =>
        r1.fold(
          ifContinue = { r1 => continueCompose(r0, r1) },
          ifSuccess = { r1 => Success(State.Success(r1.state.value),r1.output) },
          ifFailure = { r1 => Failure(new State.Failure, r1.output) }
        )
      },
      ifSuccess = { r0 =>
        r1.fold(
          ifContinue = { r1 => Failure(new State.Failure, r1.output) },
          ifSuccess = { r1 => Success(State.Success(r1.state.value),r1.output) },
          ifFailure = { r1 => Failure(new State.Failure, r1.output) }
        )
      },
      ifFailure = { r0 =>
        r1.fold(
          ifContinue = { r1 => Failure(new State.Failure, r1.output) },
          ifSuccess = { r1 => Failure(new State.Failure, r1.output) },
          ifFailure = { r1 => Failure(new State.Failure, r1.output) }
        )
      }
    )
  }

  def continueCompose[A,B,C,D,ZZ](s0: State.Continue[A,B,ZZ], s1: State.Continue[B,C,D]) : State.Continue[A,C,D] = {
    new State.Continue[A,C,D] {
  
      override def apply(xs : Seq[A]) = {
        val next_s0 : Result[A,B,ZZ] = s0(xs)
        val next_s1  : Result[B,C,D] = s1(next_s0.output)
        compose(next_s0, next_s1)
      }
  
      def apply(x: A) = {
        val next_s0 : Result[A,B,ZZ] = s0(x)
        val next_s1  : Result[B,C,D] = s1(next_s0.output)
        compose(next_s0, next_s1)
      }
  
      def apply(x: EndOfInput) = {
        val eoi_s0 : Done[A,B,ZZ] = s0(x)
        val next_s1 : Result[B,C,D] = s1(eoi_s0.output)
        val eoi_s1 : Done[B,C,D] = forceDone(next_s1)
        doneCompose(eoi_s0, eoi_s1)
      }
    }
  }
  
  def compose[A,B,C,D,ZZ](s0 : State[A,B,ZZ], s1 : State[B,C,D]) : State[A,C,D] = {
    s0.fold(
      ifContinue = { s0 =>
        s1.fold(
          ifContinue = { s1 => continueCompose(s0, s1) },
          ifSuccess = { s1 => State.Success(s1.value) },
          ifFailure = { s1 => new State.Failure }
        )
      },
      ifSuccess = { s0 =>
        s1.fold(
          ifContinue = { s1 => new State.Failure },
          ifSuccess = { s1 => State.Success(s1.value) },
          ifFailure = { s1 => new State.Failure }
        )
      },
      ifFailure = { s0 =>
        s1.fold(
          ifContinue = { s1 => new State.Failure },
          ifSuccess = { s1 => new State.Failure },
          ifFailure = { s1 => new State.Failure }
        )
      }
    )
  }

}

