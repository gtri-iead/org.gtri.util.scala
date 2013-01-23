package org.gtri.util.scala.statemachine

import org.gtri.util.scala.statemachine.statemachine._
import annotation.tailrec

package object utility {
  def forceDone[I,O,A](state : State[I,O,A]) : Done[I,O,A] = {
    state.fold(
      ifContinue = { q => q.next(EndOfInput) },
      ifSuccess = { q => q },
      ifFailure = { q => q }
    )
  }

  def fold[I,O,A](lhs : Continue[I,O,A], rhs : State[I,O,A]) : State[I,O,A] = {
    rhs.fold(
      ifContinue = { rhs =>
      // Both continue, accumulate some output
        Continue(rhs.next, rhs.output ++ lhs.output)
      },
      ifSuccess = { rhs =>
      // lhs = Continue, rhs = Success, accumulate output return Success
        Success(rhs.value, rhs.output ++ lhs.output, rhs.overflow)
      },
      ifFailure = { rhs =>
      //TODO: handle recover
      // lhs = Continue, rhs = Failure, accumulate output return Failure
        Failure(rhs.output ++ lhs.output, rhs.overflow)
      }
    )
  }


  def run[I,O,A](t: Transitor[I,O,A], input: Seq[I]) : State[I,O,A] = run(Continue(t), input)
  @tailrec def run[I,O,A](current: State[I,O,A], input: Seq[I]) : State[I,O,A] = {
    input match {
      case Nil => current
      case head :: tail =>
        current match {
          case q : Continue[I,O,A]  =>  run(fold(q, q.next(head)), tail)
          case q : Success[I,O,A]   =>  q
          case q : Failure[I,O,A]   =>  q
          //TODO: handle recover
        }
      // Can't tail optimize current.fold
    }
  }

  def doneCompose[A,B,C,D,ZZ](m0: Done[A,B,ZZ], m1: Done[B,C,D]) : Done[A,C,D] = {
    m0.fold(
      ifSuccess = { q0 =>
        m1.fold(
          ifSuccess = { q1 => Success(q1.value,q1.output) },
          ifFailure = { q1 => Failure(m1.output)}
        )
      },
      ifFailure = { q1 => Failure(m1.output)}
    )
  }

  def continueCompose[A,B,C,D,ZZ](m0: Continue[A,B,ZZ], m1: Continue[B,C,D]) : Continue[A,C,D] = {
    Continue(
      new Transitor[A,C,D] {

        override def apply(xs : Seq[A]) = {
          val nextM0 : State[A,B,ZZ] = m0.next(xs)
          val nextM1  : State[B,C,D] = m1.next(nextM0.output)
          compose(nextM0, nextM1)
        }

        def apply(x: A) = {
          val nextM0 : State[A,B,ZZ] = m0.next(x)
          val nextM1  : State[B,C,D] = m1.next(nextM0.output)
          compose(nextM0, nextM1)
        }

        def apply(x: EndOfInput) = {
          val eoiM0 : Done[A,B,ZZ] = m0.next(x)
          val nextM1 : State[B,C,D] = m1.next(eoiM0.output)
          val eoiM1 : Done[B,C,D] = forceDone(nextM1)
          doneCompose(eoiM0, eoiM1)
        }
      },

      m1.output
    )
  }

  def compose[A,B,C,D,ZZ](m0: State[A,B,ZZ], m1: State[B,C,D]) : State[A,C,D] = {
    m0.fold(
      ifContinue = { q0 =>
        m1.fold(
          ifContinue = { q1 => continueCompose(q0, q1) },
          ifSuccess = { q1 => Success(q1.value,q1.output) },
          ifFailure = { q1 => Failure(q1.output) }
        )
      },
      ifSuccess = { q0 =>
        m1.fold(
          ifContinue = { q1 => Failure(q1.output) },
          ifSuccess = { q1 => Success(q1.value,q1.output) },
          ifFailure = { q1 => Failure(q1.output) }
        )
      },
      ifFailure = { q0 =>
        m1.fold(
          ifContinue = { q1 => Failure(q1.output) },
          ifSuccess = { q1 => Failure(q1.output) },
          ifFailure = { q1 => Failure(q1.output) }
        )
      }
    )
  }

}
