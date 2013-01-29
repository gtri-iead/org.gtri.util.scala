/*
    Copyright 2012 Georgia Tech Research Institute

    Author: lance.gatlin@gtri.gatech.edu

    This file is part of org.gtri.util.scala.statemachine library.

    org.gtri.util.scala.statemachine library is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    org.gtri.util.scala.statemachine library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with org.gtri.util.scala.statemachine library. If not, see <http://www.gnu.org/licenses/>.

*/
package org.gtri.util.scala.statemachine

import org.gtri.util.scala.statemachine._
import org.gtri.util.scala.statemachine.StateMachine._
import annotation.tailrec
import collection.mutable

package object utility {

  /**
   * Force a state to be either Success or Failure by applying EndOfInput when required
   * @param state
   * @tparam I
   * @tparam O
   * @tparam A
   * @return
   */
  def forceDoneState[I,O,A](state: State[I,O,A]) : Result[I,O,A] = {
    state.fold(
      ifContinue = { s => s.apply(EndOfInput) },
      ifSuccess = { s => Result(s) },
      ifFailure = { s => Result(s) }
    )
  }

  /**
   * Force a result to be either Success or Failure by applying EndOfInput when required
   * @param r
   * @tparam I
   * @tparam O
   * @tparam A
   * @return
   */
  def forceDoneResult[I,O,A](r: Result[I,O,A]) : Result[I,O,A] = {
    r.state.fold(
      ifContinue = { s => foldResults(r, s.apply(EndOfInput)) },
      ifSuccess = { s => r },
      ifFailure = { s => r }
    )
  }

  /**
   * Fold two results such that rhs.state and rhs.overflow replace lhs.state and lhs.overflow and output and metadata
   * are accumulated
   * @param lhs
   * @param rhs
   * @tparam I
   * @tparam O
   * @tparam A
   * @return
   */
  def foldResults[I,O,A](lhs : Result[I,O,A], rhs : Result[I,O,A]) : Result[I,O,A] = {
    Result(rhs.state, lhs.output ++ rhs.output, rhs.overflow, lhs.metadata ++ rhs.metadata)
  }


//  @tailrec def applyInputToState[I,O,A](current: Result[I,O,A], input: Seq[I]) : Result[I,O,A] = {
//    // Note: can't use state.foldResults because of tailrec optimization
//    current.state match {
//      case q : State.Continue[I,O,A] =>
//        input match {
//          case Nil => current
//          case head :: tail => applyInputToState(foldResults(current,q(head)),tail)
//        }
//      case q : State.Success[I,O,A] => current.copy(overflow = input ++ current.overflow)
//      case q : State.Failure[I,O,A] => current.copy(overflow = input ++ current.overflow)
//    }
//  }
//

  case class AccResult[I,O,A](var state : State[I,O,A], output : mutable.Buffer[O], var overflow : Seq[I], metadata : mutable.Buffer[Any]) {
    def append(r: Result[I,O,A]) = {
      state = r.state
      output ++= r.output
      overflow = r.overflow
      metadata ++= r.metadata
    }
    def toResult : Result[I,O,A] = Result(state, output.toSeq, overflow, metadata.toSeq)
  }
  object AccResult {
    def apply[I,O,A](r : Result[I,O,A]) : AccResult[I,O,A] = AccResult(r.state,r.output.toBuffer,r.overflow,r.metadata.toBuffer)
  }

  /**
   * Apply input to a state with the option to recover from all recoverable Failures
   * @param s
   * @param input
   * @param recover TRUE to recover from all recoverable Failures
   * @tparam I
   * @tparam O
   * @tparam A
   * @return
   */
  def applyInputToState[I,O,A](s: State[I,O,A], input: Seq[I], recover : Boolean) : Result[I,O,A] = {
    var done = false
    val r = AccResult(Result(s))
    var i = input
    do {
      r.state.fold(
        ifContinue = { q =>
          if(i.nonEmpty) {
            r.append(q(i.head))
            i = i.tail
          } else {
            done = true
          }
        },
        ifSuccess = { q =>
          r.overflow = i ++ r.overflow
          done = true
        },
        ifFailure = { q =>
          if(recover && q.optRecover.isDefined) {
            r.append(q.optRecover.get.apply())
            i = i ++ r.overflow
            r.overflow = Seq.empty
          } else {
            r.overflow = i ++ r.overflow
            done = true
          }
        }
      )
    } while(done == false)
    r.toResult
  }

  private[utility] def composeResults[A,B,C,D,ZZ](r0: Result[A,B,ZZ], r1: Result[B,C,D]) : Result[A,C,D] = {
    Result(
      state     = composeStates(r0.state, r1.state),
      output    = r1.output,
      metadata  = r0.metadata ++ r1.metadata
    )
  }

  private[utility] def composeResultAndStateContinue[A,B,C,D,ZZ](r0: Result[A,B,ZZ], s1: State.Continue[B,C,D]) : Result[A,C,D] = {
    val r1 = s1(r0.output)
    //If r0 Success force r1 done and composeResults otherwise just composeResults
    r0.state.fold(
      ifSuccess = { q =>
      // If r0 is Success, feed an EOI to r1 since r1 will not receive further input
        composeResults(r0, forceDoneResult(r1))
      },
      ifContinue = { q => composeResults(r0,r1) },
      ifFailure = { q => composeResults(r0,r1)}
    )
  }

  private[utility] case class CompositeStateContinue[A,B,C,D,ZZ](s0: State.Continue[A,B,ZZ], s1: State.Continue[B,C,D]) extends State.Continue[A,C,D] {
      override def apply(xs : Seq[A]) : Result[A,C,D] = composeResultAndStateContinue(s0(xs),s1)

      def apply(x: A) = composeResultAndStateContinue(s0(x),s1)

      def apply(x: EndOfInput) = {
        val eoi_r0 : Result[A,B,ZZ] = s0(x)
        val r1 : Result[B,C,D] = s1(eoi_r0.output)
        val eoi_r1 : Result[B,C,D] = forceDoneResult(r1)
        composeResults(eoi_r0, eoi_r1)
      }
  }

  /**
   * Compose two states into a composite state such that the output of s0 feeds the input of s1
   * @param s0
   * @param s1
   * @tparam A
   * @tparam B
   * @tparam C
   * @tparam D
   * @tparam ZZ
   * @return
   */
  def composeStates[A,B,C,D,ZZ](s0 : State[A,B,ZZ], s1 : State[B,C,D]) : State[A,C,D] = {
    s0.fold(
      ifContinue = { s0 =>
        s1.fold(
          ifContinue = { s1 => CompositeStateContinue(s0, s1) }, // OK
          ifSuccess = { s1 => State.Success(s1.value) }, // OK
          ifFailure = { s1 => State.Failure(
            s1.optRecover map { recover => () => composeResults(Result(s0),recover())}
          )} // TODO: test to verify this
        )
      },
      ifSuccess = { s0 =>
        s1.fold(
          ifContinue = { s1 => State.Failure() }, // OK
          ifSuccess = { s1 => State.Success(s1.value) }, // OK
          ifFailure = { s1 => State.Failure(
            s1.optRecover map { recover => () => composeResults(Result(s0),recover())}
          )} // TODO: test to verify this
        )
      },
      ifFailure = { s0 =>
        s1.fold(
          ifContinue = { s1 =>
            State.Failure(
              optRecover = s0.optRecover map { recover =>
                { () => composeResultAndStateContinue(recover(), s1) }
              }
            )
          }, // TODO: test to verify this
          ifSuccess = { s1 =>
            State.Failure(
              optRecover = s0.optRecover map { recover =>
                { () =>
                  val result = recover()
                  Result(
                    state = State.Success(s1.value),
                    overflow = result.overflow,
                    metadata = result.metadata
                  )
                }
              }
            )
          }, // TODO: test to verify this
          ifFailure = { s1 =>
            State.Failure(
              for(s0_recover <- s0.optRecover;s1_recover <- s1.optRecover) yield { () => composeResults(s0_recover(), s1_recover()) }
            )} // TODO: test to verify this
        )
      }
    )
  }

  private[utility] case class CompositeStateMachine[A,B,C,D,ZZ](m0 : StateMachine[A,B,ZZ], m1 : StateMachine[B,C,D]) extends StateMachine[A,C,D] {
    def s0 = composeStates(m0.s0,m1.s0)
  }

  /**
   * Compose two state machines into a composite state machine such that the output of s0 feeds the input of s1
   * @param m0
   * @param m1
   * @tparam A
   * @tparam B
   * @tparam C
   * @tparam D
   * @tparam ZZ
   * @return
   */
  def composeStateMachines[A,B,C,D,ZZ](m0 : StateMachine[A,B,ZZ], m1 : StateMachine[B,C,D]) : StateMachine[A,C,D]
    = CompositeStateMachine(m0,m1)
}

