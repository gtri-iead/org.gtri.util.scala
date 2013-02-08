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

import org.gtri.util.scala.statemachine.StateMachine._
//import annotation.tailrec
import collection.mutable
import IssueSeverityCode._
import scala.collection.immutable.Seq

package object utility {

  def recoverTransition[I,O,A](t: Transition[I,O,A], shouldRecover: State.Halted[I,O,A] => Boolean, maxAttempts : Int) : Transition[I,O,A] = {
    var done = false
    val r = TransitionAccumulator(t)
    var n = maxAttempts
    do {
      r.state.fold(
        ifContinuation = { q => done = true },
        ifSuccess = { q => done = true },
        ifHalted = { q =>
          if(q.optRecover.isDefined && shouldRecover(q)) {
            if(n > 0) {
              // Save any overflow
              val overflow = r.overflow
              // Add Halted issues as metadata
              r.metadata ++= q.issues
              // Clear overflow -- Not needed since accumulate below will replace it
              // r.overflow = Nil
              // Recover and accumulate
              r.accumulate(q.optRecover.get.apply())
              // Apply overflow to recovered state and accumulate
              r.accumulate(applySeqToState(r.state, overflow))
              // Decrement maxAttempts
              n = n - 1
            } else {
              // Reached max recover attempts
              r.state = State.Halted(
                issues = Issue.fatal("Max recover attempts reached") :: Nil
              )
              // Dump output and overflow
              r.output.clear
              r.overflow = Nil
              // Return accumulated metadata
              done = true
            }
          } else {
            done = true
          }
        }
      )
    } while(done == false)
    r.toTransition
  }

//  /**
//   * Force a state to be either Success or Failure by applying EndOfInput when required
//   * @param state
//   * @tparam I
//   * @tparam O
//   * @tparam A
//   * @return
//   */
//  def forceDoneState[I,O,A](state: State[I,O,A]) : Transition[I,O,A] = {
//    state.fold(
//      ifContinuation = { s => s.apply(EndOfInput) },
//      ifSuccess = { s => Transition(s) },
//      ifHalted = { s => Transition(s) }
//    )
//  }

  /**
   * Force a the State of a Transition to be either Success or Failure by applying EndOfInput when required and
   * accumulate the Transitions.
   * @param t
   * @tparam I
   * @tparam O
   * @tparam A
   * @return
   */
  def forceDoneTransition[I,O,A](t: Transition[I,O,A]) : Transition[I,O,A] = {
    t.state.fold(
      ifContinuation = { q => accumulateTransitions(t, q.apply(EndOfInput)) },
      ifSuccess = { q => t },
      ifHalted = { q => t }
    )
  }

  /**
   * Accumulate two Transitions such that second.state and second.overflow replace first.state and first.overflow AND
   * second.output/metadata are appended to first.output/metadata
   * @param first
   * @param second
   * @tparam I
   * @tparam O
   * @tparam A
   * @return
   */
  def accumulateTransitions[I,O,A](first : Transition[I,O,A], second : Transition[I,O,A]) : Transition[I,O,A] = {
    Transition(second.state, second.output ++ first.output, second.overflow, second.metadata ++ first.metadata)
  }


//  @tailrec def applySeqToState[I,O,A](current: Result[I,O,A], input: Seq[I]) : Result[I,O,A] = {
//    // Note: can't use state.accumulateTransitions because of tailrec optimization
//    current.state match {
//      case q : State.Continue[I,O,A] =>
//        input match {
//          case Nil => current
//          case head :: tail => applySeqToState(accumulateTransitions(current,q(head)),tail)
//        }
//      case q : State.Success[I,O,A] => current.copy(overflow = input ++ current.overflow)
//      case q : State.Failure[I,O,A] => current.copy(overflow = input ++ current.overflow)
//    }
//  }
//

  /**
   * A case class for an accumulator of Transitions.
   * @param state
   * @param output
   * @param overflow
   * @param metadata
   * @tparam I
   * @tparam O
   * @tparam A
   */
  case class TransitionAccumulator[I,O,A](var state : State[I,O,A], output : mutable.Buffer[O], var overflow : Seq[I], metadata : mutable.Buffer[Any]) {
    def accumulate(r: Transition[I,O,A]) = {
      state = r.state
      output ++= r.output
      overflow = r.overflow
      metadata ++= r.metadata
    }
    def toTransition : Transition[I,O,A] = Transition(state, output.toVector, overflow, metadata.toVector)
  }
  object TransitionAccumulator {
    def apply[I,O,A](r : Transition[I,O,A]) : TransitionAccumulator[I,O,A] = TransitionAccumulator(r.state,r.output.toBuffer,r.overflow,r.metadata.toBuffer)
  }

  def applyInputToState[I,O,A](s: State[I,O,A], input: Input[I]) : Transition[I,O,A] = {
    input match {
      case chunk : Chunk[I] => applySeqToState(s,chunk.xs)
      case _ : EndOfInput => forceDoneTransition(Transition(s))
    }
  }

  /**
   * Apply xs to a state with the option to recover from Halted states and accumulate all transitions.
   * @param s
   * @param xs
   * @tparam I
   * @tparam O
   * @tparam A
   * @return
   */
  def applySeqToState[I,O,A](s: State[I,O,A], xs: Seq[I]) : Transition[I,O,A] = {
    if(xs.nonEmpty) {
      var done = false
      val r = TransitionAccumulator(Transition(s))
      var i = xs
      do {
        r.state.fold(
          ifContinuation = { q =>
            // If xs isn't empty
            if(i.nonEmpty) {
              // Apply the xs head to the continuation and accumulate the transition
              r.accumulate(q(i.head))
              i = i.tail
            } else {
              // Input is exhausted so we are done
              done = true
            }
          },
          ifSuccess = { q =>
            // Append remaining xs to overflow
            r.overflow ++= i
            done = true
          },
          ifHalted = { q =>
            // Append remaining xs to overflow
            r.overflow ++= i
            done = true
          }
        )
      } while(done == false)
      r.toTransition
    } else {
      // Empty xs build a dummy Transition
      Transition(s)
    }
  }

  private[utility] def composeTransition[A,B,C,D,ZZ](t0: Transition[A,B,ZZ], t1: Transition[B,C,D]) : Transition[A,C,D] = {
    Transition(
      state     = composeStates(t0.state, t1.state),
      output    = t1.output,
      metadata  = t1.metadata ++ t0.metadata
    )
  }

  private[utility] def composeTransitionAndStateContinue[A,B,C,D,ZZ](t0: Transition[A,B,ZZ], s1: State.Continuation[B,C,D]) : Transition[A,C,D] = {
    val t1 = s1(t0.output)
    //If r0 Success force r1 done and composeTransition otherwise just composeTransition
    t0.state.fold(
      ifSuccess = { q =>
      // If r0 is Success, feed an EOI to r1 since r1 will not receive further xs
        composeTransition(t0, forceDoneTransition(t1))
      },
      ifContinuation = { q => composeTransition(t0,t1) },
      ifHalted = { q => composeTransition(t0,t1)}
    )
  }

  // This is a case class instead of anonymous class for better debug messages
  private[utility] case class CompositeStateContinue[A,B,C,D,ZZ](s0: State.Continuation[A,B,ZZ], s1: State.Continuation[B,C,D]) extends State.Continuation[A,C,D] {
      override def apply(xs : Seq[A]) : Transition[A,C,D] = composeTransitionAndStateContinue(s0(xs),s1)

      def apply(x: A) = composeTransitionAndStateContinue(s0(x),s1)

      def apply(x: EndOfInput) = {
        val eoi_t0 : Transition[A,B,ZZ] = s0(x)
        val t1 : Transition[B,C,D] = s1(eoi_t0.output)
        val eoi_t1 : Transition[B,C,D] = forceDoneTransition(t1)
        composeTransition(eoi_t0, eoi_t1)
      }
  }

  /**
   * Compose two states into a composite state such that the output of s0 feeds the xs of s1
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
      ifContinuation = { s0 =>
        s1.fold(
          ifContinuation = { s1 => CompositeStateContinue(s0, s1) }, // OK
          ifSuccess = { s1 => State.Success(s1.value) }, // OK
          ifHalted = { s1 =>
          // TODO: test to verify this
            val optRecover : Option[() => Transition[A,C,D]] = s1.optRecover map { recover => () => composeTransition(Transition(s0),recover())}
            State.Halted(
              issues = s1.issues,
              optRecover = optRecover
            )
          }
        )
      },
      ifSuccess = { s0 =>
        s1.fold(
          ifContinuation = { s1 =>
          // Note: If Success/Continuation occurs, EOI should be applied to Continuation, however we can't do that here since it would discard the Transition
          // Note: can only reach here if user passes in Success/Continuation - recursion from ComposedState above handles EOI correctly
          // TODO: test to verify this
            val optRecover : Option[() => Transition[A,C,D]] = Some(() => composeTransition(Transition(s0),s1.apply(EndOfInput)))
            val msg = "No more xs available from s0, EOI should have been applied to s1 prior to composing s0 and s1"
            State.Halted(
              issues = Seq(Issue(WARN,msg)),
              optRecover = optRecover
            )
          },
          ifSuccess = { s1 => State.Success(s1.value) }, // OK
          ifHalted = { s1 =>
          // TODO: test to verify this
            val optRecover : Option[() => Transition[A,C,D]] = s1.optRecover map { recover => () => composeTransition(Transition(s0),recover())}
            State.Halted(
              issues = s1.issues,
              optRecover = optRecover
          )}
        )
      },
      ifHalted = { s0 =>
        s1.fold(
          ifContinuation = { s1 =>
          // TODO: test to verify this
            val optRecover : Option[() => Transition[A,C,D]] = s0.optRecover map { recover => () => composeTransitionAndStateContinue(recover(), s1) }
            State.Halted(
              issues = s0.issues,
              optRecover = optRecover
            )
          },
          ifSuccess = { s1 =>
          // TODO: test to verify this
            val optRecover : Option[() => Transition[A,C,D]] = s0.optRecover map { recover =>
                { () =>
                  val t0 = recover()
                  Transition(
                    state = State.Success(s1.value),
                    overflow = t0.overflow,
                    metadata = t0.metadata
                  )
                }
              }
            State.Halted(
              issues = s0.issues,
              optRecover = optRecover
            )
          },
          ifHalted = { s1 =>
          // TODO: test to verify this
            val optRecover : Option[() => Transition[A,C,D]] = for(s0_recover <- s0.optRecover;s1_recover <- s1.optRecover) yield { () => composeTransition(s0_recover(), s1_recover()) }
            State.Halted(
              issues = s1.issues ++ s0.issues,
              optRecover = optRecover
            )}
        )
      }
    )
  }

  // This is a case class instead of anonymous class for better debug messages
  private[utility] case class CompositeStateMachine[A,B,C,D,ZZ](m0 : StateMachine[A,B,ZZ], m1 : StateMachine[B,C,D]) extends StateMachine[A,C,D] {
    def s0 = composeStates(m0.s0,m1.s0)
  }

  /**
   * Compose two state machines into a composite state machine such that the output of s0 feeds the xs of s1
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

