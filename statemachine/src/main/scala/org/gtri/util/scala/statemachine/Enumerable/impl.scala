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
package org.gtri.util.scala.statemachine.Enumerable

import org.gtri.util.scala.statemachine._
import org.gtri.util.scala.statemachine.utility
import org.gtri.util.scala.statemachine.IssueSeverityCode._

object impl {
  private[impl] def stepEnumerableTransition[O,A](current: Transition[O,A]) : Transition[O,A] = {
    current.state.fold(
      ifContinuation = { q => utility.foldTransition(current,q(())) },
      ifSuccess = { _ => current },
      ifHalted = { _ => current }
    )
  }

  /**
   * Step an Enumerable.State (apply an instance of unit to the state)
   * @param s
   * @tparam O
   * @tparam A
   * @return
   */
  def stepEnumerableState[O,A](s: State[O,A]) : Transition[O,A] = {
    s.fold(
      ifContinuation = { q => stepEnumerableTransition(Transition(q)) },
      ifSuccess = { q => Transition(q) },
      ifHalted = { q => Transition(q)}
    )
  }

  /**
   * Step an Enumerable (apply an instance of unit to the s0 state)
   * @param m
   * @tparam O
   * @tparam A
   * @return
   */
  def stepEnumerable[O,A](m : Enumerable[O,A]) : Transition[O,A] = stepEnumerableState(m.s0)

  private[impl] def runEnumerableTransition[O,A](r0 : Transition[O,A], shouldRecover: State.Halted[O,A] => Boolean) : Transition[O,A] = {
    var done = false
    val r = utility.TransitionAccumulator(r0)
    do {
      r.state.fold(
        ifContinuation = { q => r.append(q(())) },
        ifSuccess = { q => done = true },
        ifHalted = { q =>
          if(shouldRecover(q) && q.optRecover.isDefined) {
            r.append(q.optRecover.get.apply())
          } else {
            done = true
          }
        }
      )
    } while(done == false)
    r.toTransition
  }

  // Previous functional implementation of runEnumerableTransition
  //  @tailrec def runEnumerableTransition[O,A](current: Transition[O,A], buffer: scala.collection.mutable.Buffer) : Transition[O,A] = {
  //    // Note: can't use state.foldTransitions because of tailrec optimization
  //    current.state match {
  //      case q : State.Continue[O,A] => runEnumerableTransition(foldTransitions(current,q(())))
  //      case q : State.Success[O,A] => current
  //      case q : State.Failure[O,A] => current
  //    }
  //  }

  /**
   * Step an Enumerable.State until it returns Success/Failure with the option to recover from all recoverable Failures
   * @param s
   * @param shouldRecover TRUE to recover from Halted state
   * @tparam O
   * @tparam A
   * @return
   */
  def runEnumerableState[O,A](s: State[O,A], shouldRecover: State.Halted[O,A] => Boolean) : Transition[O,A] = runEnumerableTransition(Transition(s), shouldRecover)

  /**
   * Step an Enumerable until it returns Success/Failure with the option to recover from all recoverable Failures
   * @param m
   * @param shouldRecover TRUE to recover from Halted state
   * @tparam O
   * @tparam A
   * @return
   */
  def runEnumerable[O,A](m: Enumerable[O,A], shouldRecover: State.Halted[O,A] => Boolean) : Transition[O,A] = runEnumerableState(m.s0, shouldRecover)

}
