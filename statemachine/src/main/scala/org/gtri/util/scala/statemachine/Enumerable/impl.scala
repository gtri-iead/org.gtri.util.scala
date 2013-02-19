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
import scala.collection.immutable.Seq

object impl {
  /**
   * Step an Enumerable.Transition (apply an instance of unit to the state)
   * @param t
   * @tparam O
   * @tparam A
   * @return
   */
  def stepEnumerableTransition[O,A](t: Transition[O,A]) : Transition[O,A] = {
    t.state.fold(
      ifContinuation = { q => utility.accumulateTransitions(t,q(())) },
      ifSuccess = { _ => t },
      ifHalted = { _ => t }
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

  /**
   * Step an Enumerable.Transition until it returns Success/Failure with the option to recover from all recoverable Failures
   * @param t0
   * @param _haltedRecoveryStrategy
   * @tparam O
   * @tparam A
   * @return
   */
  def runEnumerableTransition[O,A](t0 : Transition[O,A], _haltedRecoveryStrategy: HaltedRecoveryStrategy[Unit,O,A]) : (Transition[O,A], HaltedRecoveryStrategy[Unit,O,A]) = {
    var done = false
    val accumulator = utility.TransitionAccumulator(t0)
    var haltedRecoveryStrategy = _haltedRecoveryStrategy
    do {
      accumulator.state.fold(
        ifContinuation = { q =>
          // Accumulate transition from applying unit to continuation
          // TODO: handle possibility of infinite recursion here somehow -- look for Progress?
          accumulator.accumulate(q(()))
        },
        ifSuccess = { q =>
          done = true
        },
        ifHalted = { q =>
          val (recoveredTransition,newHaltedRecoveryStrategy) = haltedRecoveryStrategy.recoverAll(q)
          haltedRecoveryStrategy = newHaltedRecoveryStrategy
          accumulator.accumulate(recoveredTransition)
          if(accumulator.state.isContinuation == false) {
            done = true
          }

        }
      )
    } while(done == false)
    (accumulator.toTransition, haltedRecoveryStrategy)
  }

  // Previous functional implementation of runEnumerableTransition
  //  @tailrec def runEnumerableTransition[O,A](t: Transition[O,A], buffer: scala.collection.mutable.Buffer) : Transition[O,A] = {
  //    // Note: can't use state.foldTransitions because of tailrec optimization
  //    t.state match {
  //      case q : State.Continue[O,A] => runEnumerableTransition(foldTransitions(t,q(())))
  //      case q : State.Success[O,A] => t
  //      case q : State.Failure[O,A] => t
  //    }
  //  }

  /**
   * Step an Enumerable.State until it returns Success/Failure with the option to recover from all recoverable Failures
   * @param s
   * @param haltedRecoveryStrategy
   * @tparam O
   * @tparam A
   * @return
   */
  def runEnumerableState[O,A](s: State[O,A], haltedRecoveryStrategy: HaltedRecoveryStrategy[Unit,O,A]) : (Transition[O,A],HaltedRecoveryStrategy[Unit,O,A]) = runEnumerableTransition(Transition(s), haltedRecoveryStrategy)

  /**
   * Step an Enumerable until it returns Success/Failure with the option to recover from all recoverable Failures
   * @param m
   * @param haltedRecoveryStrategy
   * @tparam O
   * @tparam A
   * @return
   */
  def runEnumerable[O,A](m: Enumerable[O,A], haltedRecoveryStrategy: HaltedRecoveryStrategy[Unit,O,A]) : (Transition[O,A],HaltedRecoveryStrategy[Unit,O,A]) = runEnumerableState(m.s0, haltedRecoveryStrategy)


  /**
   * Extract the value of an Enumerable transition (or Halted state) and make this value the output of a different type of StateMachine (such as a Translator or Enumerator)
   * @param t
   * @param ifSuccess
   * @tparam O
   * @tparam A
   * @tparam II
   * @tparam AA
   * @return
   */
  def convertEnumerableTransitionToTransitionOutput[O,A,II,AA](t: Transition[O,Seq[A]], ifSuccess : => StateMachine.State[II,A,AA]) : StateMachine.Transition[II,A,AA] = {
    t.state.fold(
      ifContinuation = { q =>
        // Run the enumerable to extract the value or get a halted state
        val (t0, _) = Enumerable.impl.runEnumerableTransition(t, HaltedRecoveryStrategy.STRICT[Unit,O,Seq[A]])
        // Run enumerable only returns Success/Halted but because it returns Transition this isn't captured by type system - so there can't be infinite recursion here
        convertEnumerableTransitionToTransitionOutput(t0, ifSuccess)
      },
      ifSuccess = { q =>
        StateMachine.Transition(
          state = ifSuccess,
          output = q.value,
          metadata = t.metadata
        )
      },
      ifHalted = { q =>
        val optRecover : Option[() => StateMachine.Transition[II,A,AA]] = q.optRecover map { recover => () =>
          convertEnumerableTransitionToTransitionOutput(recover(),ifSuccess)
        }
        StateMachine.Halt(
          issues = q.issues,
          optRecover = optRecover,
          metadata = t.metadata
        )
      }
    )
  }


  /**
   * Extract the value of an Enumerable transition (or Halted state) and make this value the value of a different type of StateMachine (such as an Iteratee)
   * @param t
   * @tparam O
   * @tparam A
   * @tparam II
   * @tparam OO
   * @tparam AA
   * @return
   */
  def convertEnumerableTransitionToTransitionValue[O,A,II,OO,AA](t: Transition[O,A]) : StateMachine.Transition[II,OO,A] = {
    t.state.fold(
      ifContinuation = { q =>
        // Run the enumerable to extract the value or get a halted state
        val (t0, _) = Enumerable.impl.runEnumerableTransition(t, HaltedRecoveryStrategy.STRICT[Unit,O,A])
        // Run enumerable only returns Success/Halted but because it returns Transition this isn't captured by type system - so there can't be infinite recursion here
        convertEnumerableTransitionToTransitionValue(t0)
      },
      ifSuccess = { q =>
        StateMachine.Succeed(
          value = q.value,
          metadata = t.metadata
        )
      },
      ifHalted = { q =>
        val optRecover : Option[() => StateMachine.Transition[II,OO,A]] = q.optRecover map { recover => () =>
          convertEnumerableTransitionToTransitionValue(recover())
        }
        StateMachine.Halt(
          issues = q.issues,
          optRecover = optRecover,
          metadata = t.metadata
        )
      }
    )
  }

  /**
   * Map an Enumerable Transition in terms of bind/flatMap
   * @param s
   * @param f
   * @tparam O
   * @tparam A
   * @tparam OO
   * @tparam B
   * @return
   */
  def mapEnumerableTransition[O,A,OO,B](s : Transition[O,A], f: A => B) : Transition[OO,B] = {
    flatMapEnumerableTransition[O,A,OO,B](s, { (a : A) => bindEnumerableTransition(f(a)) } )
  }

  /**
   * Bind a value to an Enumerable Transition (Success)
   * @param value
   * @tparam O
   * @tparam A
   * @return
   */
  def bindEnumerableTransition[O,A](value : A) : Transition[O,A] = Succeed(value)

  /**
   * Flat map an Enumerable Transition such that if the Transition is a continuation it is run until it is Success/Halt,
   * If it is Success the value is re-packaged with metadata and returned. If it is Halted, a new Halted Transition is
   * returned that recursively flatMaps the recover result (should one exist).
   * @param t
   * @param f
   * @tparam O
   * @tparam A
   * @tparam OO
   * @tparam B
   * @return
   */
  def flatMapEnumerableTransition[O,A,OO,B](t : Transition[O,A], f: A => Transition[OO,B]) : Transition[OO,B] = {
    t.state.fold(
      ifContinuation = { q =>
      // Run the enumerable to extract the value or get a halted state
        val (t0, _) = Enumerable.impl.runEnumerableTransition(t, HaltedRecoveryStrategy.STRICT[Unit,O,A])
        // Run enumerable only returns Success/Halted but because it returns Transition this isn't captured by type system - so there can't be infinite recursion here
        flatMapEnumerableTransition(t0, f)
      },
      ifSuccess = { q =>
        val t1 = f(q.value)
        t1.copy(metadata = t1.metadata ++ t.metadata)
      },
      ifHalted = { q =>
        val optRecover : Option[() => Transition[OO,B]] = q.optRecover map { recover => { () =>
          flatMapEnumerableTransition(recover(), f)
        }}
        Halt(
          issues = q.issues,
          optRecover = optRecover,
          metadata = t.metadata
        )
      }
    )
  }

  /**
   * Invert a collection of Transitions into a Transition of a collection.
   * @param xs
   * @param xt
   * @tparam O
   * @tparam A
   * @return
   */
  def invertEnumerableTransitionTraversable[O,A](xs: Traversable[Transition[O,A]], xt: List[A] = Nil) : Transition[O,Seq[A]] = {
    // TODO: optimize me?
    if(xs.isEmpty) {
      Succeed(xt.reverse)
    } else {
      flatMapEnumerableTransition[O,A,O,Seq[A]](xs.head, { x => invertEnumerableTransitionTraversable(xs.tail, x :: xt) })
    }
  }
}
