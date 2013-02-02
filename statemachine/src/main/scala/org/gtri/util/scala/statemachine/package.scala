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
package org.gtri.util.scala

import org.gtri.util.scala.statemachine.StateMachine._
import org.gtri.util.scala.statemachine.IssueSeverityCode._

package object statemachine {
  val STD_CHUNK_SIZE = 256

  type  EOI               =   EndOfInput
  val   EOI               =   EndOfInput

  type  ∅                 =   Unit
  val   ∅                 =   Unit

  type  Enumerator[O]     =   StateMachine[Unit,O,Unit]
  type  Enumerable[O,A]   =   StateMachine[Unit,O,A]
  type  Iteratee[I,A]     =   StateMachine[I,Unit,A]
  type  Translator[I,O]   =   StateMachine[I,O,Unit]
  type  Plan[A]           =   StateMachine[Unit,Unit,A]

  implicit class traversableToEnumerator[A](self: Traversable[A]) {
    def toEnumerator = utility.TraversableEnumerator(self, STD_CHUNK_SIZE)
    def toEnumerator(chunkSize : Int) = utility.TraversableEnumerator(self, chunkSize)
  }

  implicit class implicitStateMachineResultOps[I,O,A](self : Transition[I,O,A]) {
    def toOption : Option[A] = self.state.toOption
    def toOption(shouldRecover: State.Halted[I,O,A] => Boolean) : Option[A] = self.state.toOption(shouldRecover)
  }

  implicit class implicitStateMachineStateOps[I,O,A](self: State[I,O,A]) {
    def compose[OO,AA](that: State[O,OO,AA]) : State[I,OO,AA] = utility.composeStates(self, that)
    def toOption : Option[A] = {
      self match {
        case q : State.Success[I,O,A] => Some(q.value)
        case q : State.Halted[I,O,A] => None
        case q : State.Continuation[I,O,A] => None
      }
    }
    def toOption(shouldRecover: State.Halted[I,O,A] => Boolean) : Option[A] = {
      self match {
        case q : State.Success[I,O,A] => Some(q.value)
        case q : State.Halted[I,O,A] =>
          if(shouldRecover(q) && q.optRecover.isDefined) {
            val r = q.optRecover.get()
            r.toOption
          } else {
            None
          }
        case q : State.Continuation[I,O,A] => None
      }
    }
  }

  implicit class implicitStateMachineOps[I,O,A](self: StateMachine[I,O,A]) {
    def compose[OO,AA](that: StateMachine[O,OO,AA]) : StateMachine[I,OO,AA] = utility.composeStateMachines(self, that)
  }

  implicit class implicitEnumerableStateOps[O,A](self: Enumerable.State[O,A]) {
    def step() = Enumerable.impl.stepEnumerableState(self)
    def run(shouldRecover: Enumerable.State.Halted[O,A] => Boolean = IssueRecoverStrategy.NORMAL) = Enumerable.impl.runEnumerableState(self, shouldRecover)
  }

  implicit class implicitEnumerableStateMachineOps[O,A](self: Enumerable[O,A]) {
    def run(shouldRecover: Enumerable.State.Halted[O,A] => Boolean = IssueRecoverStrategy.NORMAL) = Enumerable.impl.runEnumerableState(self.s0, shouldRecover)
  }

  implicit class implicitIterateeStateOps[I,A](self: Iteratee.State[I,A]) {
    def flatMap[B](f: A => Iteratee.State[I,B]) : Iteratee.State[I,B] = Iteratee.impl.flatMapIterateeState(self, f)
    def map[B](f: A => B) : Iteratee.State[I,B] = Iteratee.impl.mapIterateeState(self, f)
  }

  implicit class implicitIterateeOps[I,A](self: Iteratee[I,A]) {
    def flatMap[B](f: A => Iteratee[I,B]) : Iteratee[I,B] = Iteratee.impl.flatMapIteratee(self, f)
    def map[B](f: A => B) : Iteratee[I,B] = Iteratee.impl.mapIteratee(self, f)
  }

  implicit class implicitStateMachineStateTuple2[A,B,C](tuple: (State[Unit,A,_], State[A,B,C])) {
    def state = tuple._1 compose tuple._2
  }

  implicit class implicitStateMachineTuple2[A,B,C](tuple: (StateMachine[Unit,A,_], StateMachine[A,B,C])) extends StateMachine[Unit,B,C] {
    def s0 = tuple._1.s0 compose tuple._2.s0
  }

  implicit class implicitStateMachineStateTuple3[A,B,C,D](tuple: (State[Unit,A,_], State[A,B,_], State[B,C,D])) {
    def state = tuple._1 compose tuple._2 compose tuple._3
  }

  implicit class implicitStateMachineTuple3[A,B,C,D](tuple: (StateMachine[Unit,A,_], StateMachine[A,B,_], StateMachine[B,C,D])) extends StateMachine[Unit,C,D] {
    def s0 = tuple._1.s0 compose tuple._2.s0 compose tuple._3.s0
  }

}
