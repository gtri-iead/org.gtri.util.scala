/*
    Copyright 2012 Georgia Tech Research Institute

    Author: lance.gatlin@gtri.gatech.edu

    This file is part of org.gtri.util.iteratee library.

    org.gtri.util.iteratee library is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    org.gtri.util.iteratee library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with org.gtri.util.iteratee library. If not, see <http://www.gnu.org/licenses/>.

*/
package org.gtri.util.scala

import org.gtri.util.scala.statemachine.StateMachine._

package object statemachine {
  val STD_CHUNK_SIZE = 256

  type  EOI               =   EndOfInput
  val   EOI               =   EndOfInput

  type  ∅                 =   Unit
  val   ∅                 =   Unit

  type  Enumerator[O]     =   StateMachine[Unit,O,Unit]
  type  Iteratee[I,A]     =   StateMachine[I,Unit,A]
  type  Translator[I,O]   =   StateMachine[I,O,Unit]
  type  Plan[A]           =   StateMachine[Unit,Unit,A]

  implicit class traversableToEnumerator[A](self: Traversable[A]) {
    def toEnumerator = utility.TraversableEnumerator(self, STD_CHUNK_SIZE)
    def toEnumerator(chunkSize : Int) = utility.TraversableEnumerator(self, chunkSize)
  }

  implicit class implicitStateMachineResultOps[I,O,A](self : Result[I,O,A]) {
    def toOption : Option[A] = self.state.toOption
  }

  implicit class implicitStateMachineStateOps[I,O,A](self: State[I,O,A]) {
    def compose[OO,AA](that: State[O,OO,AA]) : State[I,OO,AA] = utility.compose(self, that)
    def toOption : Option[A] = {
      self match {
        case q : State.Success[I,O,A] => Some(q.value)
        case q : State.Failure[I,O,A] => None
        case q : State.Continue[I,O,A] => None
      }
    }
  }

  implicit class implicitStateMachineOps[I,O,A](self: StateMachine[I,O,A]) {
    def compose[OO,AA](that: StateMachine[O,OO,AA]) : StateMachine[I,OO,AA] = utility.compose(self, that)
  }

  implicit class implicitEnumeratorStateOps[O,A](self: State[Unit,O,A]) {
    def step() = utility.step(self)
    def run() = utility.run(self)
  }

  implicit class implicitEnumeratorStateMachineOps[O,A](self: StateMachine[Unit,O,A]) {
    def run() = utility.run(self.s0)
  }

  implicit class implicitIterateeStateOps[I,A](self: State[I,Unit,A]) {
    def flatMap[B](f: A => State[I,Unit,B]) : State[I,Unit,B] = utility.flatMapState(self, f)
    def map[B](f: A => B) : State[I,Unit,B] = utility.mapState(self, f)
  }

  implicit class implicitIterateeStateMachineOps[I,A](self: StateMachine[I,Unit,A]) {
    def flatMap[B](f: A => StateMachine[I,Unit,B]) : StateMachine[I,Unit,B] = utility.flatMapStateMachine(self, f)
    def map[B](f: A => B) : StateMachine[I,Unit,B] = utility.mapStateMachine(self, f)
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
