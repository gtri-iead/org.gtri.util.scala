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
import scala.collection.immutable.Seq

package object statemachine {
  type  EOI               =   EndOfInput
  val   EOI               =   EndOfInput

//  type  ∅                 =   Unit
//  val   ∅                 =   Unit

  type  Enumerator[O]     =   StateMachine[Unit,O,Unit]
  type  Enumerable[O,A]   =   StateMachine[Unit,O,A]
  type  Iteratee[I,A]     =   StateMachine[I,Unit,A]
  type  Translator[I,O]   =   StateMachine[I,O,Unit]
  type  Plan[A]           =   StateMachine[Unit,Unit,A]

  type  Parser[I,A]       =   I => Parser.Transition[A]

  implicit class traversableToEnumerator[A](val self: Traversable[A]) extends AnyVal {
    def toEnumerator = utility.TraversableEnumerator(self, Enumerator.STD_CHUNK_SIZE)
    def toEnumerator(chunkSize : Int) = utility.TraversableEnumerator(self, chunkSize)
  }

//  implicit class implicitStateMachineTransitionOps[I,O,A](self : Transition[I,O,A]) {
//    def isSuccess = self.state.isSuccess
//    def isContinuation = self.state.isContinuation
//    def isHalted = self.state.isHalted
//    def isRecoverable = self.state.isRecoverable
//    def toOption : Option[A] = self.state.toOption
//    def toOption(haltedRecoverStrategy : HaltedRecoveryStrategy[I,O,A]) : Option[A] = self.state.toOption(haltedRecoverStrategy)
//  }

  implicit class implicitStateOps[I,O,A](val self: State[I,O,A]) extends AnyVal {
    def isSuccess = self.fold(ifContinuation = { _ => false }, ifSuccess = { _ => true }, ifHalted = { _ => false})
    def isContinuation = self.fold(ifContinuation = { _ => true }, ifSuccess = { _ => false }, ifHalted = { _ => false })
    def isHalted = self.fold(ifContinuation = { _ => false }, ifSuccess = { _ => false }, ifHalted = { _ => true })
    def isRecoverable = self.fold(ifContinuation = { _ => false }, ifSuccess = { _ => false }, ifHalted = { q => q.optRecover.isDefined })

    def compose[OO,AA](that: State[O,OO,AA]) : State[I,OO,AA] = utility.composeStates(self, that)

    def toOption : Option[A] = {
      self match {
        case q : State.Success[I,O,A] => Some(q.value)
        case q : State.Halted[I,O,A] => None
        case q : State.Continuation[I,O,A] => None
      }
    }
    def toOption(haltedRecoverStrategy : HaltedRecoveryStrategy[I,O,A]) : Option[A] = {
      self match {
        case q : State.Success[I,O,A] => Some(q.value)
        case q : State.Halted[I,O,A] =>
          val (r,_) = haltedRecoverStrategy.recoverAll(q)
          r.state.toOption
        case q : State.Continuation[I,O,A] => None
      }
    }
  }

  implicit class implicitContinuationStateOps[I,O,A](val self : State.Continuation[I,O,A]) extends AnyVal {
    def apply(i: Input[I]) = utility.applyInputToState(i, self)
  }

//  implicit class implicitHaltedStateOps[I,O,A](self : State.Halted[I,O,A]) {
//    def recover : Transition[I,O,A] = self.optRecover map { recover => recover() } getOrElse Transition(self)
//    def recoverAll : Transition[I,O,A] = self.optRecover map { recover => utility.recoverAll(recover(),)}
//  }

  implicit class implicitStateMachineFromState[I,O,A](self : State[I,O,A]) extends StateMachine[I,O,A] {
    def s0 = self
  }

  implicit class implicitStateMachineOps[I,O,A](val self: StateMachine[I,O,A]) extends AnyVal {
    def compose[OO,AA](that: StateMachine[O,OO,AA]) : StateMachine[I,OO,AA] = utility.composeStateMachines(self, that)
  }

  implicit class implicitEnumerableStateOps[O,A](val self: Enumerable.State[O,A]) extends AnyVal {
    def step() = Enumerable.impl.stepEnumerableState(self)
    def run() = Enumerable.impl.runEnumerableState(self, HaltedRecoveryStrategy.STRICT[Unit,O,A])._1
    def run(haltedRecoveryStrategy : HaltedRecoveryStrategy[Unit,O,A]) = Enumerable.impl.runEnumerableState(self, haltedRecoveryStrategy)
  }

  implicit class implicitEnumerableStateMachineOps[O,A](val self: Enumerable[O,A]) extends AnyVal {
    def run() = Enumerable.impl.runEnumerableState(self.s0, HaltedRecoveryStrategy.STRICT[Unit,O,A])._1
    def run(haltedRecoveryStrategy : HaltedRecoveryStrategy[Unit,O,A]) = Enumerable.impl.runEnumerableState(self.s0, haltedRecoveryStrategy)
  }

  implicit class implicitIterateeStateOps[I,A](val self: Iteratee.State[I,A]) extends AnyVal {
    def flatMap[B](f: A => Iteratee.State[I,B]) : Iteratee.State[I,B] = Iteratee.impl.flatMapIterateeState(self, f)
    def map[B](f: A => B) : Iteratee.State[I,B] = Iteratee.impl.mapIterateeState(self, f)
  }

  implicit class implicitIterateeOps[I,A](val self: Iteratee[I,A]) extends AnyVal {
    def flatMap[B](f: A => Iteratee[I,B]) : Iteratee[I,B] = Iteratee.impl.flatMapIteratee(self, f)
    def map[B](f: A => B) : Iteratee[I,B] = Iteratee.impl.mapIteratee(self, f)
  }

//  implicit class implicitPlanStateOps[A](self: Plan.State[A]) {
//    def flatMap[II,OO,BB](f: A => StateMachine.State[II,OO,BB]) : StateMachine.State[II,OO,BB] = Plan.impl.flatMapPlanState(self, f)
//    def map[II,OO,BB](f: A => BB) : StateMachine.State[II,OO,BB] = Plan.impl.mapPlanState(self, f)
//  }
//
//  implicit class implicitPlanOps[A](self: Plan[A]) {
//    def flatMap[II,OO,BB](f: A => StateMachine[II,OO,BB]) : StateMachine[II,OO,BB] = Plan.impl.flatMapPlan[A,II,OO,BB](self, f)
//    def map[II,OO,BB](f: A => BB) : StateMachine[II,OO,BB] = Plan.impl.mapPlan[A,II,OO,BB](self, f)
//  }

  implicit class implicitEnumerableTransitionOps[O,A](val self: Enumerable.Transition[O,A]) extends AnyVal {
    def flatMap[OO,B](f: A => Enumerable.Transition[OO,B]) : Enumerable.Transition[OO,B] = Enumerable.impl.flatMapEnumerableTransition(self,f)
    def map[OO,B](f: A => B) : Enumerable.Transition[OO,B] = Enumerable.impl.mapEnumerableTransition(self,f)

//    def toOutput[II,AA](ifSuccess : => StateMachine.State[II,A,AA]) : StateMachine.Transition[II,A,AA] = Enumerable.impl.convertEnumerableTransitionToTransitionOutput(self, ifSuccess)
    def toIteratee[I] : Iteratee.Transition[I,A] = Enumerable.impl.convertEnumerableTransitionToTransitionValue(self)
  }

  implicit class implicitEnumerableTransitionSeqOps[O,A](val self: Enumerable.Transition[O,Seq[A]]) extends AnyVal {
    def toTranslator[I] : Translator.Transition[I,A] = Enumerable.impl.convertEnumerableTransitionToTransitionOutput(self, Translator.State.Success())
  }

  implicit class implicitSeqEnumerableTransitionOps[O,A](val self: Traversable[Enumerable.Transition[O,A]]) extends AnyVal {
    def invert : Enumerable.Transition[O,Seq[A]] = Enumerable.impl.invertEnumerableTransitionTraversable(self)
  }

  implicit class implicitIterateeContinuationFromFunction[I,A](f: Input[I] => Iteratee.Transition[I,A]) extends Iteratee.State.Continuation[I,A] {
//    override def apply(i: Input[I]) : Iteratee.Transition[I,A] = f(i)
    def apply(i: Input[I]) : Iteratee.Transition[I,A] = f(i)
    override def apply(xs: Seq[I]) : Iteratee.Transition[I,A] = f(Input(xs))
    def apply(x: I) : Iteratee.Transition[I,A] = f(Input(x))
    def apply(x: EndOfInput) : Iteratee.Transition[I,A] = f(EndOfInput)
  }

  implicit class implicitIterateeFromFunction[I,A](f: Input[I] => Iteratee.Transition[I,A]) extends Iteratee[I,A] {
    def s0 = f
  }

  implicit class implicitStateMachineStateTuple2[A,B,C](val tuple: (State[Unit,A,_], State[A,B,C])) extends AnyVal {
    def state = tuple._1 compose tuple._2
  }

  implicit class implicitStateMachineTuple2[A,B,C](tuple: (StateMachine[Unit,A,_], StateMachine[A,B,C])) extends StateMachine[Unit,B,C] {
    def s0 = tuple._1.s0 compose tuple._2.s0
  }

  implicit class implicitStateMachineStateTuple3[A,B,C,D](val tuple: (State[Unit,A,_], State[A,B,_], State[B,C,D])) extends AnyVal {
    def state = tuple._1 compose tuple._2 compose tuple._3
  }

  implicit class implicitStateMachineTuple3[A,B,C,D](tuple: (StateMachine[Unit,A,_], StateMachine[A,B,_], StateMachine[B,C,D])) extends StateMachine[Unit,C,D] {
    def s0 = tuple._1.s0 compose tuple._2.s0 compose tuple._3.s0
  }

}
