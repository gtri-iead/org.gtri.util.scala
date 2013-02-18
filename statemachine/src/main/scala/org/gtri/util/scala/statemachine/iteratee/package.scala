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

import IssueSeverityCode._
import scala.collection.immutable.Seq

package object Iteratee {
  type Transition[I,A]        = StateMachine.Transition[I,Unit,A]
  object Transition {
    def apply[I,A](
      state     :   State[I,A],
      overflow  :   Seq[I]                        = Seq.empty,
      metadata  :   Seq[Any]                      = Seq.empty
    ) = StateMachine.Transition[I,Unit,A](state=state, overflow=overflow, metadata=metadata)
  }

  type State[I,A]             = StateMachine.State[I,Unit,A]
  object State {
    type Done[I,A]            = StateMachine.State.Done[I,Unit,A]

    type Continuation[I,A]    = StateMachine.State.Continuation[I,Unit,A]
    
    type Success[I,A]         = StateMachine.State.Success[I,Unit,A]
    val Success               = StateMachine.State.Success

    type Halted[I,A]          = StateMachine.State.Halted[I,Unit,A]
    val Halted                = StateMachine.State.Halted
  }

  object Continue {
    def apply[I,A](
      state     :   State.Continuation[I,A],
      metadata  :   Seq[Any]                      = Seq.empty
    ) = StateMachine.Continue[I,Unit,A](state=state, metadata=metadata)
  }

  object Succeed {
    def apply[I,A](
      value : A,
      overflow    :   Seq[I]                      = Seq.empty,
      metadata    :   Seq[Any]                    = Seq.empty
    ) = StateMachine.Succeed[I,Unit,A](value=value, overflow=overflow, metadata=metadata)
  }

  object Halt {
    def apply[I,A](
      issues      :   Seq[Issue],
      optRecover  :   Option[() => Transition[I,A]] = None,
      overflow    :   Seq[I]                        = Seq.empty,
      metadata    :   Seq[Any]                      = Seq.empty    
    ) = StateMachine.Halt[I,Unit,A](issues=issues, optRecover=optRecover, overflow=overflow, metadata=metadata) 
    def warn[I,A](
      message     :   String,
      cause       :   Option[Throwable]            = None,
      recover     :   () => Transition[I,A],
      overflow    :   Seq[I]                       = Seq.empty,
      metadata    :   Seq[Any]                     = Seq.empty
    ) = StateMachine.Halt.warn[I,Unit,A](message=message, cause=cause, recover=recover, overflow=overflow, metadata=metadata)
    def error[I,A](
      message     :   String,
      cause       :   Option[Throwable]            = None,
      recover     :   () => Transition[I,A],
      overflow    :   Seq[I]                       = Seq.empty,
      metadata    :   Seq[Any]                     = Seq.empty
    ) = StateMachine.Halt.error[I,Unit,A](message=message, cause=cause, recover=recover, overflow=overflow, metadata=metadata)
    def fatal[I,A](
      message     :   String,
      cause       :   Option[Throwable]            = None,
      overflow    :   Seq[I]                       = Seq.empty,
      metadata    :   Seq[Any]                     = Seq.empty
    ) = StateMachine.Halt.fatal[I,Unit,A](message=message, cause=cause, overflow=overflow, metadata=metadata)
  }

  def peek[A] : Iteratee[A,A] = impl.peek[A]

    /*
  ∑ => input alphabet
  S => set of states
  s0 => initial state (s0 ∈ S)
  ∂ => transition function
  F => set of final states (F ⊂ S)
  A => final success value type
  ∅ => 1) the type of the empty set 2) instance of the empty set
   */
//  type  S  [∑,A]   =   State                      [∑,A]
//  type  F  [∑,A]   =   State.Done                 [∑,A]
//  type  ∂  [∑,A]   =   State.Continue             [∑,A]
//
//  val   ⊳          =   Continue
//  val   ⊡          =   Success
//  val   ⊠          =   Issue
}
