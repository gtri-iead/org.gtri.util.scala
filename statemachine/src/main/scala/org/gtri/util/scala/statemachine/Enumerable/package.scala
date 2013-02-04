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

import org.gtri.util.scala.statemachine.IssueSeverityCode._
import scala.collection.immutable.Seq

package object Enumerable {
  type Transition[O,A]          = StateMachine.Transition[Unit, O, A]
  object Transition {
    def apply[O,A](
      state     :   State[O,A],
      output    :   Seq[O]                    = Seq.empty,
      metadata  :   Seq[Any]                  = Seq.empty
    ) = StateMachine.Transition[Unit,O,A](state=state, output=output, metadata=metadata)
  }

  type State[O,A]             = StateMachine.State[Unit, O, A]
  object State {
    type Continuation[O,A]    = StateMachine.State.Continuation[Unit, O, A]
    type Done[O,A]            = StateMachine.State.Done[Unit, O, A]

    type Success[O,A]         = StateMachine.State.Success[Unit, O, A]
    val Success               = StateMachine.State.Success

    type Halted[O,A]           = StateMachine.State.Halted[Unit, O, A]
    val Halted                 = StateMachine.State.Halted
  }

  object Continue {
    def apply[O,A](
      state     :   State.Continuation[O,A],
      output    :   Seq[O]                    = Seq.empty,
      metadata  :   Seq[Any]                  = Seq.empty
    ) = StateMachine.Continue[Unit,O,A](state=state, output=output, metadata=metadata)
  }

  object Succeed {
    def apply[O,A](
      value     :   A,
      output    :   Seq[O]                    = Seq.empty,
      metadata  :   Seq[Any]                  = Seq.empty
    ) = StateMachine.Succeed[Unit,O,A](value=value, output=output, metadata=metadata)
  }

  object Halt {
    def apply[O,A](
      issues      :   Seq[Issue],
      optRecover  :   Option[() => Transition[O,A]] = None,
      output      :   Seq[O]                        = Seq.empty,
      metadata    :   Seq[Any]                      = Seq.empty    
    ) = StateMachine.Halt[Unit,O,A](issues=issues, optRecover=optRecover, output=output, metadata=metadata) 
    def warn[O,A](
      message     :   String,
      cause       :   Option[Throwable]            = None,
      recover     :   () => Transition[O,A],
      output      :   Seq[O]                       = Seq.empty,
      metadata    :   Seq[Any]                     = Seq.empty
    ) = StateMachine.Halt[Unit,O,A](issues=Seq(Issue.warn(message,cause)), optRecover=Some(recover), output=output, metadata=metadata)
    def error[O,A](
      message     :   String,
      cause       :   Option[Throwable]            = None,
      recover     :   () => Transition[O,A],
      output      :   Seq[O]                       = Seq.empty,
      metadata    :   Seq[Any]                     = Seq.empty
    ) = StateMachine.Halt[Unit,O,A](issues=Seq(Issue.error(message,cause)), optRecover=Some(recover), output=output, metadata=metadata)
    def fatal[O,A](
      message     :   String,
      cause       :   Option[Throwable]            = None,
      output      :   Seq[O]                       = Seq.empty,
      metadata    :   Seq[Any]                     = Seq.empty
    ) = StateMachine.Halt[Unit,O,A](issues=Seq(Issue.fatal(message,cause)), output=output, metadata=metadata)
  }

  /*
    Γ => output alphabet
    S => set of states
    s0 => initial state (s0 ∈ S)
    ∂ => state that can be transitioned
    F => set of final states (F ⊂ S)
  */
//  type  S  [Γ,A]   =   State                [Γ,A]
//  type  F  [Γ,A]   =   State.Done           [Γ,A]
//  type  ∂  [Γ,A]   =   State.Continue       [Γ,A]
//
//  val   ⊳        =   Continue
//  val   ⊡        =   Success
//  val   ⊠        =   Issue
}
