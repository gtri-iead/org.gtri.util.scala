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

package object Enumerator {
  val STD_CHUNK_SIZE = 256

  type Transition[O]        = StateMachine.Transition[Unit, O, Unit]
  object Transition {
    def apply[O](
      state     :   State[O],
      output    :   Seq[O]                    = Seq.empty,
      metadata  :   Seq[Any]                  = Seq.empty
    ) = StateMachine.Transition[Unit,O,Unit](state=state, output=output, metadata=metadata)
  }

  type State[O]             = StateMachine.State[Unit, O, Unit]
  object State {
    type Continuation[O]    = StateMachine.State.Continuation[Unit, O, Unit]
    type Done[O]            = StateMachine.State.Done[Unit, O, Unit]

    type Success[O]         = StateMachine.State.Success[Unit, O, Unit]
    val Success             = StateMachine.State.Success

    type Halted[O]          = StateMachine.State.Halted[Unit, O, Unit]
    val Halted              = StateMachine.State.Halted
  }

  object Continue {
    def apply[O](
      state     :   State.Continuation[O],
      output    :   Seq[O]                    = Seq.empty,
      metadata  :   Seq[Any]                  = Seq.empty
    ) = StateMachine.Continue[Unit,O,Unit](state=state, output=output, metadata=metadata)
  }

  object Succeed {
    def apply[O](
      output    :   Seq[O]                    = Seq.empty,
      metadata  :   Seq[Any]                  = Seq.empty
    ) = StateMachine.Succeed[Unit,O,Unit](value=(), output=output, metadata=metadata)
  }

  object Halt {
    def apply[O](
      issues      :   Seq[Issue],
      optRecover  :   Option[() => Transition[O]]   = None,
      output      :   Seq[O]                        = Seq.empty,
      metadata    :   Seq[Any]                      = Seq.empty    
    ) = StateMachine.Halt[Unit,O,Unit](issues=issues, optRecover=optRecover, output=output, metadata=metadata) 
    def warn[O](
      message     :   String,
      cause       :   Option[Throwable]            = None,
      recover     :   () => Transition[O],
      output      :   Seq[O]                       = Seq.empty,
      metadata    :   Seq[Any]                     = Seq.empty
    ) = StateMachine.Halt[Unit,O,Unit](issues=Seq(Issue.warn(message,cause)), optRecover=Some(recover), output=output, metadata=metadata)
    def error[O](
      message     :   String,
      cause       :   Option[Throwable]            = None,
      recover     :   () => Transition[O],
      output      :   Seq[O]                       = Seq.empty,
      metadata    :   Seq[Any]                     = Seq.empty
    ) = StateMachine.Halt[Unit,O,Unit](issues=Seq(Issue.error(message,cause)), optRecover=Some(recover), output=output, metadata=metadata)
    def fatal[O](
      message     :   String,
      cause       :   Option[Throwable]            = None,
      output      :   Seq[O]                       = Seq.empty,
      metadata    :   Seq[Any]                     = Seq.empty
    ) = StateMachine.Halt[Unit,O,Unit](issues=Seq(Issue.fatal(message,cause)), output=output, metadata=metadata)
  }

  /*
    Γ => output alphabet
    S => set of states
    s0 => initial state (s0 ∈ S)
    ∂ => state that can be transitioned
    F => set of final states (F ⊂ S)
  */
//  type  S  [Γ]  =  State                [Γ]
//  type  F  [Γ]  =  State.Done           [Γ]
//  type  ∂  [Γ]  =  State.Continue       [Γ]
//
//  val   ⊳       =  Continue
//  val   ⊡       =  Success
//  val   ⊠       =  Issue
}
