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

package object Translator {
  type Transition[I,O]        = StateMachine.Transition[I,O,Unit]
  object Transition {
    def apply[I,O](
      state     :   State[I,O],
      output    :   Seq[O]                      = Seq.empty,
      overflow  :   Seq[I]                      = Seq.empty,
      metadata  :   Seq[Any]                    = Seq.empty
    ) = StateMachine.Transition[I,O,Unit](state=state, output=output, overflow=overflow, metadata=metadata)
  }

  type State[I,O]             = StateMachine.State[I,O,Unit]
  object State {
    type Done[I,O]            = StateMachine.State.Done[I,O,Unit]

    type Continuation[I,O]    = StateMachine.State.Continuation[I,O,Unit]

    type Success[I,O]         = StateMachine.State.Success[I,O,Unit]
    val Success               = StateMachine.State.Success

    type Issue[I,O]           = StateMachine.State.Halted[I,O,Unit]
    val Issue                 = StateMachine.State.Halted
  }

  object Continue {
    def apply[I,O](
      state     :   State.Continuation[I,O],
      output    :   Seq[O]                        = Seq.empty,
      metadata  :   Seq[Any]                      = Seq.empty
    ) = StateMachine.Continue[I,O,Unit](state=state, output=output, metadata=metadata)
  }

  object Succeed {
    def apply[I,O](
      output    :   Seq[O]                        = Seq.empty,
      overflow  :   Seq[I]                        = Seq.empty,
      metadata  :   Seq[Any]                      = Seq.empty
    ) = StateMachine.Succeed[I,O,Unit](value=(), output=output, overflow=overflow, metadata=metadata)
  }

  object Halt {
    def apply[I,O](
      issues      :   Seq[Issue],
      optRecover  :   Option[() => Transition[I,O]] = None,
      output      :   Seq[O]                        = Seq.empty,
      overflow    :   Seq[I]                        = Seq.empty,
      metadata    :   Seq[Any]                      = Seq.empty
    ) = StateMachine.Halt[I,O,Unit](issues=issues, optRecover=optRecover, output=output, overflow=overflow, metadata=metadata)
    def warn[I,O](
      message     :   String,
      cause       :   Option[Throwable]            = None,
      recover     :   () => Transition[I,O],
      output      :   Seq[O]                       = Seq.empty,
      overflow    :   Seq[I]                       = Seq.empty,
      metadata    :   Seq[Any]                     = Seq.empty
    ) = StateMachine.Halt[I,O,Unit](issues=Seq(Issue.warn(message,cause)), optRecover=Some(recover), output=output, overflow=overflow, metadata=metadata)
    def error[I,O](
      message     :   String,
      cause       :   Option[Throwable]            = None,
      recover     :   () => Transition[I,O],
      output      :   Seq[O]                       = Seq.empty,
      overflow    :   Seq[I]                       = Seq.empty,
      metadata    :   Seq[Any]                     = Seq.empty
    ) = StateMachine.Halt[I,O,Unit](issues=Seq(Issue.error(message,cause)), optRecover=Some(recover), output=output, overflow=overflow, metadata=metadata)
    def fatal[I,O](
      message     :   String,
      cause       :   Option[Throwable]            = None,
      output      :   Seq[O]                       = Seq.empty,
      overflow    :   Seq[I]                       = Seq.empty,
      metadata    :   Seq[Any]                     = Seq.empty
    ) = StateMachine.Halt[I,O,Unit](issues=Seq(Issue.fatal(message,cause)), output=output, overflow=overflow, metadata=metadata)
  }

    /*
  ∑ => input alphabet
  Γ => output alphabet
  S => set of states
  s0 => initial state (s0 ∈ S)
  ∂ => transition function
  F => set of final states (F ⊂ S)
  ∅ => 1) the type of the empty set 2) instance of the empty set
   */
//  type  S  [∑,Γ]   =   State                [∑,Γ]
//  type  F  [∑,Γ]   =   State.Done                 [∑,Γ]
//  type  ∂  [∑,Γ]   =   State.Continue             [∑,Γ]
//
//  val   ⊳          =   Continue
//  val   ⊡          =   Success
//  val   ⊠          =   Issue
}
