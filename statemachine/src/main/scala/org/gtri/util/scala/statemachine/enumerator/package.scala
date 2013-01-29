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

package object Enumerator {
  type Result[O]      = StateMachine.Result[Unit, O, Unit]
  object Result {
    def apply[O](
      state     :   State[O],
      output    :   Seq[O]                    = Seq.empty,
      metadata  :   Seq[Any]                  = Seq.empty
    ) = StateMachine.Result[Unit,O,Unit](
      state = state,
      output = output,
      overflow = Seq.empty,
      metadata = metadata
    )
  }

  type State[O]         = StateMachine.State[Unit, O, Unit]
  object State {
    type Continue[O]    = StateMachine.State.Continue[Unit, O, Unit]
    type Done[O]        = StateMachine.State.Done[Unit, O, Unit]

    type Success[O]     = StateMachine.State.Success[Unit, O, Unit]
    val Success         = StateMachine.State.Success

    type Failure[O]     = StateMachine.State.Failure[Unit, O, Unit]
    val Failure         = StateMachine.State.Failure
  }

  object Continue {
    def apply[O](
      state     :   State.Continue[O],
      output    :   Seq[O]                    = Seq.empty,
      metadata  :   Seq[Any]                  = Seq.empty
    ) = Result[O](
      state     =   state,
      output    =   output,
      metadata  =   metadata
    )
  }

  object Success {
    def apply[O](
      output    :   Seq[O]                    = Seq.empty,
      metadata  :   Seq[Any]                  = Seq.empty
    ) = Result[O](
      state     =   State.Success(()),
      output    =   output,
      metadata  =   metadata
    )
  }

  object Failure {
    def apply[O](
      optRecover  :   Option[() => Result[O]] = None,
      output      :   Seq[O]                  = Seq.empty,
      metadata    :   Seq[Any]                = Seq.empty
    ) = Result[O](
      state     =   State.Failure(optRecover),
      output    =   output,
      metadata  =   metadata
    )
  }

  /*
    Γ => output alphabet
    S => set of states
    s0 => initial state (s0 ∈ S)
    ∂ => state that can be transitioned
    F => set of final states (F ⊂ S)
  */
  type  S  [Γ]   =   State                [Γ]
  type  F  [Γ]   =   State.Done           [Γ]
  type  ∂  [Γ]   =   State.Continue       [Γ]

  val   ⊳        =   Continue
  val   ⊡        =   Success
  val   ⊠        =   Failure
}
