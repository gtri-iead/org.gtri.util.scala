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

package object Translator {
  type Result[I,O]      = StateMachine.Result[I,O,Unit]
  object Result {
    def apply[I,O](
      state     :   State[I,O],
      output    :   Seq[O]                      = Seq.empty,
      overflow  :   Seq[I]                      = Seq.empty,
      metadata  :   Seq[Any]                    = Seq.empty
    ) = StateMachine.Result[I,O,Unit](
      state     =   state,
      output    =   output,
      overflow  =   overflow,
      metadata =    metadata
    )
  }

  type State[I,O]         = StateMachine.State[I,O,Unit]
  object State {
    type Done[I,O]        = StateMachine.State.Done[I,O,Unit]

    type Continue[I,O]    = StateMachine.State.Continue[I,O,Unit]

    type Success[I,O]     = StateMachine.State.Success[I,O,Unit]
    val Success           = StateMachine.State.Success

    type Failure[I,O]     = StateMachine.State.Failure[I,O,Unit]
    val Failure           = StateMachine.State.Failure
  }

  object Continue {
    def apply[I,O](
      state     :   State.Continue[I,O],
      output    :   Seq[O]                        = Seq.empty,
      metadata  :   Seq[Any]                      = Seq.empty
    ) = Result[I,O](
      state     =   state,
      output    =   output,
      metadata  =   metadata
    )
  }

  object Success {
    def apply[I,O](
      output    :   Seq[O]                        = Seq.empty,
      overflow  :   Seq[I]                        = Seq.empty,
      metadata  :   Seq[Any]                      = Seq.empty
    ) = Result[I,O](
      state     =   State.Success(()),
      output    =   output,
      overflow  =   overflow,
      metadata  =   metadata
    )
  }

  object Failure {
    def apply[I,O](
      optRecover  :   Option[() => Result[I,O]]   = None,
      output      :   Seq[O]                      = Seq.empty,
      overflow    :   Seq[I]                      = Seq.empty,
      metadata    :   Seq[Any]                    = Seq.empty
    ) = Result[I,O](
      state     =   State.Failure(optRecover),
      output    =   output,
      overflow  =   overflow,
      metadata  =   metadata
    )
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
  type  S  [∑,Γ]   =   State                [∑,Γ]
  type  F  [∑,Γ]   =   State.Done                 [∑,Γ]
  type  ∂  [∑,Γ]   =   State.Continue             [∑,Γ]

  val   ⊳          =   Continue
  val   ⊡          =   Success
  val   ⊠          =   Failure
}
