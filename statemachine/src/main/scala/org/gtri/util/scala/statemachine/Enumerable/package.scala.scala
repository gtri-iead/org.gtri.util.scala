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

package object Enumerable {
  type Result[O,A]      = StateMachine.Result[Unit, O, A]
  object Result {
    def apply[O,A](
      state     :   State[O,A],
      output    :   Seq[O]                    = Seq.empty,
      metadata  :   Seq[Any]                  = Seq.empty
    ) = StateMachine.Result[Unit,O,A](
      state = state,
      output = output,
      overflow = Seq.empty,
      metadata = metadata
    )
  }

  type State[O,A]         = StateMachine.State[Unit, O, A]
  object State {
    type Continue[O,A]    = StateMachine.State.Continue[Unit, O, A]
    type Done[O,A]        = StateMachine.State.Done[Unit, O, A]

    type Success[O,A]     = StateMachine.State.Success[Unit, O, A]
    val Success         = StateMachine.State.Success

    type Failure[O,A]     = StateMachine.State.Failure[Unit, O, A]
    val Failure         = StateMachine.State.Failure
  }

  object Continue {
    def apply[O,A](
      state     :   State.Continue[O,A],
      output    :   Seq[O]                    = Seq.empty,
      metadata  :   Seq[Any]                  = Seq.empty
    ) = Result[O,A](
      state     =   state,
      output    =   output,
      metadata  =   metadata
    )
  }

  object Success {
    def apply[O,A](
      value     :   A,
      output    :   Seq[O]                    = Seq.empty,
      metadata  :   Seq[Any]                  = Seq.empty
    ) = Result[O,A](
      state     =   State.Success(value),
      output    =   output,
      metadata  =   metadata
    )
  }

  object Failure {
    def apply[O,A](
      optRecover  :   Option[() => Result[O,A]] = None,
      output      :   Seq[O]                    = Seq.empty,
      metadata    :   Seq[Any]                  = Seq.empty
    ) = Result[O,A](
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
  type  S  [Γ,A]   =   State                [Γ,A]
  type  F  [Γ,A]   =   State.Done           [Γ,A]
  type  ∂  [Γ,A]   =   State.Continue       [Γ,A]

  val   ⊳        =   Continue
  val   ⊡        =   Success
  val   ⊠        =   Failure
}
