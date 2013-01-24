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
package org.gtri.util.scala.statemachine

package object translator {
  type Translator[I,O] = StateMachine[I,O,Unit]

//  type TransitionFunction [I,O]   =   statemachine.TransitionFunction  [I, O, Unit]
  type Result             [I,O]   =   statemachine.Result              [I, O, Unit]
  type Done               [I,O]   =   statemachine.Done                [I, O, Unit]
  type Continue           [I,O]   =   statemachine.Continue            [I, O, Unit]
  type Success            [I,O]   =   statemachine.Success             [I, O, Unit]
  type Failure            [I,O]   =   statemachine.Failure             [I, O, Unit]

  val Continue        = statemachine.Continue
  object Success {
    def apply[I,O](
      state       :   State.Success[I,O],
      output      :   Seq[O]                        =   Seq.empty
    ) : Success[I,O] = Success(state, output)
    def apply[I,O](
      output      :   Seq[O]                        =   Seq.empty
    ) : Success[I,O] = Success(State.Success(()), output)
  }
  object Failure {
    def apply[I,O](
      state       :   State.Failure[I,O],
      output      :   Seq[O]                        =   Seq.empty,
      optRecover  :   Option[() => Result[I,O]]       =   None
    ) : Failure[I,O] = statemachine.Failure(state, output, Seq.empty, optRecover)
    def apply[I,O](
      output      :   Seq[O]                        =   Seq.empty
    ) : Failure[I,O] = statemachine.Failure(new State.Failure, output)
  }

  type State      [I,O]   =   statemachine.State          [I, O, Unit]
  object State {
    type Done     [I,O]   = statemachine.State.Done       [I, O, Unit]
    type Continue [I,O]   = statemachine.State.Continue   [I, O, Unit]
    type Success  [I,O]   = statemachine.State.Success    [I, O, Unit]
    type Failure  [I,O]   = statemachine.State.Failure    [I, O, Unit]
//    val Continue          = statemachine.State.Continue
    val Success           = statemachine.State.Success
    val Failure           = statemachine.State.Failure
  }

}
