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

import org.gtri.util.scala.statemachine.{
  Transitor => SM_Transitor,
  State => SM_State,
  Done => SM_Done,
  Continue => SM_Continue,
  Success => SM_Success,
  Failure => SM_Failure,
  EndOfInput => SM_EndOfInput
}

package object translator {
  type Translator[I,O] = StateMachine[I,O,Unit]

  type Transitor          [I,O]   =   SM_Transitor           [I, O, Unit]
  type State              [I,O]   =   SM_State               [I, O, Unit]
  type Done               [I,O]   =   SM_Done                [I, O, Unit]
  type Continue           [I,O]   =   SM_Continue            [I, O, Unit]
  type Success            [I,O]   =   SM_Success             [I, O, Unit]
  type Failure            [I,O]   =   SM_Failure             [I, O, Unit]

  type  EndOfInput      =   SM_EndOfInput
  val   EndOfInput      =   SM_EndOfInput

  val Continue = SM_Continue

  object Success {
    def apply[I,O](output : Seq[O] = Seq.empty, overflow : Seq[I] = Seq.empty) : Success[I,O] = SM_Success(
      value = (),
      output = output,
      overflow = overflow
    )
    def unapply[I,O](state : State[I,O]) : Option[Success[I,O]] = {
      state.fold(
        ifContinue  = { q => None },
        ifSuccess = { q => Some(q) },
        ifFailure = { q => None }
      )
    }
  }
  object Failure {
    def apply[I,O](output : Seq[O] = Seq.empty, overflow : Seq[I] = Seq.empty, optRecover : Option[() => State[I,O]] = None) : Failure[I,O] = SM_Failure(
      output = output,
      overflow = overflow
    )
    def unapply[I,O](state : State[I,O]) : Option[Failure[I,O]] = {
      state.fold(
        ifContinue  = { q => None },
        ifSuccess = { q => None },
        ifFailure = { q => Some(q) }
      )
    }
  }

}
