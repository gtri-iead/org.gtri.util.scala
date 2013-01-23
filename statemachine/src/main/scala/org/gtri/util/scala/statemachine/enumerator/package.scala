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

package object enumerator {
  type Enumerator[O] = StateMachine[Unit,O,Unit]

  trait Transitor[O] extends SM_Transitor[  Unit, O, Unit] {
    def apply( in: Unit ) : State[O] = step()
    def step()            : State[O]
  }


  type State    [O]   = SM_State    [Unit, O, Unit]
  type Done     [O]   = SM_Done     [Unit, O, Unit]
  type Continue [O]   = SM_Continue [Unit, O, Unit]
  type Success  [O]   = SM_Success  [Unit, O, Unit]
  type Failure  [O]   = SM_Failure  [Unit, O, Unit]

  type  EndOfInput      =   SM_EndOfInput
  val   EndOfInput      =   SM_EndOfInput

  val Continue        = SM_Continue

  object Success {
    def apply[O](output : Seq[O] = Seq.empty) : Success[O] = new Success(
      value     = (),
      output    = output,
      overflow  = Seq.empty
    )
    def unapply[O](state : State[O]) : Option[Success[O]] = {
      state.fold(
        ifContinue  = { q => None },
        ifSuccess = { q => Some(q) },
        ifFailure = { q => None }
      )
    }
  }
  object Failure {
    def apply[O](output : Seq[O] = Seq.empty, optRecover : Option[() => State[O]] = None) : Failure[O] = new Failure(
      output    = output,
      overflow  = Seq.empty,
      optRecover = optRecover
    )
    def unapply[O](state : State[O]) : Option[Failure[O]] = {
      state.fold(
        ifContinue  = { q => None },
        ifSuccess = { q => None },
        ifFailure = { q => Some(q) }
      )
    }
  }
}
