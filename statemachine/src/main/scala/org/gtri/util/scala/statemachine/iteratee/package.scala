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

package object iteratee {
  type Iteratee[I,A] = StateMachine[I,Unit,A]

  type Transitor          [I,A]   =   SM_Transitor [I, Unit, A]
  type State              [I,A]   =   SM_State              [I, Unit, A]
  type Done               [I,A]   =   SM_Done               [I, Unit, A]
  type Continue           [I,A]   =   SM_Continue           [I, Unit, A]
  type Success            [I,A]   =   SM_Success            [I, Unit, A]
  type Failure            [I,A]   =   SM_Failure            [I, Unit, A]

  type  EndOfInput      =   SM_EndOfInput
  val   EndOfInput      =   SM_EndOfInput

  object Continue {
    def apply[I,A](next : Transitor[I,A]) : Continue[I,A] = new Continue(
      next = next,
      output = Seq.empty
    )
  }
  object Success {
    def apply[I,A](value : A, overflow : Seq[I] = Seq.empty) : Success[I,A] = new Success(
      value = value,
      output = Seq.empty,
      overflow = overflow
    )
    def unapply[I,A](state : State[I,A]) : Option[Success[I,A]] = {
      state.fold(
        ifContinue  = { q => None },
        ifSuccess = { q => Some(q) },
        ifFailure = { q => None }
      )
    }
  }
  object Failure {
    def apply[I,A](overflow : Seq[I] = Seq.empty, optRecover : Option[() => State[I,A]] = None) : Failure[I,A] = SM_Failure(
      output = Seq.empty,
      overflow = overflow
    )
    def unapply[I,A](state : State[I,A]) : Option[Failure[I,A]] = {
      state.fold(
        ifContinue  = { q => None },
        ifSuccess = { q => None },
        ifFailure = { q => Some(q) }
      )
    }
  }
}
