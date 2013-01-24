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

package object iteratee {
  type Iteratee[I,A] = StateMachine[I,Unit,A]

//  type TransitionFunction [I,A]   =   statemachine.TransitionFunction [I, Unit, A]
  type Result             [I,A]   =   statemachine.Result             [I, Unit, A]
  type Done               [I,A]   =   statemachine.Done               [I, Unit, A]
  type Continue           [I,A]   =   statemachine.Continue           [I, Unit, A]
  type Success            [I,A]   =   statemachine.Success            [I, Unit, A]
  type Failure            [I,A]   =   statemachine.Failure            [I, Unit, A]

  val Continue        = statemachine.Continue
  object Success {
    def apply[I,A](
      state       :   State.Success[I,A],
      value       :   A,
      overflow    :   Seq[I] = Seq.empty
    ) : Success[I,A] = new Success(state, Seq.empty, overflow)
    def apply[I,A](
      value       :   A,
      overflow    :   Seq[I] = Seq.empty
    ) : Success[I,A] = new Success[I,A](State.Success.apply(value), Seq.empty, overflow)
  }
  object Failure {
    def apply[I,A](
      state       :   State.Failure[I,A],
      overflow    :   Seq[I] = Seq.empty,
      optRecover  :   Option[() => Result[I,A]] = None
    ) : Failure[I,A] = new Failure(state, Seq.empty, overflow, optRecover)
    def apply[I,A](
      overflow    :   Seq[I] = Seq.empty
    ) : Failure[I,A] = new Failure(new State.Failure[I,A], Seq.empty, overflow)
  }

  type State      [I,A]   =   statemachine.State          [I, Unit, A]
  object State {
    type Done     [I,A]   = statemachine.State.Done       [I, Unit, A]
    type Continue [I,A]   = statemachine.State.Continue   [I, Unit, A]
    type Success  [I,A]   = statemachine.State.Success    [I, Unit, A]
    type Failure  [I,A]   = statemachine.State.Failure    [I, Unit, A]
//    val Continue          = statemachine.State.Continue
    val Success           = statemachine.State.Success
    val Failure           = statemachine.State.Failure
  }

}
