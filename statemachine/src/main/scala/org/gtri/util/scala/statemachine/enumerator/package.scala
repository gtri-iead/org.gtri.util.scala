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

package object enumerator {

//  trait TransitionFunction[O] extends statemachine.TransitionFunction[Unit, O, Unit] {
//    def apply( in: Unit ) : Result[O] = step()
//    def step()            : Result[O]
//  }

  type Result   [O]   = statemachine.Result           [Unit, O, Unit]
  type Done     [O]   = statemachine.Done             [Unit, O, Unit]
  type Continue [O]   = statemachine.Continue         [Unit, O, Unit]
  type Success  [O]   = statemachine.Success          [Unit, O, Unit]
  type Failure  [O]   = statemachine.Failure          [Unit, O, Unit]
  val Continue        = statemachine.Continue
  object Success {
    def apply[O](
      state       :   State.Success[O],
      output      :   Seq[O]                        =   Seq.empty
    ) : Success[O] = Success(state, output)
    def apply[O](
      output      :   Seq[O]                        =   Seq.empty
    ) : Success[O] = Success(State.Success(()), output)
  }
  object Failure {
    def apply[O](
      state       :   State.Failure[O],
      output      :   Seq[O]                        =   Seq.empty,
      optRecover  :   Option[() => Result[O]]       =   None
    ) : Failure[O] = new Failure(state, output, Seq.empty, optRecover)
    def apply[O](
      output      :   Seq[O]                        =   Seq.empty
    ) : Failure[O] = new Failure(new State.Failure, output)
  }

  type State      [O]   = statemachine.State            [Unit, O, Unit]
  object State {
    type Done     [O]   = statemachine.State.Done       [Unit, O, Unit]
    trait Continue[O] extends statemachine.State.Continue[Unit, O, Unit] {
      def apply( in: Unit ) : Result[O] = step()
      def step()            : Result[O]
    }
    type Success  [O]   = statemachine.State.Success    [Unit, O, Unit]
    type Failure  [O]   = statemachine.State.Failure    [Unit, O, Unit]
//    val Continue        = statemachine.State.Continue
    val Success         = statemachine.State.Success
    val Failure         = statemachine.State.Failure
  }
}
