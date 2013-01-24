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
package org.gtri.util.scala.statemachine.statemachine

import org.gtri.util.scala.statemachine.EndOfInput
import org.gtri.util.scala.statemachine.utility._

sealed trait State[I,O,A] {
  import State._

  // Better performing alternative to using match statement
  def fold[X](
    ifContinue  : Continue  [I,O,A] => X,
    ifSuccess   : Success   [I,O,A] => X,
    ifFailure   : Failure   [I,O,A] => X
  ) : X
}

object State {
  sealed trait Done[I,O,A] extends State[I,O,A] {
    // Better performing alternative to using match statement
    def fold[X](
      ifSuccess   : Success [I,O,A]  => X,
      ifFailure   : Failure [I,O,A]  => X
    ) : X
  }

  abstract class Continue[I,O,A] extends State[I,O,A] {

    def apply( xs  : Seq[I]     ) : Result[I,O,A] = run(this, xs)
    def apply( x   : I          ) : Result[I,O,A]
    def apply( x   : EndOfInput ) : org.gtri.util.scala.statemachine.statemachine.Done[I,O,A]
    //    def delta : TransitionFunction[I,O,A]

    def fold[X](
      ifContinue  : Continue  [I,O,A] => X,
      ifSuccess   : Success   [I,O,A] => X,
      ifFailure   : Failure   [I,O,A] => X
    ) = ifContinue(this)

  }
//  object Continue {
//    def apply[I,O,A](_delta : TransitionFunction[I,O,A]) = new Continue[I,O,A] {
//      def delta = _delta
//    }
//  }

  abstract class Success[I,O,A] extends Done[I,O,A] {
    def value : A

    def fold[X](
      ifContinue  : Continue  [I,O,A] => X,
      ifSuccess   : Success   [I,O,A] => X,
      ifFailure   : Failure   [I,O,A] => X
    ) = ifSuccess(this)

    def fold[X](
      ifSuccess   : Success [I,O,A]  => X,
      ifFailure   : Failure [I,O,A]  => X
    ) = ifSuccess(this)
  }
  object Success {
    def apply[I,O,A](_value : A) = new Success[I,O,A] {
      val value = _value
    }
  }

  class Failure[I,O,A] extends Done[I,O,A] {
    def fold[X](
      ifContinue  : Continue  [I,O,A] => X,
      ifSuccess   : Success   [I,O,A] => X,
      ifFailure   : Failure   [I,O,A] => X
    ) = ifFailure(this)

    def fold[X](
      ifSuccess   : Success [I,O,A]  => X,
      ifFailure   : Failure [I,O,A]  => X
    ) = ifFailure(this)

  }
  object Failure {
    def apply[I,O,A] = new Failure
  }

}