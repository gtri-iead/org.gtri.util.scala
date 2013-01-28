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

trait StateMachine[I,O,A] {
  import StateMachine._
  def s0 : State[I,O,A]
}
object StateMachine {
  case class Result[I,O,A](
    state         :   State[I,O,A],
    output        :   Seq[O]                        = Seq.empty,
    overflow      :   Seq[I]                        = Seq.empty,
    metadata      :   Seq[Any]                      = Seq.empty
  )

  object Continue {
    def apply[I,O,A](
      state       :   State.Continue[I,O,A],
      output      :   Seq[O]                        = Seq.empty,
      metadata    :   Seq[Any]                      = Seq.empty
    ) = Result[I,O,A](
      state     =   state,
      output    =   output,
      overflow  =   Seq.empty,
      metadata  =   metadata
    )
  }

  object Success {
    def apply[I,O,A](
      value       :   A,
      output      :   Seq[O]                        = Seq.empty,
      overflow    :   Seq[I]                        = Seq.empty,
      metadata    :   Seq[Any]                      = Seq.empty
    ) = Result[I,O,A](
      state     =   State.Success(value),
      output    =   output,
      overflow  =   overflow,
      metadata  =   metadata
    )
  }

  object Failure {
    def apply[I,O,A](
      optRecover  :   Option[() => Result[I,O,A]],
      output      :   Seq[O]                        = Seq.empty,
      overflow    :   Seq[I]                        = Seq.empty,
      metadata    :   Seq[Any]                      = Seq.empty
    ) = Result[I,O,A](
      state     =   State.Failure(optRecover),
      output    =   output,
      overflow  =   overflow,
      metadata  =   metadata
    )
  }

  sealed trait State[I,O,A] {

    // Better performing alternative to using match statement
    def fold[X](
      ifContinue  : State.Continue  [I,O,A] => X,
      ifSuccess   : State.Success   [I,O,A] => X,
      ifFailure   : State.Failure   [I,O,A] => X
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

      def apply( xs  : Seq[I]     ) : Result[I,O,A] = utility.applyInput(this, xs)
      def apply( x   : I          ) : Result[I,O,A]
      def apply( x   : EndOfInput ) : Result[I,O,A]

      def fold[X](
        ifContinue  : State.Continue  [I,O,A] => X,
        ifSuccess   : State.Success   [I,O,A] => X,
        ifFailure   : State.Failure   [I,O,A] => X
      ) = ifContinue(this)

    }

    final case class Success[I,O,A](value : A) extends Done[I,O,A] {

      def fold[X](
        ifContinue  : State.Continue  [I,O,A] => X,
        ifSuccess   : State.Success   [I,O,A] => X,
        ifFailure   : State.Failure   [I,O,A] => X
      ) = ifSuccess(this)

      def fold[X](
        ifSuccess   : State.Success [I,O,A]  => X,
        ifFailure   : State.Failure [I,O,A]  => X
      ) = ifSuccess(this)
    }

    final case class Failure[I,O,A](optRecover : Option[() => Result[I,O,A]] = None) extends Done[I,O,A] {
      def fold[X](
        ifContinue  : State.Continue  [I,O,A] => X,
        ifSuccess   : State.Success   [I,O,A] => X,
        ifFailure   : State.Failure   [I,O,A] => X
      ) = ifFailure(this)

      def fold[X](
        ifSuccess   : State.Success [I,O,A]  => X,
        ifFailure   : State.Failure [I,O,A]  => X
      ) = ifFailure(this)

    }
  }

  /*
  ∑ => input alphabet
  Γ => output alphabet
  S => set of states
  s0 => initial state (s0 ∈ S)
  ∂ => state that can be transitioned
  F => set of final states (F ⊂ S)
  ∅ => 1) the type of the empty set 2) instance of the empty set
  EOI => 1) type of end of input 2) instance of end of input
   */
  type  S  [∑,Γ,A]   =   State                [∑,Γ,A]
  type  F  [∑,Γ,A]   =   State.Done           [∑,Γ,A]
  type  ∂  [∑,Γ,A]   =   State.Continue       [∑,Γ,A]

  val   ⊳        =   Continue
  val   ⊡        =   Success
  val   ⊠        =   Failure
}