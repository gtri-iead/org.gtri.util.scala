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

import org.gtri.util.scala.statemachine.IssueSeverityCode._
import scala.collection.immutable.Seq

trait StateMachine[I,O,A] {
  import StateMachine._
  def s0 : State[I,O,A]
}
object StateMachine {
  val STD_CHUNK_SIZE = 256

  case class Transition[I,O,A](
    state         :   State[I,O,A],
    output        :   Seq[O]                        = Seq.empty,
    overflow      :   Seq[I]                        = Seq.empty,
    metadata      :   Seq[Any]                      = Seq.empty
  )

  object Continue {
    def apply[I,O,A](
      state       :   State.Continuation[I,O,A],
      output      :   Seq[O]                        = Seq.empty,
      metadata    :   Seq[Any]                      = Seq.empty
    ) = Transition[I,O,A](
      state     =   state,
      output    =   output,
      overflow  =   Seq.empty,
      metadata  =   metadata
    )
  }

  object Succeed {
    def apply[I,O,A](
      value       :   A,
      output      :   Seq[O]                        = Seq.empty,
      overflow    :   Seq[I]                        = Seq.empty,
      metadata    :   Seq[Any]                      = Seq.empty
    ) = Transition[I,O,A](
      state     =   State.Success(value),
      output    =   output,
      overflow  =   overflow,
      metadata  =   metadata
    )
  }

  object Halt {
    def apply[I,O,A](
      issues      :   Seq[Issue],
      optRecover  :   Option[() => Transition[I,O,A]]   = None,
      output      :   Seq[O]                            = Seq.empty,
      overflow    :   Seq[I]                            = Seq.empty,
      metadata    :   Seq[Any]                          = Seq.empty
    ) = Transition[I,O,A](
      state     =   State.Halted(
        issues        =   issues,
        optRecover    =   optRecover
      ),
      output    =   output,
      overflow  =   overflow,
      metadata  =   metadata
    )
  }

  sealed trait State[I,O,A] {

    // Better performing alternative to using match statement
    def fold[X](
      ifContinuation  : State.Continuation  [I,O,A] => X,
      ifSuccess       : State.Success       [I,O,A] => X,
      ifHalted        : State.Halted        [I,O,A] => X
    ) : X
  }

  object State {
    sealed trait Done[I,O,A] extends State[I,O,A] {
      // Better performing alternative to using match statement
      def fold[X](
        ifSuccess     : State.Success  [I,O,A]  => X,
        ifHalted      : State.Halted   [I,O,A]  => X
      ) : X
    }

    abstract class Continuation[I,O,A] extends State[I,O,A] {

      def apply( xs  : Seq[I]     ) : Transition[I,O,A] = utility.applyInputToState(this, xs, IssueRecoverStrategy.STRICT)
      def apply( x   : I          ) : Transition[I,O,A]
      def apply( x   : EndOfInput ) : Transition[I,O,A]

      final def fold[X](
        ifContinuation  : State.Continuation  [I,O,A] => X,
        ifSuccess       : State.Success       [I,O,A] => X,
        ifHalted        : State.Halted        [I,O,A] => X
      ) = ifContinuation(this)

    }

    final case class Success[I,O,A](value : A) extends Done[I,O,A] {

      def fold[X](
        ifContinuation  : State.Continuation  [I,O,A] => X,
        ifSuccess       : State.Success       [I,O,A] => X,
        ifHalted        : State.Halted        [I,O,A] => X
      ) = ifSuccess(this)

      def fold[X](
        ifSuccess     : State.Success [I,O,A]  => X,
        ifHalted      : State.Halted  [I,O,A]  => X
      ) = ifSuccess(this)
    }

    final case class Halted[I,O,A](
      issues          :   Seq[Issue],
      optRecover      :   Option[() => Transition[I,O,A]] = None
    ) extends Done[I,O,A] {
      def fold[X](
        ifContinuation  : State.Continuation  [I,O,A] => X,
        ifSuccess       : State.Success       [I,O,A] => X,
        ifHalted        : State.Halted        [I,O,A] => X
      ) = ifHalted(this)

      def fold[X](
        ifSuccess     : State.Success    [I,O,A]  => X,
        ifHalted      : State.Halted     [I,O,A]  => X
      ) = ifHalted(this)

      lazy val severityCode = issues.maxBy({ _.severityCode }).severityCode
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
//  type  S  [∑,Γ,A]   =   State                [∑,Γ,A]
//  type  F  [∑,Γ,A]   =   State.Done           [∑,Γ,A]
//  type  ∂  [∑,Γ,A]   =   State.Continuation       [∑,Γ,A]
//
//  val   ⊳            =   Continue
//  val   ⊡            =   Success
//  val   ⊠            =   Issue
}