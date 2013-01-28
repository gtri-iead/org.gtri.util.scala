package org.gtri.util.scala.statemachine

package object Iteratee {
  type Result[I,A]      = StateMachine.Result[I,Unit,A]
  object Result {
    def apply[I,A](
      state     :   State[I,A],
      overflow  :   Seq[I]                        = Seq.empty,
      metadata  :   Seq[Any]                      = Seq.empty
    ) = StateMachine.Result[I,Unit,A](
      state     =   state,
      output    =   Seq.empty,
      overflow  =   overflow,
      metadata  =   metadata
    )
  }

  type State[I,A]         = StateMachine.State[I,Unit,A]
  object State {
    type Done[I,A]        = StateMachine.State.Done[I,Unit,A]

    type Continue[I,A]    = StateMachine.State.Continue[I,Unit,A]

    type Success[I,A]     = StateMachine.State.Success[I,Unit,A]
    val Success           = StateMachine.State.Success

    type Failure[I,A]     = StateMachine.State.Failure[I,Unit,A]
    val Failure           = StateMachine.State.Failure
  }

  object Continue {
    def apply[I,A](
      state     :   State.Continue[I,A],
      metadata  :   Seq[Any]                      = Seq.empty
    ) = Result[I,A](
      state     =   state,
      metadata  =   metadata
    )
  }

  object Success {
    def apply[I,A](
      value : A,
      overflow    :   Seq[I]                      = Seq.empty,
      metadata    :   Seq[Any]                    = Seq.empty
    ) = Result[I,A](
      state     =   State.Success(value),
      overflow  =   overflow,
      metadata  =   metadata
    )
  }

  object Failure {
    def apply[I,A](
      optRecover  :   Option[() => Result[I,A]]   = None,
      overflow    :   Seq[I]                      = Seq.empty,
      metadata    :   Seq[Any]                    = Seq.empty
    ) = Result[I,A](
      state     =   State.Failure(optRecover),
      overflow  =   overflow,
      metadata  =   metadata
    )
  }

    /*
  ∑ => input alphabet
  S => set of states
  s0 => initial state (s0 ∈ S)
  ∂ => transition function
  F => set of final states (F ⊂ S)
  A => final success value type
  ∅ => 1) the type of the empty set 2) instance of the empty set
   */
  type  S  [∑,A]   =   State                      [∑,A]
  type  F  [∑,A]   =   State.Done                 [∑,A]
  type  ∂  [∑,A]   =   State.Continue             [∑,A]

  val   ⊳          =   Continue
  val   ⊡          =   Success
  val   ⊠          =   Failure
}
