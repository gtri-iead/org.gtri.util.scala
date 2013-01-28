package org.gtri.util.scala.statemachine

package object Plan {
  type Result[A]      = StateMachine.Result[Unit,Unit,A]
  object Result {
    def apply[A](
      state     :   State[A],
      metadata  :   Seq[Any]                      = Seq.empty
    ) = StateMachine.Result[Unit,Unit,A](
      state     =   state,
      metadata  =   metadata
    )
  }

  type State[A]         = StateMachine.State[Unit,Unit,A]
  object State {
    type Done[A]        = StateMachine.State.Done[Unit,Unit,A]

    type Continue[A]    = StateMachine.State.Continue[Unit,Unit,A]

    type Success[A]     = StateMachine.State.Success[Unit,Unit,A]
    val Success           = StateMachine.State.Success

    type Failure[A]     = StateMachine.State.Failure[Unit,Unit,A]
    val Failure           = StateMachine.State.Failure
  }

  object Continue {
    def apply[A](
      state     :   State.Continue[A],
      metadata  :   Seq[Any]                      = Seq.empty
    ) = Result[A](
      state     =   state,
      metadata  =   metadata
    )
  }

  object Success {
    def apply[A](
      value : A,
      metadata    :   Seq[Any]                    = Seq.empty
    ) = Result[A](
      state     =   State.Success(value),
      metadata  =   metadata
    )
  }

  object Failure {
    def apply[A](
      optRecover  :   Option[() => Result[A]]   = None,
      metadata    :   Seq[Any]                    = Seq.empty
    ) = Result[A](
      state     =   State.Failure(optRecover),
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
  type  S  [A]   =   State                      [A]
  type  F  [A]   =   State.Done                 [A]
  type  ∂  [A]   =   State.Continue             [A]

  val   ⊳          =   Continue
  val   ⊡          =   Success
  val   ⊠          =   Failure
}
