package org.gtri.util.scala.statemachine

package object Enumerable {
  type Result[O,A]      = StateMachine.Result[Unit, O, A]
  object Result {
    def apply[O,A](
      state     :   State[O,A],
      output    :   Seq[O]                    = Seq.empty,
      metadata  :   Seq[Any]                  = Seq.empty
    ) = StateMachine.Result[Unit,O,A](
      state = state,
      output = output,
      overflow = Seq.empty,
      metadata = metadata
    )
  }

  type State[O,A]         = StateMachine.State[Unit, O, A]
  object State {
    type Continue[O,A]    = StateMachine.State.Continue[Unit, O, A]
    type Done[O,A]        = StateMachine.State.Done[Unit, O, A]

    type Success[O,A]     = StateMachine.State.Success[Unit, O, A]
    val Success         = StateMachine.State.Success

    type Failure[O,A]     = StateMachine.State.Failure[Unit, O, A]
    val Failure         = StateMachine.State.Failure
  }

  object Continue {
    def apply[O,A](
      state     :   State.Continue[O,A],
      output    :   Seq[O]                    = Seq.empty,
      metadata  :   Seq[Any]                  = Seq.empty
    ) = Result[O,A](
      state     =   state,
      output    =   output,
      metadata  =   metadata
    )
  }

  object Success {
    def apply[O,A](
      value     :   A,
      output    :   Seq[O]                    = Seq.empty,
      metadata  :   Seq[Any]                  = Seq.empty
    ) = Result[O,A](
      state     =   State.Success(value),
      output    =   output,
      metadata  =   metadata
    )
  }

  object Failure {
    def apply[O,A](
      optRecover  :   Option[() => Result[O,A]] = None,
      output      :   Seq[O]                    = Seq.empty,
      metadata    :   Seq[Any]                  = Seq.empty
    ) = Result[O,A](
      state     =   State.Failure(optRecover),
      output    =   output,
      metadata  =   metadata
    )
  }

  /*
    Γ => output alphabet
    S => set of states
    s0 => initial state (s0 ∈ S)
    ∂ => state that can be transitioned
    F => set of final states (F ⊂ S)
  */
  type  S  [Γ,A]   =   State                [Γ,A]
  type  F  [Γ,A]   =   State.Done           [Γ,A]
  type  ∂  [Γ,A]   =   State.Continue       [Γ,A]

  val   ⊳        =   Continue
  val   ⊡        =   Success
  val   ⊠        =   Failure
}
