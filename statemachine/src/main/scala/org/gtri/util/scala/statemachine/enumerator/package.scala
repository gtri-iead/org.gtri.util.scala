package org.gtri.util.scala.statemachine

package object Enumerator {
  type Result[O]      = StateMachine.Result[Unit, O, Unit]
  object Result {
    def apply[O](
      state     :   State[O],
      output    :   Seq[O]                    = Seq.empty,
      metadata  :   Seq[Any]                  = Seq.empty
    ) = StateMachine.Result[Unit,O,Unit](
      state = state,
      output = output,
      overflow = Seq.empty,
      metadata = metadata
    )
  }

  type State[O]         = StateMachine.State[Unit, O, Unit]
  object State {
    type Continue[O]    = StateMachine.State.Continue[Unit, O, Unit]
    type Done[O]        = StateMachine.State.Done[Unit, O, Unit]

    type Success[O]     = StateMachine.State.Success[Unit, O, Unit]
    val Success         = StateMachine.State.Success

    type Failure[O]     = StateMachine.State.Failure[Unit, O, Unit]
    val Failure         = StateMachine.State.Failure
  }

  object Continue {
    def apply[O](
      state     :   State.Continue[O],
      output    :   Seq[O]                    = Seq.empty,
      metadata  :   Seq[Any]                  = Seq.empty
    ) = Result[O](
      state     =   state,
      output    =   output,
      metadata  =   metadata
    )
  }

  object Success {
    def apply[O](
      output    :   Seq[O]                    = Seq.empty,
      metadata  :   Seq[Any]                  = Seq.empty
    ) = Result[O](
      state     =   State.Success(()),
      output    =   output,
      metadata  =   metadata
    )
  }

  object Failure {
    def apply[O](
      optRecover  :   Option[() => Result[O]] = None,
      output      :   Seq[O]                  = Seq.empty,
      metadata    :   Seq[Any]                = Seq.empty
    ) = Result[O](
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
  type  S  [Γ]   =   State                [Γ]
  type  F  [Γ]   =   State.Done           [Γ]
  type  ∂  [Γ]   =   State.Continue       [Γ]

  val   ⊳        =   Continue
  val   ⊡        =   Success
  val   ⊠        =   Failure
}
