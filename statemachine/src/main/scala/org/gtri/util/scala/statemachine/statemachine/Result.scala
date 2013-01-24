package org.gtri.util.scala.statemachine.statemachine

sealed trait Result[I,O,A] {
 
  def state : State[I,O,A]

  def output : Seq[O]

  def fold[X](
    ifContinue  :   Continue  [I,O,A]   => X,
    ifSuccess   :   Success   [I,O,A]   => X,
    ifFailure   :   Failure   [I,O,A]   => X
  ) : X
}

case class Continue[I,O,A](
  state : State.Continue[I,O,A],
  output : Seq[O]                 =   Seq.empty
) extends Result[I,O,A] {

  def fold[X](
  ifContinue  :   Continue  [I,O,A]   => X,
  ifSuccess   :   Success   [I,O,A]   => X,
  ifFailure   :   Failure   [I,O,A]   => X
  ) = ifContinue(this)
}

sealed trait Done[I,O,A] extends Result[I,O,A] {
  def overflow : Seq[I]

  def fold[X](
    ifSuccess   : Success [I,O,A]  => X,
    ifFailure   : Failure [I,O,A]  => X
  ) : X
}

case class Success[I,O,A](
  state       :   State.Success[I,O,A],
  output      :   Seq[O]                        =   Seq.empty,
  overflow    :   Seq[I]                        =   Seq.empty
) extends Done[I,O,A] {

  def fold[X](
    ifContinue  :   Continue  [I,O,A]   => X,
    ifSuccess   :   Success   [I,O,A]   => X,
    ifFailure   :   Failure   [I,O,A]   => X
 ) = ifSuccess(this)

  def fold[X](
    ifSuccess   : Success [I,O,A]  => X,
    ifFailure   : Failure [I,O,A]  => X
  ) = ifSuccess(this)
}

case class Failure[I,O,A](
  state       :   State.Failure[I,O,A],
  output      :   Seq[O]                        =   Seq.empty,
  overflow    :   Seq[I]                        =   Seq.empty,
  optRecover  :   Option[() => Result[I,O,A]]   =   None
) extends Done[I,O,A] {

  def fold[X](
    ifContinue  :   Continue  [I,O,A]   => X,
    ifSuccess   :   Success   [I,O,A]   => X,
    ifFailure   :   Failure   [I,O,A]   => X
  ) = ifFailure(this)

  def fold[X](
    ifSuccess   : Success [I,O,A]  => X,
    ifFailure   : Failure [I,O,A]  => X
  ) = ifFailure(this)
}