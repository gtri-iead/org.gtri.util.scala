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

sealed trait State[I,O,A] {
  def output : Seq[O]

  // Better performing alternative to using match statement
  def fold[X](
    ifContinue  : Continue[I,O,A] => X,
    ifSuccess   : Success[I,O,A]  => X,
    ifFailure   : Failure[I,O,A]  => X
  ) : X
}

sealed trait Done[I,O,A] extends State[I,O,A] {
  // Better performing alternative to using match statement
  def fold[X](
    ifSuccess   : Success[I,O,A]  => X,
    ifFailure   : Failure[I,O,A]  => X
  ) : X
}

case class Continue[I,O,A](
  next        :   Transitor[I,O,A],
  output      :   Seq[O]                      =   Seq.empty
) extends State[I,O,A] {
  def fold[X](
    ifContinue  : Continue[I,O,A] => X,
    ifSuccess   : Success[I,O,A]  => X,
    ifFailure   : Failure[I,O,A]  => X
  ) = ifContinue(this)
}

case class Success[I,O,A](
  value       :   A,
  output      :   Seq[O]                      =   Seq.empty,
  overflow    :   Seq[I]                      =   Seq.empty
) extends Done[I,O,A] {

  def fold[X](
    ifContinue  : Continue[I,O,A] => X,
    ifSuccess   : Success[I,O,A]  => X,
    ifFailure   : Failure[I,O,A]  => X
  ) = ifSuccess(this)

  def fold[X](
    ifSuccess   : Success[I,O,A]  => X,
    ifFailure   : Failure[I,O,A]  => X
  ) = ifSuccess(this)

}

case class Failure[I,O,A](
  output      :   Seq[O]                      =   Seq.empty,
  overflow    :   Seq[I]                      =   Seq.empty,
  optRecover  :   Option[() => State[I,O,A]]  =   None
) extends Done[I,O,A] {

  def fold[X](
    ifContinue  : Continue[I,O,A] => X,
    ifSuccess   : Success[I,O,A]  => X,
    ifFailure   : Failure[I,O,A]  => X
  ) = ifFailure(this)

  def fold[X](
    ifSuccess   : Success[I,O,A]  => X,
    ifFailure   : Failure[I,O,A]  => X
  ) = ifFailure(this)

}
