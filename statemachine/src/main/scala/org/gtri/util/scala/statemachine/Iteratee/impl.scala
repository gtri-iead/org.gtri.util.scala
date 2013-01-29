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
package org.gtri.util.scala.statemachine.Iteratee

import org.gtri.util.scala.statemachine._
import org.gtri.util.scala.statemachine.Iteratee._
import org.gtri.util.scala.statemachine.utility

object impl {

  /**
   * Implementation of Iteratee.State.map
   * @param s
   * @param f
   * @tparam I
   * @tparam A
   * @tparam B
   * @return
   */
  def mapIterateeState[I,A,B](s : State[I,A], f: A => B) : State[I,B] = {
    flatMapIterateeState(s, { (a : A) => bindIterateeState(f(a)) } )
  }

  /**
   * Implementation of Iteratee.State.bind
   * @param value
   * @tparam I
   * @tparam A
   * @return
   */
  def bindIterateeState[I,A](value : A) : State[I,A] = State.Success(value)

  private[impl] def flatMapIterateeResult[I,A,B](r : Result[I,A], f: A => State[I,B]) : Result[I,B] = {
    r.state.fold(
      ifContinue = { q => Result(
        state = FlatMapIterateeStateContinue(q,f),
        metadata = r.metadata
      )},
      ifSuccess = { q =>
        val overflowResult = utility.applyInputToState(f(q.value),r.overflow, false)
        overflowResult.copy(metadata = overflowResult.metadata ++ r.metadata)
      },
      ifFailure = { q => Result(
        state = State.Failure(q.optRecover map { recover => () => flatMapIterateeResult(recover(), f)}),
        overflow = r.overflow,
        metadata = r.metadata
      )}
    )
  }

  private[impl] case class FlatMapIterateeStateContinue[I,A,B](s : State.Continue[I,A], f: A => State[I,B]) extends State.Continue[I,B] {
    override def apply(xs: Seq[I]) = flatMapIterateeResult(s.apply(xs),f)
  
    def apply(x: I) = flatMapIterateeResult(s.apply(x),f)
  
    def apply(x: EndOfInput) = flatMapIterateeResult(s.apply(x),f)
  }

  /**
   * Implementation of Iteratee.flatMap
   * @param s
   * @param f
   * @tparam I
   * @tparam A
   * @tparam B
   * @return
   */
  def flatMapIterateeState[I,A,B](s : State[I,A], f: A => State[I,B]) : State[I,B] = {
    s.fold(
      ifContinue = { q => FlatMapIterateeStateContinue(q,f) },
      ifSuccess = { q => f(q.value) },
      ifFailure = { q => State.Failure(q.optRecover map { recover => () => flatMapIterateeResult(recover(),f) }) }
    )
  }

  
  /**
   * Implementation of Iteratee.map
   * @param m
   * @param f
   * @tparam I
   * @tparam A
   * @tparam B
   * @return
   */
  def mapIteratee[I,A,B](m : Iteratee[I,A], f: A => B) : StateMachine[I,Unit,B] = {
    flatMapIteratee(m, { (a: A) => bindIteratee(f(a)) })
  }

  /**
   * Implementation of Iteratee.bind
   * @param value
   * @tparam I
   * @tparam A
   * @return
   */
  def bindIteratee[I,A](value : A) = new Iteratee[I,A] {
    def s0 = bindIterateeState(value)
  }

  private[impl] case class FlatMapIteratee[I,A,B](m : Iteratee[I,A], f: A => Iteratee[I,B]) extends Iteratee[I,B] {
    def s0 = flatMapIterateeState(m.s0, { (a : A) => f(a).s0 })
  }

  /**
   * Implementation of Iteratee.flatMap
   * @param m
   * @param f
   * @tparam I
   * @tparam A
   * @tparam B
   * @return
   */
  def flatMapIteratee[I,A,B](m : Iteratee[I,A], f: A => Iteratee[I,B]) : Iteratee[I,B] = FlatMapIteratee(m,f)

}
