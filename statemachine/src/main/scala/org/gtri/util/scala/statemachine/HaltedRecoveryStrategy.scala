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

import IssueSeverityCode._
import org.gtri.util.scala.statemachine.StateMachine._
import org.gtri.util.scala.statemachine.StateMachine.State.Halted

object HaltedRecoveryStrategy {
  implicit class implicitHaltedRecoveryStrategyFromFunction[I,O,A](f: StateMachine.State.Halted[I,O,A] => Boolean) extends HaltedRecoveryStrategy[I,O,A] {
    def recoverOnce(s: State.Halted[I, O, A]) = (if(f(s)) s.optRecover map { recover => recover() } getOrElse Transition(s) else Transition(s), this)
    def recoverAll(s: State.Halted[I, O, A]) = (utility.recoverAll(s,f,Int.MaxValue), this)
  }

  // Never recover
  def STRICT[I,O,A] = new HaltedRecoveryStrategy[I,O,A] {
    def recoverOnce(s: Halted[I, O, A]) = (Transition(s),this)

    def recoverAll(s: Halted[I, O, A]) = (Transition(s),this)
  }

  // Only recover warnings
  def NORMAL[I,O,A] : HaltedRecoveryStrategy[I,O,A]= (q: State.Halted[_,_,_]) =>
    q.severityCode match {
      case WARN => true
      case ERROR => false
      case FATAL => false
    }

  // Always recover
  def LAX[I,O,A] : HaltedRecoveryStrategy[I,O,A] = (q: State.Halted[_,_,_]) => true
}
trait HaltedRecoveryStrategy[I,O,A] {
  def recoverOnce(s: State.Halted[I,O,A]) : (Transition[I,O,A], HaltedRecoveryStrategy[I,O,A])
  def recoverAll(s: State.Halted[I,O,A]) : (Transition[I,O,A], HaltedRecoveryStrategy[I,O,A])
}

