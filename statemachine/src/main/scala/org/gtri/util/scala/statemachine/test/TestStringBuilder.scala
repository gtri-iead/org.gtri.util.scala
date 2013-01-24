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

package org.gtri.util.scala.statemachine.test

import org.gtri.util.scala.statemachine.iteratee._
import org.gtri.util.scala.statemachine.EndOfInput

class TestStringBuilder extends Iteratee[String, String] {
  case class Cont(acc : String) extends State.Continue[String, String] {

    def apply(item: String) = {
      println("received=" + item)
      Continue(Cont(acc + item))
    }

    def apply(eoi : EndOfInput) = {
      println("eoi=" + acc)
      Success(acc)
    }
  }
  def s0 = Cont("")
}
