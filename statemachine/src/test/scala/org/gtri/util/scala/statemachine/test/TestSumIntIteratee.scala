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

import org.gtri.util.scala.statemachine._

case class TestSumIntIteratee(maxN : Int = Int.MaxValue) extends Iteratee[Int, Int] {
//  import Iteratee._
  case class Cont(n : Int, loopState : Int) extends Iteratee.State.Continue[Int, Int] {
    def apply(item : Int) = {
//      println(s"n=$n item=$item")
      if(n < maxN) {
        Iteratee.Continue(new Cont(n + 1,loopState + item))
      } else {
        Iteratee.Success(loopState + item)
      }
    }

    def apply(eoi : EndOfInput) = Iteratee.Success(loopState)
  }
  def s0 = Cont(1, 0)
}

