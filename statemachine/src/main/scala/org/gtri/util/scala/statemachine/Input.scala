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

import scala.collection.immutable.Seq

object Input {
  def apply(eoi : EndOfInput) : Input[Nothing] = EndOfInput
  def apply[A](x : A) : Input[A] = Chunk(Seq(x))
  def apply[A](xs : Seq[A]) : Input[A] = Chunk(xs)
}
sealed trait Input[+A]
sealed trait EndOfInput extends Input[Nothing]
object EndOfInput extends EndOfInput
case class Chunk[A](xs: Seq[A]) extends Input[A]
