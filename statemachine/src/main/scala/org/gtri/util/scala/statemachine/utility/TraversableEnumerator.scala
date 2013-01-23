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
package org.gtri.util.scala.statemachine.utility

import org.gtri.util.scala.statemachine._
import org.gtri.util.scala.statemachine.enumerator._
import org.gtri.util.scala.statemachine.enumerator.aliases._

class TraversableEnumerator[A](
  t         :     Traversable[A],
  chunkSize :     Int               =   STD_CHUNK_SIZE
) extends Enumerator[A] {
  require(chunkSize > 0)

  case class ∂∂(current : Traversable[A]) extends ∂[A] {

    def step() : S[A] = {
      val (nextChunk, remaining) = current.splitAt(chunkSize)
      if(remaining.isEmpty) {
        ⊡(nextChunk.toSeq)
      } else {
        ⊳(∂∂(remaining), nextChunk.toSeq)
      }
    }
    def apply(x : EOI) = ⊡()
  }

  def s0 = ⊳(∂∂(t))
}
