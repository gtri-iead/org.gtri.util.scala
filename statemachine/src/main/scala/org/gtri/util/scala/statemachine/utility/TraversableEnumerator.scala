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
package org.gtri.util.scala.statemachine.utility

import org.gtri.util.scala.statemachine._

case class TraversableEnumerator[A](
  t         :     Traversable[A],
  chunkSize :     Int               =   STD_CHUNK_SIZE
) extends Enumerator[A] {
  import Enumerator._
  require(chunkSize > 0)

  case class ContWithoutProgress(current : Traversable[A]) extends State.Continuation[A] {

    def apply(x : Unit) : Transition[A] = {
      val (nextChunk, remaining) = current.splitAt(chunkSize)
      if(remaining.isEmpty) {
        Succeed(nextChunk.toSeq)
      } else {
        Continue(ContWithoutProgress(remaining), nextChunk.toSeq)
      }
    }
    def apply(x : EndOfInput) = Succeed()
  }

  case class ContWithProgress(maxN : Int, current : Traversable[A]) extends State.Continuation[A] {

    def currentProgress = Progress(maxN - current.size, maxN)

    def apply(x : Unit) : Transition[A] = {
      val (nextChunk, remaining) = current.splitAt(chunkSize)
      if(remaining.isEmpty) {
        Succeed(
          output = nextChunk.toSeq,
          metadata = Seq(currentProgress, Progress(maxN, maxN))
        )
      } else {
        val output = nextChunk.toSeq
        Continue(
          state = ContWithProgress(maxN,remaining),
          output = output,
          metadata = Seq(currentProgress)
        )
      }
    }
    def apply(x : EndOfInput) = Succeed(metadata = Seq(currentProgress))
  }

  def s0 = {
    if(t.hasDefiniteSize) {
      ContWithProgress(t.size,t)
    } else {
      ContWithoutProgress(t)
    }
  }
}