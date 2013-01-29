package org.gtri.util.scala.statemachine

import org.scalatest.FunSpec
import org.gtri.util.scala.statemachine._
import scala.util.Random
import test._

class TraversableEnumeratorTest extends FunSpec {
  val rnd = Stream.continually(Random.nextInt(100))

  describe("A TraversableEnumerator[O]") {
    it("should be be constructable from any traversable") {
      val e : Enumerator[Int] = rnd.toEnumerator
    }
    it("should end with the Success state once input is exhausted") {
      val n = STD_CHUNK_SIZE * 2
      val l : List[Int] = rnd.take(n).toList
      val e : Enumerator[Int] = l.toEnumerator
      val state0 : Enumerator.State[Int] = e.s0
      val result = state0.run()
      val isSuccess =
        result.state match {
          case q : Enumerator.State.Continue[Int] => false
          case q : Enumerator.State.Success[Int] => true
          case q : Enumerator.State.Failure[Int] => false
        }
      // Just demo'ing a second way of doing this
      val isSuccess2 =
        result.state.fold(
          ifContinue = { q => false },
          ifSuccess = { q => true },
          ifFailure = { q => false }
        )
      assert(isSuccess == true && isSuccess2 == true)
    }
    it("should return Success state when endOfInput is applied to a continue state") {
      val n = STD_CHUNK_SIZE * 2
      val l : List[Int] = rnd.take(n).toList
      val e : Enumerator[Int] = l.toEnumerator
      val state0 = e.s0
      val result = utility.forceDoneState(state0)
      val isSuccess =
        result.state.fold(
          ifContinue = { q => false },
          ifSuccess = { q => true },
          ifFailure = { q => false }
        )
      assert(isSuccess == true)
    }
  }
}
