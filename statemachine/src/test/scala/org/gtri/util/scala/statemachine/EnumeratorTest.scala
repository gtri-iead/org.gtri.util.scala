package org.gtri.util.scala.statemachine

import org.scalatest.FunSpec
import org.gtri.util.scala.statemachine._
import scala.util.Random
import test._

class EnumeratorTest extends FunSpec {
  val rnd = Stream.continually(Random.nextInt(100))

  describe("A Enumerator[O]") {
    it("should provide a single result chunk when stepped") {
      val n = STD_CHUNK_SIZE
      val l : List[Int] = rnd.take(n).toList
      val e : Enumerator[Int] = l.toEnumerator
      val result = e.s0.step()
      assert(result.output == l)
    }
    it("should contain all output in result after run") {
      val n = STD_CHUNK_SIZE * 2
      val l : List[Int] = rnd.take(n).toList
      val e : Enumerator[Int] = l.toEnumerator
      val state0 = e.s0
      val result = state0.run()
      assert(result.output == l)
    }
    it("should be able to recover from failure") {
      val n = STD_CHUNK_SIZE * 2
      val l : List[Int] = rnd.take(n).toList
      val e : Enumerator[Int] = TestRecoverEnumerator(l)
      val result = e.run()
      val isRecover =
        result.state match {
          case q : Enumerator.State.Continue[Int] => false
          case q : Enumerator.State.Failure[Int] => q.optRecover.isDefined
          case q: Enumerator.State.Success[Int] => false
        }
      assert(isRecover == true)
      val result2 = result.state.run(recover = true)
      assert(result.output ++ result2.output == l)
    }
    it("should accumulate metadata") {
      val n = STD_CHUNK_SIZE * 2
      val l : List[Int] = rnd.take(n).toList
      val e : Enumerator[Int] = TestRecoverEnumerator(l)
      val result = e.run(recover = true)
      assert(result.metadata.length == (n * 2) + ((n / 5) * 2))
    }
  }
}


