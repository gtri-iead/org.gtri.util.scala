package org.gtri.util.scala.statemachine

import org.scalatest.FunSpec
import scala.util.Random
import test._

class PlanTest extends FunSpec {
  val rnd = Stream.continually(Random.nextInt(100))

  describe("A Plan[A]") {
    it("should be composable from an Enumerator and an Iteratee") {
      val n = STD_CHUNK_SIZE * 2
      val l : List[Int] = rnd.take(n).toList
      val sum = l.foldLeft(0) { _ + _ }
      val e : Enumerator[Int] = l.toEnumerator
      val i : Iteratee[Int,Int] = TestSumIntIteratee()
      val ei : Plan[Int] = e compose i
      val result = ei.run()
      val opt = result.toOption
      assert(opt.isDefined && opt.get == sum)
    }
    it("should be composable from an Enumerator, any number of Translators and an Iteratee") {
      val n = STD_CHUNK_SIZE * 2
      val l : List[Int] = rnd.take(n).toList
      val sum = l.foldLeft("") { _ + _.toString }
      val e : Enumerator[Int] = l.toEnumerator
      val t : Translator[Int,String] = TestIntToStringTranslator()
      val i : Iteratee[String,String] = TestAppendStringIteratee()
      val eit : Plan[String] = e compose t compose i
      val result = eit.run()
      val opt = result.toOption
      assert(opt.isDefined && opt.get == sum)
    }
    it("should be able to recover from failure") {
      val n = STD_CHUNK_SIZE * 2
      val l : List[Int] = rnd.take(n).toList
      val sum = l.foldLeft(0) { _ + _ }
      val e : Enumerator[Int] = TestRecoverEnumerator(l)
      val i : Iteratee[Int,Int] = TestRecoverSumIntIteratee()
      val ei : Plan[Int] = e compose i
      val result = ei.run()
      val isRecover =
        result.state match {
          case q : Plan.State.Continue[Int] => false
          case q : Plan.State.Failure[Int] => q.optRecover.isDefined
          case q: Plan.State.Success[Int] => false
        }
      assert(isRecover == true)
      val result2 = result.state.run(recover = true)
      val opt = result2.toOption
      assert(opt.isDefined && opt.get == sum)
    }
    it("should accumulate metadata") {
      val n = STD_CHUNK_SIZE * 2
      val l : List[Int] = rnd.take(n).toList
      val sum = l.foldLeft("") { _ + _.toString }
      val e : Enumerator[Int] = TestRecoverEnumerator(l) // (n * 2) + ((n / 5) * 2)
      val t : Translator[Int,String] = TestIntToStringTranslator()  // (n * 2) + 2
      val i : Iteratee[String,String] = TestAppendStringIteratee() // (n * 2) + 2
      val eit : Plan[String] = e compose t compose i
      val result = eit.run(recover = true)
      println("result.metdata.length="+result.metadata.length)
      val eMetadataCount = (n * 2) + ((n/5)*2)
      val tMetadataCount = (n * 2) + 2
      val iMetadataCount = (n * 2) + 2
      assert(result.metadata.length == (eMetadataCount + tMetadataCount + iMetadataCount))
    }
  }
}
