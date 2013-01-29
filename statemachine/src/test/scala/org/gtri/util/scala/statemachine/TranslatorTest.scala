package org.gtri.util.scala.statemachine

import org.scalatest.FunSpec
import scala.util.Random
import test._

class TranslatorTest extends FunSpec {
  val rnd = Stream.continually(Random.nextInt(100))

  describe("A Translator[I,O]") {
    it("should be composable with an Enumerator to form a new Enumerator") {
      val n = STD_CHUNK_SIZE * 2
      val l : List[Int] = rnd.take(n).toList
      val ls : List[String] = for(i <- l) yield i.toString
      val e : Enumerator[Int] = l.toEnumerator
      val t : Translator[Int,String] = TestIntToStringTranslator()
      val ei : Enumerator[String] = e compose t
      val result = ei.run()
      assert(result.output == ls)
    }
    it("should be composable with an Iteratee to form a new Iteratee") {
      val n = STD_CHUNK_SIZE * 2
      val l = rnd.take(n).toList
      val ls = l.foldLeft("") { _ + _.toString }
      val i : Iteratee[String,String] = TestAppendStringIteratee()
      val t : Translator[Int,String] = TestIntToStringTranslator()
      val it : Iteratee[Int,String] = t compose i
      // Note: utility functions called directly for testing purposes. An enumerator should be composed with the iteratee instead
      val result = utility.forceDoneResult(utility.applyInputToState(it.s0, l, false))
      val optSum : Option[String] = result.toOption
      assert(optSum.isDefined && optSum.get == ls)
    }
    it("should accumulate metadata") {
      val n = STD_CHUNK_SIZE * 2
      val l : List[Int] = rnd.take(n).toList
      val ls : List[String] = for(i <- l) yield i.toString
      val e : Enumerator[Int] = l.toEnumerator
      val t : Translator[Int,String] = TestIntToStringTranslator()
      val ei : Enumerator[String] = e compose t
      val result = ei.run()
      assert(result.metadata.length == (n * 2)+2)
    }
  }
}
