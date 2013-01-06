package org.gtri.util.iteratee.impl.box.test

import org.scalatest.FunSpec
import org.gtri.util.iteratee.impl.box._
import org.gtri.util.iteratee.api.Issues._
import scalaz._
import Scalaz._

/**
 * Created with IntelliJ IDEA.
 * User: Lance
 * Date: 12/28/12
 * Time: 7:17 PM
 * To change this template use File | Settings | File Templates.
 */

class BoxSpec extends FunSpec {

  val issue1 = log("asdf1",java.util.logging.Level.INFO)
  val issue2 = log("asdf2",java.util.logging.Level.INFO)
  val issue3 = log("asdf3",java.util.logging.Level.INFO)
  val issue4 = log("asdf4",java.util.logging.Level.INFO)
  val issue5 = log("asdf5",java.util.logging.Level.INFO)
  val issue6 = log("asdf6",java.util.logging.Level.INFO)
  val issue7 = log("asdf7",java.util.logging.Level.INFO)
  val issue8 = log("asdf8",java.util.logging.Level.INFO)

  describe("A Box[+A]") {

    it("should be able to append debug messages") {
      val b1 : Box[Int] = Box(issue1, 1)
      val box = b1 :++> List(issue2)
      assert(box.written.contains(issue1))
      assert(box.written.contains(issue2))
    }

    it("should be able to accumulate debug messages in a for-comprehension") {
      val b1 : Box[Int] = Box(issue1, 1)
      val b2 : Box[Int] = Box(issue2, 2)
      val b3 : Box[Int] = Box(issue3, 3)
      val bsum =
        for(innerA <- b1; innerB <- b2; innerC <- b3)
        yield for (a <- innerA; b <- innerB; c <- innerC)
        yield a+b+c

      assert(bsum.value.isGo)
      assert(bsum.value.get == 6)
      assert(bsum.written.contains(issue1))
      assert(bsum.written.contains(issue2))
      assert(bsum.written.contains(issue3))
    }

    it("should be able to retrieve debug messages from recovery") {
      lazy val v2 : Box[Int] = { println("here");2.box(issue2) }
      lazy val v3 : Box[Int] = { println("here");3.box(issue3) }
      val b1 : Box[Int] = Box(1)
      val b2 : Box[Int] = Box.recover(v2)
      val b3 : Box[Int] = Box.recover(v3)
      val bsum =
        for(innerA <- b1; innerB <- b2; innerC <- b3)
        yield for (a <- innerA; b <- innerB; c <- innerC)
        yield a+b+c

      assert(bsum.value.isRecover)
      val result = bsum.value.recover
      assert(result.value == Some(6))
      assert(result.written.contains(issue2))
      assert(result.written.contains(issue3))
    }
  }

}