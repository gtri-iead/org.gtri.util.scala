/*
    Copyright 2012 Georgia Tech Research Institute

    Author: lance.gatlin@gtri.gatech.edu

    This file is part of org.gtri.util.scala library.

    org.gtri.util.scala library is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    org.gtri.util.scala library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with org.gtri.util.scala library. If not, see <http://www.gnu.org/licenses/>.

*/
package org.gtri.util.scala.optrecover.test

import scala.language.higherKinds
import org.scalatest.FunSpec
import org.gtri.util.scala.box._
import scalaz._
import Scalaz._

/**
 * Created with IntelliJ IDEA.
 * User: Lance
 * Date: 12/28/12
 * Time: 7:17 PM
 * To change this template use File | Settings | File Templates.
 */

class OptRecoverMSpec extends FunSpec {

  describe("A OptRecoverM[M[+_],A]") {

    it("should be able to store an item (Go)") {
      val b1 : OptRecoverM[Id,Int] = OptRecoverM(1)
      assert(b1 == Go(1))
      assert(b1.isGo)
      assert(b1.get == 1)
      val b2 : OptRecoverM[Id,Int] = Go(1)
      assert(b2 == Go(1))
    }

    it("should be able to be empty (NoGo)") {
      val b1 : OptRecoverM[Id,Int] = OptRecoverM.empty
      assert(b1 == NoGo())
      assert(b1.isNoGo)
      val b2 : OptRecoverM[Id,Int] = NoGo()
      assert(b2 == NoGo())
    }

    it("should be able to be empty but provide a method to recover from bad input (Recover)") {
      lazy val v : Id[Option[Int]] = { Some(2) }
      val b1 : OptRecoverM[Id,Int] = OptRecoverM.recover[Id, Int](v)
      assert(b1.isRecover)
      assert(b1.recoverable eq v)
      val b2 : OptRecoverM[Id,Int] = Recover[Id, Int](v)
      assert(b2.isRecover)
    }

    it("should be able to recover from bad input") {
      lazy val v : Id[Option[Int]] = { Some(2) }
      val b1 : OptRecoverM[Id,Int] = OptRecoverM.recover[Id, Int](v)
      val result = b1.recover
      assert(result == Some(2))
    }

    it("should not evaluate the recoverable until the request to recover from bad input") {
      var s = false
      lazy val v : Id[Option[Int]] = { s = true;Some(2) }
      val b1 : OptRecoverM[Id,Int] = OptRecoverM.recover[Id, Int](v)
      assert(s == false)
      val result = b1.recover
      assert(result == Some(2))
      assert(s == true)
    }

    it("should be able to wrap the recoverable in any Monad") {
      type StrWriter[+A] = Writer[List[String],A]
      lazy val writer = { Writer(List("message"),Some(2)) }
      val b1 : OptRecoverM[StrWriter,Int] = OptRecoverM.recover[StrWriter,Int](writer)
      val result = b1.recover
      assert(result.value == Some(2))
      assert(result.written.contains("message"))
    }

    it("should be able to convert to an Option") {
      val b1 : OptRecoverM[Id,Int] = OptRecoverM(1)
      val opt1 : Option[Int] = b1.toOption
      assert(opt1 == Some(1))
      val b2 : OptRecoverM[Id,Int] = OptRecoverM.empty
      val opt2 : Option[Int] = b2.toOption
      assert(opt2 == None)
      lazy val v : Id[Option[Int]] = { Some(2) }
      val b3 : OptRecoverM[Id,Int] = OptRecoverM.recover(v)
      val opt3 : Option[Int] = b3.toOption
      assert(opt3 == None)
    }

    it("should be able to extract its value using a for-comprehension") {
      val b1 : OptRecoverM[Id,Int] = OptRecoverM(1)
      val b2 : OptRecoverM[Id,Int] = OptRecoverM(2)
      val b3 : OptRecoverM[Id,Int] = OptRecoverM(3)
      val bsum = for(a <- b1; b <- b2; c <- b3) yield a+b+c
      assert(bsum.isGo)
      assert(bsum.get == 6)
    }

    it("should return a NoGo in a for-comprehension if any OptRecoverM is a NoGo") {
      val b1 : OptRecoverM[Id,Int] = OptRecoverM(1)
      val b2 : OptRecoverM[Id,Int] = OptRecoverM.empty
      val b3 : OptRecoverM[Id,Int] = OptRecoverM(3)
      val bsum = for(a <- b1; b <- b2; c <- b3) yield a+b+c
      assert(bsum.isNoGo)
    }

    it("should return a Recover in a for-comprehension if any OptRecoverM is a Recover") {
      lazy val v : Id[Option[Int]] = { Some(2) }
      val b1 : OptRecoverM[Id,Int] = OptRecoverM(1)
      val b2 : OptRecoverM[Id,Int] = OptRecoverM.recover(v)
      val b3 : OptRecoverM[Id,Int] = OptRecoverM(3)
      val bsum = for(a <- b1; b <- b2; c <- b3) yield a+b+c
      assert(bsum.isRecover)
    }

    it("should be able to return the result of a for-comprehension that can be recovered") {
      lazy val v2 : Id[Option[Int]] = { Some(2) }
      lazy val v3 : Id[Option[Int]] = { Some(3) }
      val b1 : OptRecoverM[Id,Int] = OptRecoverM(1)
      val b2 : OptRecoverM[Id,Int] = OptRecoverM.recover(v2)
      val b3 : OptRecoverM[Id,Int] = OptRecoverM.recover(v3)
      val bsum = for(a <- b1; b <- b2; c <- b3) yield a+b+c
      assert(bsum.isRecover)
      val result = bsum.recover
      assert(result == Some(6))
    }

    it("should return None if any OptRecoverM of a for-comprehension that can be recovered is NoGo") {
      lazy val v2 : Id[Option[Int]] = { Some(2) }
      lazy val v3 : Id[Option[Int]] = { Some(3) }
      val b1 : OptRecoverM[Id,Int] = OptRecoverM(1)
      val b2 : OptRecoverM[Id,Int] = OptRecoverM.recover(v2)
      val b3 : OptRecoverM[Id,Int] = OptRecoverM.recover(v3)
      val b4 : OptRecoverM[Id,Int] = OptRecoverM.empty
      val bsum = for(a <- b1; b <- b2; c <- b3; d <- b4) yield a+b+c+d
      assert(bsum.isRecover)
      val result = bsum.recover
      assert(result == None)
    }

    it("should map/flatMap the Recover Monad wrapper in a recoverable for-comprehension") {
      type StrWriter[+A] = Writer[List[String],A]
      lazy val writer2 = { Writer(List("message2"),Some(2)) }
      lazy val writer3 = { Writer(List("message3"),Some(3)) }
      val b1 : OptRecoverM[StrWriter,Int] = OptRecoverM(1)
      val b2 : OptRecoverM[StrWriter,Int] = OptRecoverM.recover[StrWriter,Int](writer2)
      val b3 : OptRecoverM[StrWriter,Int] = OptRecoverM.recover[StrWriter,Int](writer3)
      val bsum = for(a <- b1; b <- b2; c <- b3) yield a+b+c
      assert(bsum.isRecover)
      val result = bsum.recover
      assert(result.value == Some(6))
      assert(result.written.contains("message2"))
      assert(result.written.contains("message3"))
    }
  }

  it("should be a scalaz.Monad and all Monad laws should be true") {
    import OptRecoverM._

    type MyBox[+A] = OptRecoverM[List,A]
    // TODO: fix me
//    val m = implicitly[Monad[MyBox]]
    val m = OptRecoverMMonad[List]
    assert(m.monadLaw.rightIdentity(OptRecoverM(1)) == true)
    assert(m.monadLaw.leftIdentity[Int,String](1, { a => OptRecoverM(a.toString)}) == true)
    assert(m.monadLaw.associativeBind[Int,String,Char](OptRecoverM(1), { a => OptRecoverM(a.toString)}, { b => OptRecoverM(b(0))}) == true)
  }

}