///*
//    Copyright 2012 Georgia Tech Research Institute
//
//    Author: lance.gatlin@gtri.gatech.edu
//
//    This file is part of org.gtri.util.scala library.
//
//    org.gtri.util.scala library is free software: you can redistribute it and/or modify
//    it under the terms of the GNU General Public License as published by
//    the Free Software Foundation, either version 3 of the License, or
//    (at your option) any later version.
//
//    org.gtri.util.scala library is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//    GNU General Public License for more details.
//
//    You should have received a copy of the GNU General Public License
//    along with org.gtri.util.scala library. If not, see <http://www.gnu.org/licenses/>.
//
//*/
//package org.gtri.util.scala.oprecover.test
//
//import org.scalatest.FunSpec
//import org.gtri.util.iteratee.impl.OptRecover._
//import org.gtri.util.iteratee.api.Issues._
//import scalaz._
//import Scalaz._
//
///**
// * Created with IntelliJ IDEA.
// * User: Lance
// * Date: 12/28/12
// * Time: 7:17 PM
// * To change this template use File | Settings | File Templates.
// */
//
//class OptRecoverSpec extends FunSpec {
//
//  val issue1 = log("asdf1",java.util.logging.Level.INFO)
//  val issue2 = log("asdf2",java.util.logging.Level.INFO)
//  val issue3 = log("asdf3",java.util.logging.Level.INFO)
//  val issue4 = log("asdf4",java.util.logging.Level.INFO)
//  val issue5 = log("asdf5",java.util.logging.Level.INFO)
//  val issue6 = log("asdf6",java.util.logging.Level.INFO)
//  val issue7 = log("asdf7",java.util.logging.Level.INFO)
//  val issue8 = log("asdf8",java.util.logging.Level.INFO)
//
//  describe("A OptRecover[+A]") {
//
//    it("should be able to append debug messages") {
//      val b1 : OptRecover[Int] = OptRecover(issue1, 1)
//      val OptRecover = b1 :++> List(issue2)
//      assert(OptRecover.written.contains(issue1))
//      assert(OptRecover.written.contains(issue2))
//    }
//
//    it("should be able to accumulate debug messages in a for-comprehension") {
//      val b1 : OptRecover[Int] = OptRecover(issue1, 1)
//      val b2 : OptRecover[Int] = OptRecover(issue2, 2)
//      val b3 : OptRecover[Int] = OptRecover(issue3, 3)
//      val bsum =
//        for(innerA <- b1; innerB <- b2; innerC <- b3)
//        yield for (a <- innerA; b <- innerB; c <- innerC)
//        yield a+b+c
//
//      assert(bsum.value.isGo)
//      assert(bsum.value.get == 6)
//      assert(bsum.written.contains(issue1))
//      assert(bsum.written.contains(issue2))
//      assert(bsum.written.contains(issue3))
//    }
//
//    it("should be able to retrieve debug messages from recovery") {
//      lazy val v2 : OptRecover[Int] = { println("here");2.OptRecover(issue2) }
//      lazy val v3 : OptRecover[Int] = { println("here");3.OptRecover(issue3) }
//      val b1 : OptRecover[Int] = OptRecover(1)
//      val b2 : OptRecover[Int] = OptRecover.recover(v2)
//      val b3 : OptRecover[Int] = OptRecover.recover(v3)
//      val bsum =
//        for(innerA <- b1; innerB <- b2; innerC <- b3)
//        yield for (a <- innerA; b <- innerB; c <- innerC)
//        yield a+b+c
//
//      assert(bsum.value.isRecover)
//      val result = bsum.value.recover
//      assert(result.value == Some(6))
//      assert(result.written.contains(issue2))
//      assert(result.written.contains(issue3))
//    }
//  }
//
//}