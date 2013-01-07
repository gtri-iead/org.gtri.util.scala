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
package org.gtri.util.scala

import scala.language.higherKinds


/**
 * Created with IntelliJ IDEA.
 * User: Lance
 * Date: 1/6/13
 * Time: 9:59 PM
 * To change this template use File | Settings | File Templates.
 */
package object optrecover {
  type OptRecover[M[+_], A] = M[OptRecoverM[M,A]]
}


//package org.gtri.util.iteratee.impl
//
//import language.higherKinds
//import scalaz._
//import Scalaz._
////import org.gtri.util.iteratee.api.Issue
//
///**
// * Created with IntelliJ IDEA.
// * User: Lance
// * Date: 12/24/12
// * Time: 7:35 AM
// * To change this template use File | Settings | File Templates.
// */
//package object box {
//
////  type LogWriter[+A] = Writer[List[Issue],A]
////  object LogWriter {
////    def apply[A](a : A) : LogWriter[A] = apply(Nil, a)
////    def apply[A](issue : Issue, a : A) : LogWriter[A] = apply(List(issue), a)
////    def apply[A](issues : List[Issue], a : A) : LogWriter[A] = Writer(issues, a)
////  }
////
////  type InnerBox[+A] = OptRecover[LogWriter,A]
////  type Box[+A] = LogWriter[InnerBox[A]]
////  object Box {
////    def empty[A] : Box[A] = empty[A](Nil)
////    def empty[A](issue : Issue) : Box[A] = empty[A](List(issue))
////    def empty[A](log : List[Issue]) : Box[A] = LogWriter(log, OptRecover.empty[LogWriter,A])
////
////    def apply[A](a : A) : Box[A] = apply[A](Nil,a)
////    def apply[A](issue : Issue,a : A) : Box[A] = apply[A](List(issue),a)
////    def apply[A](log : List[Issue],a : A) : Box[A] = LogWriter(log,OptRecover[LogWriter,A](a))
////
////    def recover[A](recoverable : => Box[A]) : Box[A] = recover[A](Nil,recoverable)
////    def recover[A](issue : Issue,recoverable : => Box[A]) : Box[A] = recover[A](List(issue),recoverable)
////    def recover[A](log : List[Issue],recoverable : => Box[A]) : Box[A] = {
////      lazy val r = {
////        val opt = recoverable.value.toOption
////        val log = recoverable.written
////        LogWriter(log,opt)
////      }
////      LogWriter(log, OptRecover.recover[LogWriter,A](r))
////    }
////  }
////
////  implicit class boxAnything[A](self : A) {
////    def box : Box[A] = Box(self)
////    def box(issue : Issue) : Box[A] = box(List(issue))
////    def box(log : List[Issue]) : Box[A] = Box(log, self)
////  }
//}