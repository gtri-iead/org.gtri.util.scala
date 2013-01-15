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
package org.gtri.util.scala.exelog

package object noop {
  type Logger = NoopLogger
  type Log = NoopLog
  implicit val logger = new Logger

  implicit class logMethods(self: Log)(implicit c : Class[_]) {
    @inline def block[A](methodName: String)(f: => A) : A = f
    @inline def block[A](methodName: String, args : => Seq[(String,Any)])(f: => A) : A = f


    @inline def begin(methodName : String) : Unit = { } //noop
    @inline def begin(methodName : String, args: => Seq[(String,Any)]) { } //noop


    @inline def end(methodName : String) : Unit = { } //noop
    @inline def end[A](methodName : String, retv : A) { } //noop

  }

  implicit class logMe(message : => String) {
    @inline def unary_~(implicit log : Log) = { } //noop
    @inline def unary_+(implicit log : Log) = { } //noop
  }

}