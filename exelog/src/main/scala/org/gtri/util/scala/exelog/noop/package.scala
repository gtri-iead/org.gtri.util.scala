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
  def init(args: => Seq[(String,Any)] = Nil)(implicit classLog : ClassLog) : MethodLog = MethodLog(classLog.name,args) // still have to return a methodlog
  def enter(name: String)(args: => Seq[(String,Any)] = Nil)(implicit classLog : ClassLog) : MethodLog = MethodLog(name, args) // still have to return a class log
  def exit[A](a: => A)(implicit log : MethodLog) = a // noop

  implicit class logMe(message : => String) {
    def unary_~(implicit log : MethodLog) = { } // noop
    def unary_+(implicit log : MethodLog) = { } // noop
  }

  implicit class logFunctions(log: MethodLog) {
    def :~>[A](a : A) = a // noop
    def <~:[A](a : A) = a // noop
  }

}
