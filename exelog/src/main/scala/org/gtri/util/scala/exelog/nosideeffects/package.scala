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

/**
 * Created with IntelliJ IDEA.
 * User: Lance
 * Date: 1/8/13
 * Time: 5:52 PM
 * To change this template use File | Settings | File Templates.
 */
package object nosideeffects {
  def init(args: => Seq[(String,Any)] = Nil)(implicit classLog : ClassLog) : MethodLog = MethodLog(classLog.name,args)
  def enter(name: String)(args: => Seq[(String,Any)] = Nil)(implicit classLog : ClassLog) : MethodLog = MethodLog(name, args)
  def exit[A](a: => A)(implicit log : MethodLog) : (List[LogEntry], A) = {
    log exit a
    (log.entries, a)
  }

  implicit class logMe(message : => String) {
    def unary_~(implicit log : MethodLog) = log trace message
    def unary_+(implicit log : MethodLog) = log debug message
  }

  implicit class logFunctions(log: MethodLog) {
    def :~>[A](a : A) = exit(a)(log)
    def <~:[A](a : A) = exit(a)(log)
  }
}
