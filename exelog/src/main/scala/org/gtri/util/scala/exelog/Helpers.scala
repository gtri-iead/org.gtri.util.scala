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

object Helpers {
  def exitMessage[A](c : Class[_], methodName : String, retv : A) : String = {
    c.getSimpleName + "." + methodName + " => " + formatArg(retv)
  }

  def enterMessage(c : Class[_], methodName: String, args: Seq[(String,Any)]) : String ={
    c.getSimpleName + "." + methodName + formatArgs(args)
  }

  def formatArg(arg : Any) : String = {
    if(arg == null) {
      "(null)"
    } else {
      arg match {
        // if a string, format with single-quotes and replace newlines with literal \n
        // TODO: limit string length
        case a : String => "'" + a.replaceAll("\n","""\\n""") + "'"
        // TODO: for known containers (traversable) limit length
        case a : Any => a.toString
      }
    }
  }

  // format args (arg0=val0,arg1=val1,...)
  def formatArgs(args: Seq[(String, Any)]) = {
    "(" + (args map { t => t._1 + "=" + formatArg(t._2) } mkString ",") + ")"
  }
}

