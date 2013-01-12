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

import org.apache.log4j.Level
import org.apache.log4j.Level._

trait ExeLog {
  def qualifiedName : String
  def name : String
  def formatMessage(message : String) : String = message
  
  def isEnabledFor(level : Level) : Boolean
  def log(fqcn : String, level : Level, message : => String, cause : Option[Throwable])
  def tryLog(fqcn : String, level : Level, message : => String, cause : Option[Throwable]) {
    if(isEnabledFor(level)) {
      val msg = if(message != null) formatMessage(message) else ""
      log(fqcn, level, msg, cause)
    }
  }

  def info(message: => String) = tryLog(qualifiedName, INFO, message, None)

  def +=(message: => String) = debug(message)
  def debug(message : => String) = tryLog(qualifiedName, DEBUG, message, None)
  
  def ~=(message: => String) = trace(message)
  def trace(message : => String) = tryLog(qualifiedName, TRACE, message, None)

  def warn(message : => String) = tryLog(qualifiedName, WARN, message, None)
  def warn(cause : Throwable) = tryLog(qualifiedName, WARN, cause.getMessage, Some(cause))
  def warn(message : => String, cause : Throwable) = tryLog(qualifiedName, WARN, message, Some(cause))

  def error(message : => String) = tryLog(qualifiedName, ERROR, message, None)
  def error(cause : Throwable) = tryLog(qualifiedName, ERROR, cause.getMessage, Some(cause))
  def error(message : => String, cause : Throwable) = tryLog(qualifiedName, ERROR, message, Some(cause))
  
  def fatal(message : => String) = tryLog(qualifiedName, FATAL, message, None)
  def fatal(cause : Throwable) = tryLog(qualifiedName, FATAL, cause.getMessage, Some(cause))
  def fatal(message : => String, cause : Throwable) = tryLog(qualifiedName, FATAL, message, Some(cause))

  def formatArg(arg : Any) : String = {
    if(arg == null) {
      "!null"
    } else
      arg match {
        // if a string, format with single-quotes and replace newlines with literal \n
        // TODO: limit string length
        case a : String => "'" + a.replaceAll("\n","""\\n""") + "'"
        // TODO: for known containers (traversable) limit length
        case a : Any => a.toString
      }
  }

  // format args (arg0=val0,arg1=val1,...)
  def formatArgs(args: Seq[(String, Any)]) = {
    "(" + (args map {
      t =>
        t._1 + "=" + formatArg(t._2)
    } mkString ",") + ")"
  }
}