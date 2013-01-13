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

import org.apache.log4j
import org.apache.log4j.Level
import org.apache.log4j.Level._
import org.gtri.util.scala.exelog

object Log4jLog {
  def apply(name : String, parentName: String) = new Log4jLog(name,parentName)
}
class Log4jLog(
  val name : String,
  val parentName : String
) extends exelog.Log {

  val fqcn = parentName + "." + name
  val log = log4j.Logger.getLogger(fqcn)

//  def formatMessage(message : String) : String = s"$name: $message"

  def tryLog(level : Level, message: => String) {
    if(log.isEnabledFor(level)) {
      log.log(fqcn, level, message, null)
    }
  }

  def tryLog(level : Level, message: => String, cause : Throwable) {
    if(log.isEnabledFor(level)) {
      log.log(fqcn, level, message, cause)
    }
  }

  def trace(message: => String) = tryLog(TRACE, message)

  def debug(message: => String) = tryLog(DEBUG, message)

  def info(message: => String) = tryLog(INFO, message)

  def warn(message: => String) = tryLog(WARN, message)

  def warn(cause: Throwable) = tryLog(WARN, cause.getMessage, cause)

  def warn(message: => String, cause: Throwable) = tryLog(WARN, message, cause)

  def error(message: => String) = tryLog(ERROR, message)

  def error(cause: Throwable) = tryLog(ERROR, cause.getMessage, cause)

  def error(message: => String, cause: Throwable) = tryLog(ERROR, message, cause)

  def fatal(message: => String) = tryLog(FATAL,message)

  def fatal(cause: Throwable) = tryLog(FATAL, cause.getMessage, cause)

  def fatal(message: => String, cause: Throwable) = tryLog(FATAL, message, cause)
}
