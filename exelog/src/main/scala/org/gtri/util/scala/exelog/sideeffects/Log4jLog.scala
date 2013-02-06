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
package org.gtri.util.scala.exelog.sideeffects

import org.apache.log4j
import org.apache.log4j.Level
import org.apache.log4j.Level._
import org.gtri.util.scala.exelog

final class Log4jLog(
  val name : String,
  val parentName : String
) {

  val fqcn = parentName + "." + name
  val log = log4j.Logger.getLogger(fqcn)

  @inline def tryLog(level : Level, message: => String) {
    if(log.isEnabledFor(level)) {
      log.log(fqcn, level, message, null)
    }
  }

  @inline def tryLog(level : Level, message: => String, cause : Throwable) {
    if(log.isEnabledFor(level)) {
      log.log(fqcn, level, message, cause)
    }
  }

  @inline def trace(message: => String) = tryLog(TRACE, message)

  @inline def debug(message: => String) = tryLog(DEBUG, message)

  @inline def info(message: => String) = tryLog(INFO, message)

  @inline def warn(message: => String) = tryLog(WARN, message)

  @inline def warn(cause: Throwable) = tryLog(WARN, cause.getMessage, cause)

  @inline def warn(message: => String, cause: Throwable) = tryLog(WARN, message, cause)

  @inline def error(message: => String) = tryLog(ERROR, message)

  @inline def error(cause: Throwable) = tryLog(ERROR, cause.getMessage, cause)

  @inline def error(message: => String, cause: Throwable) = tryLog(ERROR, message, cause)

  @inline def fatal(message: => String) = tryLog(FATAL,message)

  @inline def fatal(cause: Throwable) = tryLog(FATAL, cause.getMessage, cause)

  @inline def fatal(message: => String, cause: Throwable) = tryLog(FATAL, message, cause)
}
