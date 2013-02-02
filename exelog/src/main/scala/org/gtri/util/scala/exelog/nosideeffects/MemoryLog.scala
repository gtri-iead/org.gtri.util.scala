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
package org.gtri.util.scala.exelog.nosideeffects

import org.gtri.util.scala.exelog._
import org.gtri.util.scala.exelog.LogLevel._

final class MemoryLog(
  name : String,
  parentName : String,
  var log : List[LogRecord] = Nil
) {
  val fqcn = parentName + "." + name

  @inline def log(level : LogLevel, message : String) = {
    log ::= LogRecord(
      level               =   level,
      loggerName          =   fqcn,
      message             =   message
    )
  }

  @inline def log(level : LogLevel, message : String, cause : Throwable) = {
    log ::= LogRecord(
      level               =   level,
      loggerName          =   fqcn,
      message             =   message,
      optThrown           =   Some(cause)
    )
  }

  @inline def trace(message: => String) = log(TRACE, message)

  @inline def debug(message: => String) = log(DEBUG, message)

  @inline def info(message: => String) = log(INFO, message)

  @inline def warn(message: => String) = log(WARN, message)

  @inline def warn(cause: Throwable) = log(WARN, cause.getMessage, cause)

  @inline def warn(message: => String, cause: Throwable) = log(WARN, message, cause)

  @inline def error(message: => String) = log(ERROR, message)

  @inline def error(cause: Throwable) = log(ERROR, cause.getMessage, cause)

  @inline def error(message: => String, cause: Throwable) = log(ERROR, message, cause)

  @inline def fatal(message: => String) = log(FATAL,message)

  @inline def fatal(cause: Throwable) = log(FATAL, cause.getMessage, cause)

  @inline def fatal(message: => String, cause: Throwable) = log(FATAL, message, cause)

  @inline def log(r: LogRecord) {
      r.level match {
        case LogLevel.TRACE => trace(r.message)
        case LogLevel.DEBUG => debug(r.message)
        case LogLevel.INFO => info(r.message)
        case LogLevel.WARN =>
          if(r.optThrown.isDefined) {
            warn(r.message, r.optThrown.get)
          } else {
            warn(r.message)
          }
        case LogLevel.ERROR =>
          if(r.optThrown.isDefined) {
            error(r.message, r.optThrown.get)
          } else {
            error(r.message)
          }
        case LogLevel.FATAL =>
          if(r.optThrown.isDefined) {
            fatal(r.message, r.optThrown.get)
          } else {
            fatal(r.message)
          }
        case _ => // noop
      }
  }

}
