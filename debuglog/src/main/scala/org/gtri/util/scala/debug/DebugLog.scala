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
package org.gtri.util.scala.debug

import org.apache.log4j.{Level, Logger}
import org.apache.log4j.Level._

/**
 * Created with IntelliJ IDEA.
 * User: Lance
 * Date: 1/5/13
 * Time: 7:23 AM
 * To change this template use File | Settings | File Templates.
 */
trait DebugLog {
  def logger : Logger
  def qualifiedName : String
  def name : String
}
case class ClassLog[+A](c : Class[A]) {
  val logger = Logger.getLogger(c)
  def name : String = c.getName
  def qualifiedName = c.getCanonicalName
}
case class MethodLog(name : String)(implicit classScope : ClassLog) {
  def logger = classScope.logger
  lazy val qualifiedName =  classScope.qualifiedName + "." + name
}

//case class Log(val name : String, val parameters : String = "")(implicit logger : Logger) {
//  debug(s"CALL $name($parameters)")
//
//  def **>[A](a: A) : A = retn(a)
//  def retn[A](a: A) : A = {
//    val s = a.toString
//    debug(s"RETURN $name=$s")
//    a
//  }
//
//  def formatMessage(message: String) : String = s"[$name] $message"
//
//  def ***[A](a : A) : A = show(a)
//  def show[A](a : A) : A = {
//    debug({a.toString})
//    a
//  }
//
//  def trace(message: => String) {
//    if(logger.isEnabledFor(DEBUG)) {
//      logger.debug(formatMessage(message))
//    }
//  }
//
//  def debug(ex:Throwable) {
//    if(logger.isEnabledFor(DEBUG)) {
//      logger.debug(formatMessage(ex.toString), ex)
//    }
//  }
//
//  def debug(message: => String) {
//    if(logger.isEnabledFor(DEBUG)) {
//      logger.debug(formatMessage(message))
//    }
//  }
//
//  def debug(message: => String, ex:Throwable) {
//    if(logger.isEnabledFor(DEBUG)) {
//      logger.debug(formatMessage(message),ex)
//    }
//  }
//
//  def info(ex:Throwable) {
//    if(logger.isEnabledFor(INFO)) {
//      logger.info(formatMessage(ex.toString), ex)
//    }
//  }
//
//  def info(message: => String) {
//    if(logger.isEnabledFor(INFO)) {
//      logger.info(formatMessage(message))
//    }
//  }
//
//  def info(message: => String, ex:Throwable) {
//    if(logger.isEnabledFor(INFO)) {
//      logger.info(formatMessage(message),ex)
//    }
//  }
//
//  def warn(message: => String) {
//    if(logger.isEnabledFor(WARN)) {
//      logger.warn(formatMessage(message))
//    }
//  }
//
//  def warn(message: => String, ex:Throwable) {
//    if(logger.isEnabledFor(WARN)) {
//      logger.warn(formatMessage(message),ex)
//    }
//  }
//
//  def error(ex:Throwable) {
//    if(logger.isEnabledFor(ERROR)) {
//      logger.error(formatMessage(ex.toString),ex)
//    }
//  }
//
//  def error(message: => String) {
//    if(logger.isEnabledFor(ERROR)) {
//      logger.error(formatMessage(message))
//    }
//  }
//
//  def error(message: => String, ex:Throwable) {
//    if(logger.isEnabledFor(ERROR)) {
//      logger.error(formatMessage(message),ex)
//    }
//  }
//
//  def fatal(ex:Throwable) {
//    if(logger.isEnabledFor(FATAL)) {
//      logger.fatal(formatMessage(ex.toString),ex)
//    }
//  }
//
//  def fatal(message: => String) {
//    if(logger.isEnabledFor(FATAL)) {
//      logger.fatal(formatMessage(message))
//    }
//  }
//
//  def fatal(message: => String, ex:Throwable) {
//    if(logger.isEnabledFor(FATAL)) {
//      logger.fatal(formatMessage(message),ex)
//    }
//  }
//}
