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

import org.apache.log4j.{Level, Logger}
import org.apache.log4j.Level._

/**
 * Created with IntelliJ IDEA.
 * User: Lance
 * Date: 1/5/13
 * Time: 7:23 AM
 * To change this template use File | Settings | File Templates.
 */
trait ExeLog {
  def logger : Logger
  def qualifiedName : String
  def name : String

  def formatMessage(message : String) : String
  
  def info(message: => String) {
    if(logger.isEnabledFor(Level.INFO)) {
      logger.log(qualifiedName, Level.INFO, formatMessage(message), null)
    }
  }

  def debug(message : => String) {
    if(logger.isEnabledFor(Level.DEBUG)) {
      logger.log(qualifiedName, Level.DEBUG, formatMessage(message), null)
    }
  }

  def trace(message : => String) {
    if(logger.isEnabledFor(Level.TRACE)) {
      logger.log(qualifiedName, Level.TRACE, formatMessage(message), null)
    }
  }

  def warn(message : => String) {
    if(logger.isEnabledFor(Level.WARN)) {
      logger.log(qualifiedName, Level.WARN, formatMessage(message), null)
    }
  }

  def warn(throwable : Throwable) {
    if(logger.isEnabledFor(Level.WARN)) {
      logger.log(qualifiedName, Level.WARN, throwable.getMessage, throwable)
    }
  }

  def warn(message : => String, throwable : Throwable) {
    if(logger.isEnabledFor(Level.WARN)) {
      logger.log(qualifiedName, Level.WARN, formatMessage(message), throwable)
    }
  }

  def fatal(message : => String) {
    if(logger.isEnabledFor(Level.FATAL)) {
      logger.log(qualifiedName, Level.FATAL, formatMessage(message), null)
    }
  }

  def fatal(throwable : Throwable) {
    if(logger.isEnabledFor(Level.FATAL)) {
      logger.log(qualifiedName, Level.FATAL, throwable.getMessage, throwable)
    }
  }

  def fatal(message : => String, throwable : Throwable) {
    if(logger.isEnabledFor(Level.FATAL)) {
      logger.log(qualifiedName, Level.FATAL, formatMessage(message), throwable)
    }
  }

}

object ExeLog {
  def buildParameterString(parameters: Seq[(String,Any)]) : String = {
    val f : ((String,Any)) => String = {
      (tuple) =>
        val (name,value) = tuple
        val sval = value.toString
        s"$name='$sval'"
    }
    val s : Seq[String] = parameters map f
    s.mkString(",")
  }
}

class ClassLog(a : Any) extends ExeLog {
  val logger = Logger.getLogger(a.getClass)
  def name : String = a.getClass.getSimpleName
  def qualifiedName = a.getClass.getCanonicalName
  def formatMessage(message : String) : String = message
}

object ClassLog {

  def apply(a : Any, parameters: (String, Any)*) : ClassLog = {
    val log = new ClassLog(a)
    val name = log.name
    val sparameters = ExeLog.buildParameterString(parameters)
    log.trace(s"$name($sparameters)")
    log
  }

}

class MethodLog(val name : String)(implicit classLog : ClassLog) extends ExeLog {
  def logger = classLog.logger
  lazy val qualifiedName =  classLog.qualifiedName + "." + name

  def formatMessage(message : String) : String = {
    val className = classLog.name
    s"$className.$name: $message"
  }

  def exit[A](a : A) : A = {
    val className = classLog.name
    val s = a.toString
    classLog.trace({ s"$className.$name => '$s'" })
    a
  }
}

object MethodLog {
  def apply(name: String, parameters: (String,Any)*)(implicit classLog : ClassLog) : MethodLog = {
    classLog.trace({
      val className = classLog.name
      val sparameters = ExeLog.buildParameterString(parameters)
      s"$className.$name($sparameters)"
    })
    new MethodLog(name)
  }
}