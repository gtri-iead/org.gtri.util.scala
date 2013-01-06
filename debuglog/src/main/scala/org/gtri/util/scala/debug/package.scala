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

import debug._
import org.apache.log4j.Level

/**
 * Created with IntelliJ IDEA.
 * User: Lance
 * Date: 1/5/13
 * Time: 7:23 AM
 * To change this template use File | Settings | File Templates.
 */
package object debug {

  def **(message : => String) = info(message)
  def ***(message : => String) = debug(message)
  def ****(message : => String) = trace(message)

  private def buildParameterString(parameters: Seq[(String,Any)]) : String = {
    val f : ((String,Any)) => String = {
      (tuple) =>
        val (name,value) = tuple
        val sval = value.toString
        s"$name='$sval'"
    }
    val s : Seq[String] = parameters map f
    s.mkString(",")
  }

  def enter[A](c : Class[A], parameters: (String, Any)*) : ClassLog[A] = {
    val name = c.getName
    val sparameters = buildParameterString(parameters)
    trace(s"CLASS $name($sparameters)")
    ClassLog(name)
  }

  def enter(name: String, packageName: String, parameters: (String,Any)*)(implicit classScope : ClassLog) : MethodLog = {
    trace({
      val sparameters = buildParameterString(parameters)
      s"METHOD $name($sparameters)"
    })
    MethodLog(name)
  }

  def exit(value: => String = "")(implicit scope : DebugLog) {
    val name = scope.name
    trace({ s"EXIT $name='$value'" })
  }

  def info(message: => String)(implicit scope : DebugLog) {
    if(scope.logger.isEnabledFor(Level.INFO)) {
      scope.logger.log(scope.qualifiedName, Level.INFO, message, null)
    }
  }

  def debug(message : => String)(implicit scope : DebugLog) {
    if(scope.logger.isEnabledFor(Level.DEBUG)) {
      scope.logger.log(scope.qualifiedName, Level.DEBUG, message, null)
    }
  }

  def trace(message : => String)(implicit scope : DebugLog) {
    if(scope.logger.isEnabledFor(Level.TRACE)) {
      scope.logger.log(scope.qualifiedName, Level.TRACE, message, null)
    }
  }

  def warn(message : => String)(implicit scope : DebugLog) {
    if(scope.logger.isEnabledFor(Level.WARN)) {
      scope.logger.log(scope.qualifiedName, Level.WARN, message, null)
    }
  }

  def warn(throwable : => Throwable)(implicit scope : DebugLog) {
    if(scope.logger.isEnabledFor(Level.WARN)) {
      scope.logger.log(scope.qualifiedName, Level.WARN, throwable.getMessage, throwable)
    }
  }

  def warn(message : => String, throwable : => Throwable)(implicit scope : DebugLog) {
    if(scope.logger.isEnabledFor(Level.WARN)) {
      scope.logger.log(scope.qualifiedName, Level.WARN, message, throwable)
    }
  }

  def fatal(message : => String)(implicit scope : DebugLog) {
    if(scope.logger.isEnabledFor(Level.FATAL)) {
      scope.logger.log(scope.qualifiedName, Level.FATAL, message, null)
    }
  }

  def fatal(throwable : => Throwable)(implicit scope : DebugLog) {
    if(scope.logger.isEnabledFor(Level.FATAL)) {
      scope.logger.log(scope.qualifiedName, Level.FATAL, throwable.getMessage, throwable)
    }
  }

  def fatal(message : => String, throwable : => Throwable)(implicit scope : DebugLog) {
    if(scope.logger.isEnabledFor(Level.FATAL)) {
      scope.logger.log(scope.qualifiedName, Level.FATAL, message, throwable)
    }
  }

  implicit class returnMe[A](a :A) {
    def <**(log : MethodLog) : A = {
      implicit val _log = log
      exit(a.toString)
    }
  }

}
