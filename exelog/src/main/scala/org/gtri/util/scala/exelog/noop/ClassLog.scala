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
package org.gtri.util.scala.exelog.noop

import org.apache.log4j.{Level, Logger}
import org.gtri.util.scala.exelog
import exelog.ExeLog

/**
 * Created with IntelliJ IDEA.
 * User: Lance
 * Date: 1/8/13
 * Time: 5:53 PM
 * To change this template use File | Settings | File Templates.
 */
class ClassLog(val name : String, val qualifiedName : String) extends exelog.ClassLog {

  def isEnabledFor(level: Level) = false

  def log(fqcn: String, level: Level, message: => String, cause: Option[Throwable]) = { } // noop
}

object ClassLog {
  def apply(c: Class[_ <: AnyRef]) : ClassLog = apply(c.getSimpleName, c.getCanonicalName)
  def apply(name : String) : ClassLog = apply(name,name)
  def apply(name : String, qualifiedName : String) : ClassLog = new ClassLog(name, qualifiedName)
}
