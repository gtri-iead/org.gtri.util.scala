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
import org.gtri.util.scala.exelog

/**
 * Created with IntelliJ IDEA.
 * User: Lance
 * Date: 1/13/13
 * Time: 6:26 AM
 * To change this template use File | Settings | File Templates.
 */
class Log4jLogger extends exelog.Logger {
  def getLog(c: Class[_]) = Log4jLog(c.getSimpleName, c.getPackage.getName)

  def getLog(name: String, parentName : String) = Log4jLog(name, parentName)
}
