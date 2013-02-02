/*
    Copyright 2012 Georgia Tech Research Institute

    Author: lance.gatlin@gtri.gatech.edu

    This file is part of org.gtri.util.scala.statemachine library.

    org.gtri.util.scala.statemachine library is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    org.gtri.util.scala.statemachine library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with org.gtri.util.scala.statemachine library. If not, see <http://www.gnu.org/licenses/>.

*/
package org.gtri.util.scala.statemachine

import IssueSeverityCode._

case class Issue(
  severityCode    :   IssueSeverityCode,
  message         :   String,
  cause           :   Option[Throwable] = None
)

object Issue {
  def warn(message : String, cause : Option[Throwable] = None) = Issue(WARN, message, cause)
  def error(message : String, cause : Option[Throwable] = None) = Issue(ERROR, message, cause)
  def fatal(message : String, cause : Option[Throwable] = None) = Issue(FATAL, message, cause)

//  implicit object IssueOrdering extends Ordering[Issue] {
//    def compare(x: Issue, y: Issue) = x.severityCode compare y.severityCode
//  }
}