package org.gtri.util.scala.exelog.nosideeffects

import org.apache.log4j.Level

/**
 * Created with IntelliJ IDEA.
 * User: Lance
 * Date: 1/9/13
 * Time: 7:43 PM
 * To change this template use File | Settings | File Templates.
 */
class LogEntry(val fqcn : String, val level : Level, _message : =>String, val cause: Option[Throwable]) {
  def message = _message
}
object LogEntry {
  def apply(fqcn : String, level : Level, message : =>String, cause: Option[Throwable]) = new LogEntry(fqcn, level, message, cause)
}
