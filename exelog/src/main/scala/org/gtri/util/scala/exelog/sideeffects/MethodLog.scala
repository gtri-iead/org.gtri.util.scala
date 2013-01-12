package org.gtri.util.scala.exelog.sideeffects

import org.gtri.util.scala.exelog
import org.apache.log4j.Level

/**
 * Created with IntelliJ IDEA.
 * User: Lance
 * Date: 1/9/13
 * Time: 7:01 PM
 * To change this template use File | Settings | File Templates.
 */
class MethodLog(_name : String, args: => Seq[(String,Any)])(implicit classLog : ClassLog) extends exelog.MethodLog(_name) {
  classLog trace { name + formatArgs(args) }

  def isEnabledFor(level: Level) = classLog.isEnabledFor(level)

  def log(fqcn: String, level: Level, message: => String, cause: Option[Throwable]) = classLog.log(fqcn, level, message, cause)
}

object MethodLog {
  def apply(name: String, args : => Seq[(String,Any)])(implicit classLog : ClassLog) = new MethodLog(name, args)
}
