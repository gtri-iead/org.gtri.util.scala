package org.gtri.util.scala.exelog.nosideeffects

import org.gtri.util.scala.exelog
import org.apache.log4j.Level



class MethodLog(_name : String, args: => Seq[(String,Any)])(implicit classLog : ClassLog) extends exelog.MethodLog(_name) {
  private var _entries : List[LogEntry] = Nil
  def entries = _entries

  def isEnabledFor(level: Level) = classLog.isEnabledFor(level)

  def log(fqcn: String, level: Level, message: => String, cause: Option[Throwable]) = {
    _entries ::= LogEntry(fqcn, level, message, cause)
  }
}



object MethodLog {
  def apply(name: String, args : => Seq[(String,Any)])(implicit classLog : ClassLog) = {
    val log = new MethodLog(name, args)
    log trace { log.name + log.formatArgs(args) }
    log
  }
}
