package org.gtri.util.scala.statemachine.test

case class LogEntry(level : String, fqcn : String, message : String, cause : Option[Throwable] = None) {
  override def toString : String = s"[$level] $fqcn: $message $cause"
}

class Log(fqcn : String) {
  def info(message: String) = LogEntry("INFO",fqcn,message,None)
  def warn(message: String, cause : Option[Throwable] = None) = LogEntry("WARN",fqcn,message,cause)
  def error(message: String, cause : Option[Throwable] = None) = LogEntry("ERROR",fqcn,message,cause)
  def fatal(message: String, cause : Option[Throwable] = None) = LogEntry("FATAL",fqcn,message,cause)
}
object Log {
  def apply(c: Class[_ <: AnyRef]) : Log = new Log(c.getCanonicalName)
}