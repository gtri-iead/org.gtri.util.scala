package org.gtri.util.scala.exelog

sealed trait LogLevel
object LogLevel {
  case object TRACE extends LogLevel
  case object INFO extends LogLevel
  case object DEBUG extends LogLevel
  case object WARN extends LogLevel
  case object ERROR extends LogLevel
  case object FATAL extends LogLevel
}

case class LogRecord(
  level                  :   LogLevel,
  loggerName             :   String,
  message                :   String,
  optSequenceNumber      :   Option[Long]        =   None,
  optMillis              :   Option[Long]        =   None,
  optParameters          :   Array[Any]          =   Array.empty,
  optResourceBundleName  :   Option[String]      =   None,
  optSourceClassName     :   Option[String]      =   None,
  optSourceMethodName    :   Option[String]      =   None,
  optThreadID            :   Option[Int]         =   None,
  optThrown              :   Option[Throwable]   =   None
)

