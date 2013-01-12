package org.gtri.util.scala.exelog.noop

import org.gtri.util.scala.exelog
import org.apache.log4j.Level
import org.apache.log4j.Level._
import scala.Some

/**
 * Created with IntelliJ IDEA.
 * User: Lance
 * Date: 1/9/13
 * Time: 7:01 PM
 * To change this template use File | Settings | File Templates.
 */
class MethodLog(_name : String, args: => Seq[(String,Any)])(implicit classLog : ClassLog) extends exelog.MethodLog(_name) {
  def isEnabledFor(level: Level) = false

  override def log(fqcn: String, level: Level, message: => String, cause: Option[Throwable]) = { } // noop

  override def info(message: => String) = { } // noop

  override def +=(message: => String) = { } // noop
  override def debug(message : => String) = { } // noop

  override def ~=(message: => String) = { } // noop
  override def trace(message : => String) = { } // noop

  override def warn(message : => String) = { } // noop
  override def warn(cause : Throwable) = { } // noop
  override def warn(message : => String, cause : Throwable) = { } // noop

  override def error(message : => String) = { } // noop
  override def error(cause : Throwable) = { } // noop
  override def error(message : => String, cause : Throwable) = { } // noop

  override def fatal(message : => String) = { } // noop
  override def fatal(cause : Throwable) = { } // noop
  override def fatal(message : => String, cause : Throwable) = { } // noop

  override def exit[A](a : A) = { } // noop
}

object MethodLog {
  def apply(name: String, args : => Seq[(String,Any)])(implicit classLog : ClassLog) = new MethodLog(name, args)
}
