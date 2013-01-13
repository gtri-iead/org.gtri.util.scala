package org.gtri.util.scala.exelog

/**
 * Created with IntelliJ IDEA.
 * User: Lance
 * Date: 1/13/13
 * Time: 5:36 AM
 * To change this template use File | Settings | File Templates.
 */
trait Log {
  def name : String

  def info(message: => String)
  def debug(message : => String)

  def trace(message : => String)

  def warn(message : => String)
  def warn(cause : Throwable)
  def warn(message : => String, cause : Throwable)

  def error(message : => String)
  def error(cause : Throwable)
  def error(message : => String, cause : Throwable)

  def fatal(message : => String)
  def fatal(cause : Throwable)
  def fatal(message : => String, cause : Throwable)
}
