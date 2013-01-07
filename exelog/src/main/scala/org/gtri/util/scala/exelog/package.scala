package org.gtri.util.scala

/**
 * Created with IntelliJ IDEA.
 * User: Lance
 * Date: 1/6/13
 * Time: 9:45 PM
 * To change this template use File | Settings | File Templates.
 */
package object exelog {

  def ***(message : => String)(implicit log : MethodLog) = log.debug(message)
  def ****(message : => String)(implicit log : MethodLog) = log.trace(message)

  def enter(a : Any, parameters: (String, Any)*) : ClassLog = ClassLog(a, parameters:_*)
  def enter(name: String, parameters: (String,Any)*)(implicit classLog : ClassLog) : MethodLog = MethodLog(name, parameters:_*)
  def exit[A](a: => A)(implicit log : MethodLog) = log.exit(a)

  implicit class returnMe[A](a :A) {
    def ***>(log : MethodLog) : A = log.exit(a)
  }

}
