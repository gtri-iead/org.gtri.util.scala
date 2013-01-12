package org.gtri.util.scala.exelog.sideeffects

import org.scalatest.FunSuite
import org.gtri.util.scala.exelog.sideeffects._
import org.apache.log4j.{PatternLayout, ConsoleAppender, Level, Logger}
import org.apache.log4j.Level._

/**
* Created with IntelliJ IDEA.
* User: Lance
* Date: 1/6/13
* Time: 9:20 PM
* To change this template use File | Settings | File Templates.
*/

object A {
  implicit val classLog = ClassLog(classOf[A])

  def apply(s: String, f: Float) = {
    implicit val log = enter("apply") { "s" -> s :: "f" -> f :: Nil }
    val a = new A(s,f)
    exit(a)
  }
}
class A(s: String, f: Float) {
  import A._
  {
    implicit val log = init { "s" -> s :: "f" -> f :: Nil }
    exit(this)
  }
  def foo(s: String, i: Int) : Int = {
    implicit val log = enter("foo") { "s" -> s :: "i" -> i :: Nil }
    +"debug message" // log debug "debug message"
    ~"trace message" // log trace "trace message"
    log info "info message"
    log += "debug message"
    log error("message",new RuntimeException)
    log fatal new IllegalStateException
    1 <~: log
  }
}

object B {
  implicit val classLog = ClassLog(classOf[B])
}
class B() {
  import B._
  init()
  def foo() : Int = {
    implicit val log = enter("foo")()
    +"debug message"
    ~"trace message"
    log :~> 1
    1 <~: log
    exit(1)
  }
}


class SideEffectsExeLogTests extends FunSuite {
  val rootLogger = Logger.getRootLogger();
  rootLogger.setLevel(Level.INFO);
  rootLogger.addAppender(new ConsoleAppender(
    new PatternLayout("%-6r [%p] %c - %m%n")));

  rootLogger.setLevel(Level.ALL)


  test("exelog") {
    val a = A("asdf",1.0f)
    a.foo("qwety\nasdf",2)
    val b = new B()
    b.foo()
  }
}
