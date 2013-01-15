package org.gtri.util.scala.exelog.sideeffects

import org.scalatest.FunSuite
import org.gtri.util.scala.exelog.sideffects._
import org.gtri.util.scala.exelog._
import org.apache.log4j

object A {
  implicit val thisclass = classOf[A]
  implicit val log = implicitly[Logger].getLog(thisclass)
  
  def apply(s : String, f : Float) : A ={
    log.block("apply") {
      new A(s,f)
    }

  }
}
class A(s : String, f: Float) {
  import A._
  def foo(s : String, f : Float) : Int = {
    log.block("foo", Seq("s" -> s, "f" -> f)) {
      1
    }
  }
  def foo2(s : String, f : Float) : Int = {
    log.begin("foo2", Seq("s" -> s, "f" -> f))
    log trace "asdf"
    log debug "asdf"
    log info "asdf"
    log warn "sadf"
    log warn new IllegalArgumentException
    log warn("asdf",new IllegalArgumentException)
    log error "sadf"
    log error new IllegalArgumentException
    log error("asdf",new IllegalArgumentException)
    log fatal "sadf"
    log fatal new IllegalArgumentException
    log fatal("asdf",new IllegalArgumentException)
    val retv = 1
    log.end("foo2",retv)
    retv
  }
}
class SideEffectsTests extends FunSuite {
  val rootLogger = log4j.Logger.getRootLogger();
  rootLogger.setLevel(log4j.Level.INFO);
  rootLogger.addAppender(new log4j.ConsoleAppender(
    new log4j.PatternLayout("%-6r [%p] %c - %m%n")));

  rootLogger.setLevel(log4j.Level.ALL)
  test("SideEffectsTests") {
    val a = A("asdf",1.0f)
    a.foo("qwety\nasdf",2)
    a.foo2("qwety\nasdf",2)
  }
}