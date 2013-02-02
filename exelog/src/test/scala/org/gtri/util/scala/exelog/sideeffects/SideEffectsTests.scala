package org.gtri.util.scala.exelog.sideeffects

import org.scalatest.FunSuite
import org.gtri.util.scala.exelog.sideeffects._
import org.gtri.util.scala.exelog._
import org.apache.log4j

object A {
  implicit val thisclass = classOf[A[_]]
  implicit val log = Logger.getLog(thisclass)

  def apply[T](s : String, f : T) : A[T] ={
    log.block("apply") {
      new A(s,f)
    }

  }
}
class A[T](s : String, f: T) {
  import A._
  def foo(s : String, f : T) : Int = {
    log.block("foo", Seq("s" -> s, "f" -> f)) {
      1
    }
  }
  def foo2(s : String, f : T) : Int = {
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
    val a = A[Float]("asdf",1.0f)
    a.foo("qwety\nasdf",2.0f)
    a.foo2("qwety\nasdf",2.0f)
  }
}