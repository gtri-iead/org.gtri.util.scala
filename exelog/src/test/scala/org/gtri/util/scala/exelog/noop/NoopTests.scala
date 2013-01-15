package org.gtri.util.scala.exelog.noop

import org.scalatest.FunSuite
import org.gtri.util.scala.exelog.noop._
import org.gtri.util.scala.exelog._

object B {
  implicit val thisclass = classOf[B]
  implicit val log = implicitly[Logger].getLog(thisclass)

  def apply(s : String, f : Float) : B ={
    log.block("apply") {
      new B(s,f)
    }

  }
}
class B(s : String, f: Float) {
  import B._
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
class NoopTests extends FunSuite {
  test("exelog") {
    val b = B("asdf",1.0f)
    b.foo("qwety\nasdf",2)
    b.foo2("qwety\nasdf",2)
}
}