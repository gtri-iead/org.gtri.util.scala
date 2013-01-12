package org.gtri.util.scala.exelog.noop

import org.scalatest.FunSuite

/**
 * Created with IntelliJ IDEA.
 * User: Lance
 * Date: 1/6/13
 * Time: 9:20 PM
 * To change this template use File | Settings | File Templates.
 */

object C {
  implicit val classLog = ClassLog(classOf[C])

  def apply(s: String, f: Float) = {
    implicit val log = enter("apply") { "s" -> s :: "f" -> f :: Nil }
    val a = new C(s,f)
    exit(a)
  }
}
class C(s: String, f: Float) {
  import C._
  {
    implicit val log = init { "s" -> s :: "f" -> f :: Nil }
    exit(this)
  }
  def foo(s: String, i: Int) : Int = {
    implicit val log = enter("foo") { "s" -> s :: "i" -> i :: Nil }
    +"debug message"
    ~"trace message"
    log info "info message"
    log += "debug message"
    log error("message",new RuntimeException)
    log fatal new IllegalStateException
    1 <~: log
  }
}

object D {
  implicit val classLog = ClassLog(classOf[D])
}
class D() {
  import D._
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


class NoopExeLogTests extends FunSuite {
  test("exelog") {
    val a = C("asdf",1.0f)
    a.foo("qwety\nasdf",2)
    val b = new D()
    b.foo()
  }
}
