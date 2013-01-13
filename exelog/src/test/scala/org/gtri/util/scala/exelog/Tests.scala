package org.gtri.util.scala.exelog

import org.scalatest.FunSuite
import org.gtri.util.scala.exelog._
import org.apache.log4j


/* Logging style 1: Method logging (Recommended)
   Pro:
   -Caller controlled logging through implicit Logger parameter
   -Referential transparency
   Con:
   -Slower performance due to reacquiring a log in every method call
*/
object A {
  implicit val thisclass = classOf[A]

  def apply(s: String, f: Float)(implicit logger : Logger) = {
    implicit val log = enter("apply") { "s" -> s :: "f" -> f :: Nil }
    val a = new A(s,f)
    log :~> a
  }
}

class A(s: String, f: Float) {
  import A._
  def foo(s: String, i: Int)(implicit logger : Logger) : Int = {
    implicit val log = enter("foo") { "s" -> s :: "i" -> i :: Nil }
    +"debug message" // log debug "debug message"
    ~"trace message" // log trace "trace message"
    log info "info message"
    log error("message",new RuntimeException)
    log fatal new IllegalStateException
    1 <~: log // log the return value
  }
}


/*
  Logging style 2: Class logging with instance log
  Pro:
  -Slightly more performant than style 1
  Con:
  -Object creator controls logging (not method caller)
  -No referential transparency for methods

 */
object B {
  def apply(s: String, f: Float)(implicit logger : Logger) = {
    implicit val log = logger.getLog(classOf[B].getCanonicalName,"apply")
    val b = new B(s,f)
    log debug "debug message"
    log trace s"$b"
    b
  }
}

class B(s: String, f: Float)(implicit logger : Logger) {
  implicit val log = logger.getLog(this.getClass)
  def foo(s: String, i: Int) : Int = {
    log trace s"foo(s=$s,i=$i)"
    +"debug message" // log debug "debug message"
    ~"trace message" // log trace "trace message"
    log info "info message"
    log error("message",new RuntimeException)
    log fatal new IllegalStateException
    log trace "1"
    1
  }
}

/*
  Logging style 3: Class logging with global log
  Pro:
  -Most performant
  Con:
  -Logger can only be controlled through a per-class var
  -No referential transparency
 */
object C {

  @volatile implicit var log = NoopLog

  def apply(s: String, f: Float) = {
    val c = new C(s,f)
    log debug "debug message"
    log trace s"$c"
    c
  }
}

class C(s: String, f: Float) {
  import C._
  def foo(s: String, i: Int) : Int = {
    log trace s"foo(s=$s,i=$i)"
    +"debug message" // log debug "debug message"
    ~"trace message" // log trace "trace message"
    log info "info message"
    log error("message",new RuntimeException)
    log fatal new IllegalStateException
    log trace "1"
    1
  }
}

class Tests extends FunSuite {
  val rootLogger = log4j.Logger.getRootLogger();
  rootLogger.setLevel(log4j.Level.INFO);
  rootLogger.addAppender(new log4j.ConsoleAppender(
    new log4j.PatternLayout("%-6r [%p] %c - %m%n")));

  rootLogger.setLevel(log4j.Level.ALL)
  implicit val v = new Log4jLogger
  test("exelog") {
    val a = A("asdf",1.0f)
    a.foo("qwety\nasdf",2)
    val b = B("asdf",1.0f)
    b.foo("qwety\nasdf",2)
    val c = C("asdf",1.0f)
    c.foo("qwety\nasdf",2)
  }
}