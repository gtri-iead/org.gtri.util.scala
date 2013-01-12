package org.gtri.util.scala.exelog.nosideeffects

import org.scalatest.FunSuite
import org.gtri.util.scala.exelog.nosideeffects._
import org.apache.log4j.{PatternLayout, ConsoleAppender, Level, Logger}

/**
* Created with IntelliJ IDEA.
* User: Lance
* Date: 1/6/13
* Time: 9:20 PM
* To change this template use File | Settings | File Templates.
*/

object E {
  implicit val classLog = ClassLog(classOf[E])

  def apply(s: String, f: Float) : (List[LogEntry], E) = {
    implicit val log = enter("apply") { "s" -> s :: "f" -> f :: Nil }
    val a = new E(s,f)
    exit(a)
    (log.entries, a)
  }
}

class E(s: String, f: Float) {
  import E._
  val classlog : List[LogEntry] =
  {
    implicit val log = init { "s" -> s :: "f" -> f :: Nil }
    exit(this)
    log.entries
  }
  def foo(s: String, i: Int) : (List[LogEntry], Int) = {
    implicit val log = enter("foo") { "s" -> s :: "i" -> i :: Nil }
    +"debug message"
    ~"trace message"
    log info "info message"
    log += "info message"
    log error("message",new RuntimeException)
    log fatal new IllegalStateException
    log exit 1
    (log.entries, 1)
  }
}

object F {
  implicit val classLog = ClassLog(classOf[F])
}

class F() {
  import F._
  val thisLog = init().entries
  def foo() : (List[LogEntry],Int) = {
    implicit val log = enter("foo")()
    +"debug message"
    ~"trace message"
    log :~> 1
    1 <~: log
    exit(1)
    (log.entries, 1)
  }
}


class NoSideEffectsExeLogTests extends FunSuite {
  val rootLogger = Logger.getRootLogger();
  rootLogger.setLevel(Level.INFO);
  rootLogger.addAppender(new ConsoleAppender(
//    new PatternLayout("%-6r [%p] %c - %m%n")));
  new PatternLayout("%d{ISO8601} %-5p [%t] %m%n [UTF-8]")));
  rootLogger.setLevel(Level.ALL)

  def dumpLogs(log : List[LogEntry], c : Class[_ <: AnyRef]) {
    val logger = Logger.getLogger(c)
    log.reverse map { entry =>
      logger.log(entry.fqcn, entry.level, entry.message, entry.cause.orNull)
    }
  }

  test("exelog") {
    val (log1,e) = E("asdf",1.0f)
    val (log2,result1) = e.foo("qwety\nasdf",2)
    val f = new F()
    val (log3,result2) = f.foo()
    dumpLogs(log2 ::: log1, classOf[E])
    dumpLogs(log3, classOf[F])
  }
}
