package org.gtri.util.scala.exelog

import org.scalatest.FunSuite
import org.gtri.util.scala.exelog._
import org.apache.log4j.{PatternLayout, ConsoleAppender, Level, Logger}
import org.apache.log4j.Level._

/**
 * Created with IntelliJ IDEA.
 * User: Lance
 * Date: 1/6/13
 * Time: 9:20 PM
 * To change this template use File | Settings | File Templates.
 */

class A(s: String, f: Float) {
  implicit val classlog = enter(this, "s" -> s, "f" -> f)
  def foo(s: String, i: Int) : Int = {
    implicit val log = enter("foo","s" -> s,"i" -> i)
    ***("Debug message")
    ****("Trace message")
    1 ***> log
  }
}


class ExeLogTests extends FunSuite {


  test("exelog") {
    val rootLogger = Logger.getRootLogger();
    rootLogger.setLevel(Level.INFO);
    rootLogger.addAppender(new ConsoleAppender(
      new PatternLayout("%-6r [%p] %c - %m%n")));

    rootLogger.setLevel(Level.ALL)
    val a = new A("asdf",1.0f)
    a.foo("qwety",2)
  }
}
