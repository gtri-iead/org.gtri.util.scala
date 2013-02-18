package org.gtri.util.scala.statemachine

import org.scalatest.FunSpec
import org.gtri.util.scala.statemachine._
import scala.util.Random
import scala.collection.immutable.Seq

class ParserTest extends FunSpec {
  val rnd = Stream.continually(Random.nextInt(100))

//  object AllOrNoneCode extends Enumeration {
//    type AllOrNoneCode = Value
//    val NONE, ALL= Value
//  }
//  import AllOrNoneCode._
//
//  case class AllOrNone[A](value: Either[AllOrNoneCode, A])
//
//  def parser[A](subparser: String => Plan.State.Done[A]) : String => Plan.State.Done[AllOrNone[A]] = {
//    case s : String if s == "" => Plan.State.Success(AllOrNone(Left(AllOrNoneCode.NONE)))
//    case s : String if s == "#all" => Plan.State.Success(AllOrNone(Left(AllOrNoneCode.ALL)))
//    case s : String =>
//      val a : Array[String] = s.split("\\s+")
//      val d : Plan.State[A] = Plan.State.Halted(Issue.warn("")::Nil)
//      a.foldLeft(d) { (b,z) =>
//        b.flatMap()
//      }
//  }
  describe("A Parser[A]") {
    it("should composable") {
      def parser1(s : String) : Parser.Transition[Int] = {
        try {
          Parser.Succeed(s.toInt)
        } catch {
          case e : Exception => Parser.Halt.fatal(e.getMessage, Some(e))
        }
      }
      val i : Parser.Transition[Int] =
        for {
          v1 <- parser1("1")
          v2 <- parser1("2")
        } yield v1 + v2
      assert(i == Parser.Succeed(3))
    }
    it("should result in halted composable") {
      def parser1(s : String) : Parser.Transition[Int] = {
        try {
          Parser.Succeed(s.toInt)
        } catch {
          case e : Exception => Parser.Halt.error(e.getMessage,Some(e),{ () =>
            Parser.Succeed(100)
          })
        }
      }
      val i : Parser.Transition[Int] =
        for {
          v1 <- parser1("1")
          v2 <- parser1("asdf")
          v3 <- parser1("2")
        } yield v1 + v2 + v3
      assert(i.state.isHalted)
      val i2 : Parser.State.Halted[Int] = i.state.asInstanceOf[Parser.State.Halted[Int]]
      assert(i2.optRecover.isDefined)
      val s1 : Parser.Transition[Int] = i2.optRecover.get()
      assert(s1 == Parser.Succeed(103))
    }
  }

}
