package org.gtri.util.scala.xsdbuilder

import org.gtri.util.scala.statemachine._
import org.gtri.util.xsddatatypes.XsdCodes.AllOrNoneCode
import scala.collection.immutable.Seq

case class XsdAllOrNone[A](value : Either[AllOrNoneCode, Set[A]]) {
  override def toString = {
    value fold(
      fa = { allOrNoneCode => allOrNoneCode.toString },
      fb = { set => set.mkString(" ")}
    )
  }
}
object XsdAllOrNone {
  def parser[A](subparser: Parser[String,A]) : Parser[String,XsdAllOrNone[A]] = {
    case s : String if s == AllOrNoneCode.NONE.toString => Parser.Succeed(XsdAllOrNone(Left(AllOrNoneCode.NONE)))
    case s : String if s == AllOrNoneCode.ALL.toString => Parser.Succeed(XsdAllOrNone(Left(AllOrNoneCode.ALL)))
    case s : String =>
      val r0 : Seq[Parser.Transition[A]] =
        for {
          member <- s.split("\\s+").distinct.toList
        } yield subparser(member)
      val r1 : Parser.Transition[Seq[A]] = r0.sequence
      r1.flatMap { xs => Parser.Succeed(XsdAllOrNone(Right(xs.toSet))) }
  }
}
