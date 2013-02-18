package org.gtri.util.scala.xsdbuilder

import org.gtri.util.scala.statemachine._
import org.gtri.util.xsddatatypes.XsdCodes.AllOrNoneCode

case class XsdAllOrNone[+A](value : Either[AllOrNoneCode, Set[A]]) {
  override def toString = {
    value fold(
      fa = { allOrNoneCode => allOrNoneCode.toString },
      fb = { set => set.mkString(",")}
    )
  }
}
object XsdAllOrNone {
//  def parser[A](subparser: String => Iteratee.State.Done[String,A]) = new Iteratee[String,XsdAllOrNone[A]] {
//    import Iteratee._
//    def s0 = new State.Continuation[String,XsdAllOrNone[A]] {
//      def apply(x: String) =
//        x match {
//          case s : String if s == AllOrNoneCode.NONE.toString => Succeed(XsdAllOrNone(Left(AllOrNoneCode.NONE)))
//          case s : String if s == AllOrNoneCode.ALL.toString => Succeed(XsdAllOrNone(Left(AllOrNoneCode.ALL)))
//          case s : String =>
//            val members =
//              for(member <- s.split("\\s+"))
//              yield subparser(member)
//            Right(members.toSet)
//        }
//
//      def apply(x: EndOfInput) = ???
//    }
//  }

  def parser[A](subparser: String => Iteratee.State.Done[String,A]) : String => Iteratee.State.Done[String,XsdAllOrNone[A]] = {
    case s : String if s == AllOrNoneCode.NONE.toString => Iteratee.State.Success(XsdAllOrNone(Left(AllOrNoneCode.NONE)))
    case s : String if s == AllOrNoneCode.ALL.toString => Iteratee.State.Success(XsdAllOrNone(Left(AllOrNoneCode.ALL)))
    case s : String =>
      val members : Seq[A] =
        for {
          member <- s.split("\\s+")
          value <- subparser(member)
        } yield value


      Right(members.toSet)
  }
}
