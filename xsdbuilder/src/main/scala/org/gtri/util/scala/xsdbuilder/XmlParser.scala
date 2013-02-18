package org.gtri.util.scala.xsdbuilder

import org.gtri.util.xsddatatypes.XsdQName
import org.gtri.util.xsddatatypes.XsdCodes.AllOrNoneCode
import org.gtri.util.scala.statemachine._
import org.gtri.util.scala.xmlbuilder.XmlElement

object XmlParser {

  case class QNamePeekParser(qName : XsdQName) extends Iteratee[XmlElement,XmlElement] {
    import Iteratee._
    case class Cont() extends State.Continuation[XmlElement, XmlElement] {
      def apply(x : XmlElement) = {
        if(x.qName == qName) {
          Succeed(
            value = x,
            overflow = x :: Nil
          )
        } else {
          Halt.fatal("")
        }
      }
      def apply(eoi : EndOfInput) = Halt.fatal("")
    }
    def s0 = Cont()
  }

  case class RequiredAttributePeekParser[U](qName : XsdQName, parser: String => U, optValidValue: Option[() => U] = None) extends Iteratee[XmlElement,U] {
    import Iteratee._

    case class Cont() extends State.Continuation[XmlElement,U] {

      def halt(message : String, cause : Option[Throwable]) : Transition[XmlElement,U] = {
        optValidValue map { validValue =>
          val recoverF = () => {
            val value = validValue()
            Succeed(
              value = value,
              metadata = Issue.warn("Set required attribute to a valid value " + qName.toString + "='" + value.toString + "'") :: Nil
            )
          }
          Halt.error(message,cause,recoverF)
        } getOrElse {
          Halt.fatal(message,cause)
        }
      }

      def apply(element : XmlElement) = {
        // Is the attribute set?
        if(element.attributesMap.contains(qName)) {
          // Attribute is set - try to parser it
          try {
            // Return the parsed value AND overflow the input to allow flatMap/map chaining
            Succeed(
              value = parser(element.attributesMap(qName)),
              overflow = element :: Nil
            )
          } catch {
            case e : Exception =>
            halt(e.getMessage,Some(e))
          }
        } else {
          // Attribute is not set
          halt("Missing required attribute " + qName, None)
        }

      }
      def apply(x : EOI) = Halt.fatal("Missing input")
    }

    def s0 = Cont()
  }

  case class OptionalAttributePeekParser[U](qName : XsdQName, parser: String => U) extends Iteratee[XmlElement,Option[U]] {
    import Iteratee._

    case class Cont() extends State.Continuation[XmlElement,Option[U]] {
      def apply(element : XmlElement) = {
        // Attribute set?
        if(element.attributesMap.contains(qName)) {
          // Attribute is set - try to downcast it
          try {
            // Return the parsed value AND overflow the input to allow flatMap/map chaining
            Succeed(
              value = Some(parser(element.attributesMap(qName))),
              overflow = element :: Nil
            )
          } catch {
            case e : Exception =>
              Halt.error(
                message = e.getMessage,
                cause = Some(e),
                recover = () => Succeed(
                  value = None,
                  metadata = Issue.warn("Ignorning optional attribute with invalid value " + qName.toString) :: Nil)
              )
          }
        } else {
          // Attribute is missing, return an unset value (success)
          Succeed(
            value = None,
            overflow = element :: Nil
          )
        }

      }
      def apply(x : EOI) = Halt.fatal("Missing input")
    }

    def s0 = Cont()
  }

  def parseAllOrNone[A](parser: String => A)(s : String) : Either[AllOrNoneCode, Set[A]] = {
    s match {
      case s : String if s == AllOrNoneCode.NONE.toString => Left(AllOrNoneCode.NONE)
      case s : String if s == AllOrNoneCode.ALL.toString => Left(AllOrNoneCode.ALL)
      case _ =>
        val members =
          for(member <- s.split("\\s+"))
          yield parser(member)
        Right(members.toSet)
    }
  }

}
