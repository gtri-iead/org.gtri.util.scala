package org.gtri.util.scala.xsdbuilder

import org.gtri.util.xsddatatypes.XsdQName
import org.gtri.util.xsddatatypes.XsdCodes.AllOrNoneCode
import org.gtri.util.scala.statemachine._
import org.gtri.util.scala.xmlbuilder.XmlElement

object XmlParser {
  // TODO: move me to a common library
  type Try[A] = Either[java.lang.Throwable, A]
  object Try {
    def apply[A](a: => A) : Try[A] =
      try {
        Right(a)
      } catch {
        case t : Throwable => Left(t)
      }
    def parser[A,B](f: A => B) : Parser[A,B] = { a =>
      import Parser._
      Try(f(a)) match {
        case Right(b) => Succeed(b)
        case Left(throwable) => Halt.fatal(throwable.getMessage, Some(throwable))
      }
    }
    def parser[A,B](f: A => B, default: => B) : Parser[A,B] = { a =>
      import Parser._
      Try(f(a)) match {
        case Right(b) => Succeed(b)
        case Left(throwable) => Halt.error(throwable.getMessage, Some(throwable), () => Succeed(default))
      }
    }
  }


  def requiredAttributeParser[U](attrQName : XsdQName, simpleTypeParser: Parser[String,U], optValGen: Option[() => U]) : Parser[XmlElement, U] = { (element : XmlElement) =>
    import Parser._
    def buildHalt(message : String, cause : Option[Throwable] = None) : Transition[U] = {
      optValGen match {
        case Some(gen) =>
          val recoverF = { () => {
            val value = gen()
            Succeed(
              value = value,
              metadata = Issue.warn("Set required attribute to a generated value " + attrQName.toString + "='" + value.toString + "'") :: Nil
            )
          }}
          Halt.error(message,cause,recoverF)
        case None => Halt.fatal(message,cause)
      }
    }

    element.attributesMap.mapValues(attrStrValue => {
      val t0 = simpleTypeParser(attrStrValue)
      t0.state match {
        case q : State.Success[U] => Succeed(q.value,t0.metadata)
          // TODO: better error message here
        case q : State.Halted[U] => buildHalt("Invalid attribute value " + attrQName + "='" + attrStrValue + "'. " + q.toString)
      }
    }
    ).getOrElse(
      key = attrQName,
      default = buildHalt("Missing required attribute " + attrQName)
    )
  }

  def optionalAttributeParser[U](attrQName : XsdQName, simpleTypeParser: Parser[String,U]) : Parser[XmlElement, Option[U]] = { element =>
    import Parser._
    def buildHalt(message : String, cause : Option[Throwable] = None) : Transition[Option[U]] = {
      val recoverF = { () => {
        Succeed[Option[U]](
          value = None,
          metadata = Issue.warn("Unset optional attribute " + attrQName.toString) :: Nil
        )
      }}
      Halt.error(message,cause,recoverF)
    }

    element.attributesMap.mapValues[Transition[Option[U]]](attrStrValue => {
        val t0 = simpleTypeParser(attrStrValue)
        t0.state match {
          case q : State.Success[U] => Succeed(Some(q.value),t0.metadata)
          case q : State.Halted[U] => buildHalt("Invalid attribute value " + attrQName + "='" + attrStrValue + "'")
        }
      }
    ).getOrElse(
      key = attrQName,
      default = Succeed(None)
    )
  }

//  def requiredAttributeParser[U](attrQName : XsdQName, simpleTypeParser: String => Try[U], optValGen: Option[() => U]) : Parser[XmlElement, U] = { element =>
//    import Parser._
//    def buildHalt(message : String, cause : Option[Throwable] = None) : Transition[U] = {
//      optValGen match {
//        case Some(gen) =>
//          val recoverF = { () => {
//            val value = gen()
//            Succeed(
//              value = value,
//              metadata = Issue.warn("Set required attribute to a generated value " + attrQName.toString + "='" + value.toString + "'") :: Nil
//            )
//          }}
//          Halt.error(message,cause,recoverF)
//        case None => Halt.fatal(message,cause)
//      }
//    }
//
//    element.toAttributesMap.mapValues(attrStrValue =>
//      simpleTypeParser(attrStrValue) match {
//        case Right(value) => Succeed(value)
//        case Left(exception) => buildHalt("Invalid attribute value " + attrQName + "='" + attrStrValue + "'", Some(exception))
//      }
//    ).getOrElse(
//      key = attrQName,
//      default = buildHalt("Missing required attribute " + attrQName)
//    )
//  }
//
//  def optionalAttributeParser[U](attrQName : XsdQName, simpleTypeParser: String => Try[U]) : Parser[XmlElement, Option[U]] = { element =>
//    import Parser._
//    def buildHalt(message : String, cause : Option[Throwable] = None) : Transition[Option[U]] = {
//      val recoverF = { () => {
//        Succeed[Option[U]](
//          value = None,
//          metadata = Issue.warn("Unset optional attribute " + attrQName.toString) :: Nil
//        )
//      }}
//      Halt.error(message,cause,recoverF)
//    }
//
//    element.toAttributesMap.mapValues[Transition[Option[U]]](attrStrValue =>
//      simpleTypeParser(attrStrValue) match {
//        case Right(value) => Succeed(Some(value))
//        case Left(exception) => buildHalt("Invalid attribute value " + attrQName + "='" + attrStrValue + "'",Some(exception))
//      }
//    ).getOrElse(
//      key = attrQName,
//      default = buildHalt("Missing required attribute " + attrQName)
//    )
//  }
}
