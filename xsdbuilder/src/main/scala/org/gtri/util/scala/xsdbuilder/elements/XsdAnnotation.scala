package org.gtri.util.scala.xsdbuilder.elements

import org.gtri.util.scala.statemachine._
import org.gtri.util.xsddatatypes._
import org.gtri.util.xsddatatypes.XsdConstants._
import org.gtri.util.scala.xsdbuilder.XmlParser._
import org.gtri.util.scala.xmlbuilder.{XmlNamespaceContext, XmlElement}

final case class XsdAnnotation(
  optId        :   Option[XsdId]                 = None,
  optMetadata  :   Option[XsdElement.Metadata]   = None
) extends XsdElement {

  def optValue = None

  def util = XsdAnnotation.util

  def toAttributesMap(context: Seq[XmlNamespaceContext]) = {
    {
      optId.map { (ATTRIBUTES.ID.QNAME -> _.toString) } ::
      Nil
    }.flatten.toMap
  }
}

object XsdAnnotation {

  implicit object util extends XsdElementUtil[XsdAnnotation] {

    def qName = ELEMENTS.ANNOTATION.QNAME

    def parser[EE >: XsdAnnotation](context: Seq[XmlNamespaceContext]) : Parser[XmlElement, EE] = {
      for{
        element <- Parser.tell[XmlElement]
        optId <- optionalAttributeParser(ATTRIBUTES.ID.QNAME, Try.parser(XsdId.parseString))
      } yield {
          XsdAnnotation(
            optId = optId,
            optMetadata = Some(XsdElement.Metadata(element))
          )
      }
    }

    def attributes = Set(ATTRIBUTES.ID.QNAME)

    def allowedChildElements(children: Seq[XsdElementUtil[XsdElement]]) = Seq(XsdDocumentation.util, XsdAppInfo.util)
  }
}