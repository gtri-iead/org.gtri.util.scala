package org.gtri.util.scala.xsdbuilder.elements

import org.gtri.util.scala.statemachine._
import org.gtri.util.xsddatatypes._
import org.gtri.util.xsddatatypes.XsdConstants._
import org.gtri.util.scala.xsdbuilder.XmlParser._
import org.gtri.util.scala.xmlbuilder.{XmlNamespaceContext, XmlElement}

final case class XsdSimpleTypeRestriction(
  optId        :   Option[XsdId]                 = None,
  base         :   XsdQName,
  optMetadata  :   Option[XsdElement.Metadata]   = None
) extends XsdElement {

  def optValue = None

  def util = XsdSimpleTypeRestriction.util

  def toAttributesMap(context: Seq[XmlNamespaceContext]) = {
    {
      optId.map { (ATTRIBUTES.ID.QNAME -> _.toString) } ::
      Some(ATTRIBUTES.BASE.QNAME -> base.toStringWithPrefix(context)) ::
      Nil
    }.flatten.toMap
  }
}

object XsdSimpleTypeRestriction {

  implicit object util extends XsdElementUtil[XsdSimpleTypeRestriction] {

    def qName = ELEMENTS.RESTRICTION.QNAME

    def parser[EE >: XsdSimpleTypeRestriction](context: Seq[XmlNamespaceContext]) : Parser[XmlElement, EE] = {
      for{
        element <- Parser.tell[XmlElement]
        optId <- optionalAttributeParser(ATTRIBUTES.ID.QNAME, Try.parser(XsdId.parseString))
        base <- requiredAttributeParser(ATTRIBUTES.BASE.QNAME, Try.parser((s:String) => XsdQName.parseStringWithPrefix(s,context)), None)
      } yield {
          XsdSimpleTypeRestriction(
            optId = optId,
            base = base,
            optMetadata = Some(XsdElement.Metadata(element))
          )
      }
    }

    def attributes = Set(ATTRIBUTES.ID.QNAME)

    def allowedChildElements(children: Seq[XsdElementUtil[XsdElement]]) = Seq(XsdDocumentation.util, XsdAppInfo.util)
  }
}