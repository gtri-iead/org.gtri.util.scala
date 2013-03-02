package org.gtri.util.scala.xsdbuilder.elements

import org.gtri.util.scala.statemachine._
import org.gtri.util.xsddatatypes._
import org.gtri.util.xsddatatypes.XsdConstants._
import org.gtri.util.scala.xsdbuilder.XmlParser._
import org.gtri.util.scala.xmlbuilder.XmlElement

final case class XsdSimpleTypeRestriction(
  optId        :   Option[XsdId]                 = None,
  base         :   XsdQName,
  optMetadata  :   Option[XsdElement.Metadata]   = None
) extends XsdElement {

  def qName = XsdSimpleTypeRestriction.util.qName

  def optValue = None

  def util = XsdSimpleTypeRestriction.util

  def attributesMap(namespaceURIToPrefixResolver : XsdQName.NamespaceURIToPrefixResolver) = {
    {
      optId.map { (ATTRIBUTES.ID.QNAME -> _.toString) } ::
      (ATTRIBUTES.BASE.QNAME -> base.toString) ::
      Nil
    }.flatten.toMap
  }
}

object XsdSimpleTypeRestriction {

  implicit object util extends XsdElementUtil[XsdSimpleTypeRestriction] {

    def qName = ELEMENTS.ANNOTATION.QNAME

    def parser[EE >: XsdSimpleTypeRestriction](prefixToNamespaceURIResolver : XsdQName.PrefixToNamespaceURIResolver) : Parser[XmlElement, EE] = {
      for{
        element <- Parser.tell[XmlElement]
        optId <- optionalAttributeParser(ATTRIBUTES.ID.QNAME, Try.parser(XsdId.parseString))
      } yield {
          XsdSimpleTypeRestriction(
            optId = optId,
            optMetadata = Some(XsdElement.Metadata(element))
          )
      }
    }

    def attributes = Set(ATTRIBUTES.ID.QNAME)

    def allowedChildElements(children: Seq[XsdElementUtil[XsdElement]]) = Seq(XsdDocumentation.util, XsdAppInfo.util)
  }
}