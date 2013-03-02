package org.gtri.util.scala.xsdbuilder.elements

import org.gtri.util.scala.statemachine._
import org.gtri.util.xsddatatypes._
import org.gtri.util.xsddatatypes.XsdConstants._
import org.gtri.util.scala.xsdbuilder.XmlParser._
import org.gtri.util.scala.xmlbuilder.XmlElement

final case class XsdAnnotation(
  optId        :   Option[XsdId]                 = None,
  optMetadata  :   Option[XsdElement.Metadata]   = None
) extends XsdElement {

  def qName = XsdAnnotation.util.qName

  def optValue = None

  def util = XsdAnnotation.util

  def attributesMap(namespaceURIToPrefixResolver : XsdQName.NamespaceURIToPrefixResolver) = {
    {
      optId.map { (ATTRIBUTES.ID.QNAME -> _.toString) } ::
      Nil
    }.flatten.toMap
  }
}

object XsdAnnotation {

  implicit object util extends XsdElementUtil[XsdAnnotation] {

    def qName = ELEMENTS.ANNOTATION.QNAME

    def parser[EE >: XsdAnnotation](prefixToNamespaceURIResolver : XsdQName.PrefixToNamespaceURIResolver) : Parser[XmlElement, EE] = {
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