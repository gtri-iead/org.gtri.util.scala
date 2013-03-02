package org.gtri.util.scala.xsdbuilder.elements

import org.gtri.util.scala.statemachine._
import org.gtri.util.xsddatatypes._
import org.gtri.util.xsddatatypes.XsdCodes._
import org.gtri.util.xsddatatypes.XsdConstants._
import org.gtri.util.scala.xsdbuilder.XmlParser._
import org.gtri.util.scala.xmlbuilder.XmlElement
import org.gtri.util.scala.xsdbuilder.XsdAllOrNone
import org.gtri.util.scala.xsdbuilder.elements.XsdGenVal._

case class XsdSimpleType(
  optId            :   Option[XsdId]                                             = None,
  name             :   XsdName,
  optFinal    :   Option[XsdAllOrNone[SimpleTypeFinalCode]]   = None,
  optMetadata      :   Option[XsdElement.Metadata]                               = None
) extends XsdElement {

  def qName = XsdSimpleType.util.qName

  def optValue = None

  def util = XsdAnnotation.util

  def attributesMap(namespaceURIToPrefixResolver : XsdQName.NamespaceURIToPrefixResolver) = {
    {
      optId.map { (ATTRIBUTES.ID.QNAME -> _.toString) } ::
      Some(ATTRIBUTES.NAME.QNAME -> name.toString) ::
      optFinal.map { (ATTRIBUTES.FINAL.QNAME -> _.toString) } ::
      Nil
    }.flatten.toMap
  }
}

object XsdSimpleType {

  implicit object util extends XsdElementUtil[XsdSimpleType] {
    def qName = ELEMENTS.SIMPLETYPE.QNAME

    def parser[EE >: XsdSimpleType](prefixToNamespaceURIResolver : XsdQName.PrefixToNamespaceURIResolver) : Parser[XmlElement,EE] = {
      for{
        element <- Parser.tell[XmlElement]
        optId <- optionalAttributeParser(ATTRIBUTES.ID.QNAME, Try.parser(XsdId.parseString))
        name <- requiredAttributeParser(ATTRIBUTES.NAME.QNAME,Try.parser(XsdName.parseString), Some(genRandomName _))
        optFinal <- optionalAttributeParser(ATTRIBUTES.FINAL.QNAME, XsdAllOrNone.parser(Try.parser(SimpleTypeFinalCode.parseString)))
      } yield
          XsdSimpleType(
            optId = optId,
            name = name,
            optFinal = optFinal,
            optMetadata = Some(XsdElement.Metadata(element))
          )
    }

    def attributes = Set(
      ATTRIBUTES.ID.QNAME,
      ATTRIBUTES.NAME.QNAME,
      ATTRIBUTES.FINAL.QNAME
    )

    def allowedChildElements(children: Seq[XsdElementUtil[XsdElement]]) = {
      Seq(XsdAnnotation.util)
    }

  }
}