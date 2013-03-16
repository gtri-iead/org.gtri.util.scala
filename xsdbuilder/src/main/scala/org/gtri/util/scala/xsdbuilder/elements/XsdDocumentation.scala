package org.gtri.util.scala.xsdbuilder.elements

import org.gtri.util.scala.statemachine._
import org.gtri.util.xsddatatypes._
import org.gtri.util.xsddatatypes.XsdConstants._
import org.gtri.util.scala.xsdbuilder.XmlParser._
import org.gtri.util.scala.xmlbuilder.{XmlNamespaceContext, XmlElement}

final case class XsdDocumentation(
    optSource    :   Option[XsdAnyURI]             = None,
    optXmlLang   :   Option[XsdToken]              = None,
    optValue     :   Option[String]                = None,
    optMetadata  :   Option[XsdElement.Metadata]   = None
  ) extends XsdElement {

  def util = XsdDocumentation.util

  def toAttributesMap(context: Seq[XmlNamespaceContext]) = {
    {
      optSource.map { (ATTRIBUTES.SOURCE.QNAME -> _.toString) } ::
      optXmlLang.map { (ATTRIBUTES.FINALDEFAULT.QNAME -> _.toString )} ::
      Nil
    }.flatten.toMap
  }
}

object XsdDocumentation {
  implicit object util extends XsdElementUtil[XsdDocumentation] {

    def qName = XsdConstants.ELEMENTS.DOCUMENTATION.QNAME

    def parser[EE >: XsdDocumentation](context: Seq[XmlNamespaceContext]) : Parser[XmlElement,EE] = {
      for {
        element <- Parser.tell[XmlElement]
        optSource <- optionalAttributeParser(ATTRIBUTES.SOURCE.QNAME, Try.parser(XsdAnyURI.parseString))
        optXmlLang <- optionalAttributeParser(ATTRIBUTES.XML_LANG.QNAME, Try.parser(XsdToken.parseString))
      } yield
          XsdDocumentation(
            optSource = optSource,
            optXmlLang = optXmlLang,
            optValue = element.optValue,
            optMetadata = Some(XsdElement.Metadata(element))
          )
    }

    def attributes = Set(
      ATTRIBUTES.SOURCE.QNAME,
      ATTRIBUTES.XML_LANG.QNAME,
      ATTRIBUTES.VALUE.QNAME
    )

    def allowedChildElements(children: Seq[XsdElementUtil[XsdElement]]) = Seq.empty

  }

}
