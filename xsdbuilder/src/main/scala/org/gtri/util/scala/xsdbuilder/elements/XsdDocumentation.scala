package org.gtri.util.scala.xsdbuilder.elements

import org.gtri.util.scala.statemachine._
import org.gtri.util.xsddatatypes._
import org.gtri.util.xsddatatypes.XsdConstants._
import org.gtri.util.scala.xsdbuilder.XmlParser._
import org.gtri.util.scala.xmlbuilder.XmlElement

final case class XsdDocumentation(
    optSource    :   Option[XsdAnyURI]             = None,
    optXmlLang   :   Option[XsdToken]              = None,
    optValue     :   Option[String]                = None,
    optMetadata  :   Option[XsdElement.Metadata]   = None
  ) extends XsdElement {

  def qName = XsdDocumentation.util.qName

  def util = XsdDocumentation.util

  def getAttributeValue(qName: XsdQName) = {
    qName.getLocalName match {
      case ATTRIBUTES.SOURCE.LOCALNAME => optSource
      case ATTRIBUTES.XML_LANG.LOCALNAME => optXmlLang
      case ATTRIBUTES.VALUE.LOCALNAME => optValue
    }
  }

//  def toAttributes = {
//    optSource.map({ source => (XsdConstants.ATTRIBUTES.SOURCE.QNAME,source.toString)}).toList :::
//    optXmlLang.map({ xmlLang => (XsdConstants.ATTRIBUTES.XML_LANG.QNAME,xmlLang.toString)}).toList
//  }

//  def pushTo(contract: XsdContract) {
//    contract.addXsdDocumentation(
//      /* XsdAnyURI _source => */optSource.orNull,
//      /* XsdToken _xml_lang => */optXmlLang.orNull,
//      /* String _value => */value.orNull,
//      /* ImmutableMap<XsdNCName, XsdAnyURI> _prefixToNamespaceURIMap  => */prefixToNamespaceURIMap
//    )
//  }
}

object XsdDocumentation {
  implicit object util extends XsdElementUtil[XsdDocumentation] {

    def qName = XsdConstants.ELEMENTS.DOCUMENTATION.QNAME

    def parser[EE >: XsdDocumentation] : Parser[XmlElement,EE] = {
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

    def attributes = Seq(
      ATTRIBUTES.SOURCE.QNAME,
      ATTRIBUTES.XML_LANG.QNAME,
      ATTRIBUTES.VALUE.QNAME
    )

    def allowedChildElements(children: Seq[XsdElementUtil[XsdElement]]) = Seq.empty

//    def downcast(element: XsdElement) : Option[XsdDocumentation] = element match {
//      case e : XsdDocumentation => Some(e)
//      case _ => None
//    }

  }

}
