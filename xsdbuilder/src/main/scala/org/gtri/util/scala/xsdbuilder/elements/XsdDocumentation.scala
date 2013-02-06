package org.gtri.util.scala.xsdbuilder.elements

import org.gtri.util.scala.statemachine._
import org.gtri.util.xsddatatypes._
import org.gtri.util.xsddatatypes.XsdConstants._
import org.gtri.util.scala.xsdbuilder.XmlParser._
import org.gtri.util.scala.xmlbuilder.XmlElement

case class XsdDocumentation(
    source    :   Option[XsdAnyURI]             = None,
    xml_lang  :   Option[XsdToken]              = None,
    value     :   Option[String]                = None,
    optMetadata  :   Option[XsdElement.Metadata]   = None
  ) extends XsdElement {

  def qName = XsdDocumentation.util.qName

  def toAttributes = {
    source.map({ source => (XsdConstants.ATTRIBUTES.SOURCE.QNAME,source.toString)}).toList :::
    xml_lang.map({ xml_lang => (XsdConstants.ATTRIBUTES.XML_LANG.QNAME,xml_lang.toString)}).toList
  }

//  def pushTo(contract: XsdContract) {
//    contract.addXsdDocumentation(
//      /* XsdAnyURI _source => */source.orNull,
//      /* XsdToken _xml_lang => */xml_lang.orNull,
//      /* String _value => */value.orNull,
//      /* ImmutableMap<XsdNCName, XsdAnyURI> _prefixToNamespaceURIMap  => */prefixToNamespaceURIMap
//    )
//  }
}

object XsdDocumentation {
  implicit object util extends XsdElementUtil[XsdDocumentation] {

    def qName = XsdConstants.ELEMENTS.DOCUMENTATION.QNAME

    def parser[EE >: XsdDocumentation] : Iteratee[XmlElement,EE] = {
      for {
        element <- QNamePeekParser(qName)
        source <- OptionalAttributePeekParser(ATTRIBUTES.SOURCE.QNAME, XsdAnyURI.parseString)
        xml_lang <- OptionalAttributePeekParser(ATTRIBUTES.XML_LANG.QNAME, XsdToken.parseString)
      } yield
          XsdDocumentation(
            source = source,
            xml_lang = xml_lang,
            value = element.value,
            optMetadata = Some(XsdElement.Metadata(element))
          )
    }


    def allowedChildElements(children: Seq[XsdElementUtil[XsdElement]]) = Seq.empty

//    def downcast(element: XsdElement) : Option[XsdDocumentation] = element match {
//      case e : XsdDocumentation => Some(e)
//      case _ => None
//    }

  }

}
