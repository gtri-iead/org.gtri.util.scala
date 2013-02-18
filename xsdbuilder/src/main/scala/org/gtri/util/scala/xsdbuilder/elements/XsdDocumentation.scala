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

  def toAttributes = {
    optSource.map({ source => (XsdConstants.ATTRIBUTES.SOURCE.QNAME,source.toString)}).toList :::
    optXmlLang.map({ xmlLang => (XsdConstants.ATTRIBUTES.XML_LANG.QNAME,xmlLang.toString)}).toList
  }

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

    def parser[EE >: XsdDocumentation] : Iteratee[XmlElement,EE] = {
      for {
        element <- QNamePeekParser(qName)
        optSource <- OptionalAttributePeekParser(ATTRIBUTES.SOURCE.QNAME, XsdAnyURI.parseString)
        optXmlLang <- OptionalAttributePeekParser(ATTRIBUTES.XML_LANG.QNAME, XsdToken.parseString)
      } yield
          XsdDocumentation(
            optSource = optSource,
            optXmlLang = optXmlLang,
            optValue = element.optValue,
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
