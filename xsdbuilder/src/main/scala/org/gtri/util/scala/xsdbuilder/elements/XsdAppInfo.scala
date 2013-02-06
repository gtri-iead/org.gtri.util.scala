package org.gtri.util.scala.xsdbuilder.elements

import org.gtri.util.scala.statemachine._
import org.gtri.util.xsddatatypes._
import org.gtri.util.xsddatatypes.XsdConstants._
import org.gtri.util.scala.xsdbuilder.XmlParser._
import org.gtri.util.scala.xmlbuilder.XmlElement

case class XsdAppInfo(
  source    :   Option[XsdAnyURI]             = None,
  optMetadata  :   Option[XsdElement.Metadata]   = None
) extends XsdElement {

  def qName = XsdAppInfo.util.qName

  def value = None

  def toAttributes = {
    source.map({ source => (XsdConstants.ATTRIBUTES.SOURCE.QNAME,source.toString)}).toList
  }

//  def pushTo(contract: XsdContract) {
//    contract.addXsdAppInfo(
//      /* XsdAnyURI _source => */source.orNull,
//      /* ImmutableMap<XsdNCName, XsdAnyURI> _prefixToNamespaceURIMap  => */prefixToNamespaceURIMap
//    )
//  }

}

object XsdAppInfo {

  implicit object util extends XsdElementUtil[XsdAppInfo] {

    def qName = ELEMENTS.APPINFO.QNAME

    def parser[EE >: XsdAppInfo] : Iteratee[XmlElement,EE] = {
      for{
        element <- QNamePeekParser(qName)
        source <- OptionalAttributePeekParser(ATTRIBUTES.SOURCE.QNAME, XsdAnyURI.parseString)
      } yield
          XsdAppInfo(
            source = source,
            optMetadata = Some(XsdElement.Metadata(element))
          )
    }


    def allowedChildElements(children: Seq[XsdElementUtil[XsdElement]]) = Seq()

//    def downcast(element: XsdElement) : Option[XsdAppInfo] = element match {
//      case e : XsdAppInfo => Some(e)
//      case _ => None
//    }
  }
}