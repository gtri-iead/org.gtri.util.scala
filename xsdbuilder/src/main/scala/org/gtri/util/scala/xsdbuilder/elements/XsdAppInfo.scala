package org.gtri.util.scala.xsdbuilder.elements

import org.gtri.util.scala.statemachine._
import org.gtri.util.xsddatatypes._
import org.gtri.util.xsddatatypes.XsdConstants._
import org.gtri.util.scala.xsdbuilder.XmlParser._
import org.gtri.util.scala.xmlbuilder.XmlElement

final case class XsdAppInfo(
  optSource    :   Option[XsdAnyURI]             = None,
  optMetadata  :   Option[XsdElement.Metadata]   = None
) extends XsdElement {

  def qName = XsdAppInfo.util.qName

  def optValue = None

  def util = XsdAppInfo.util

  def attributesMap(namespaceURIToPrefixResolver : XsdQName.NamespaceURIToPrefixResolver) = {
    {
      optSource.map { (ATTRIBUTES.SOURCE.QNAME -> _.toString) } ::
      Nil
    }.flatten.toMap
  }

}

object XsdAppInfo {

  implicit object util extends XsdElementUtil[XsdAppInfo] {

    def qName = ELEMENTS.APPINFO.QNAME

    def parser[EE >: XsdAppInfo](prefixToNamespaceURIResolver : XsdQName.PrefixToNamespaceURIResolver) : Parser[XmlElement,EE] = {
      for{
        element <- Parser.tell[XmlElement]
        optSource <- optionalAttributeParser(ATTRIBUTES.SOURCE.QNAME, Try.parser((XsdAnyURI.parseString)))
      } yield
          XsdAppInfo(
            optSource = optSource,
            optMetadata = Some(XsdElement.Metadata(element))
          )
    }

    def attributes = Set(ATTRIBUTES.SOURCE.QNAME)

    def allowedChildElements(children: Seq[XsdElementUtil[XsdElement]]) = Seq()
  }
}