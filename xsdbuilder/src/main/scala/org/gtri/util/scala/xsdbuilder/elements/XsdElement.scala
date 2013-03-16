package org.gtri.util.scala.xsdbuilder.elements

import org.gtri.util.scala.xmlbuilder.{XmlNamespaceContext, XmlElement}
import org.gtri.util.xsddatatypes.{XsdQName, XsdAnyURI, XsdNCName}

trait XsdElement  {
  def util           :   XsdElementUtil[_]
  def optValue       :   Option[String]
  def optMetadata    :   Option[XsdElement.Metadata]

  def toAttributesMap(context: Seq[XmlNamespaceContext]) : Map[XsdQName,String]

  def toXmlElement(context: Seq[XmlNamespaceContext]) = XmlElement(
      qName                     =   util.qName,
      optValue                  =   optValue,
      attributesMap             =   toAttributesMap(optMetadata.map(_ +: context).getOrElse(context)),
      prefixToNamespaceURIMap   =   optMetadata.map(_.prefixToNamespaceURIMap).getOrElse(Map.empty),
      optMetadata               =   optMetadata.map(_.toXmlElementMetadata)
    )
}

object XsdElement {
  case class Metadata(
    optAttributesOrder            :   Option[Seq[XsdQName]]               = None,
    optPrefixesOrder              :   Option[Seq[XsdNCName]]              = None,
    optPrefixToNamespaceURIMap    :   Option[Map[XsdNCName, XsdAnyURI]]   = None,
    optLocator                    :   Option[Any]                         = None
  ) extends XmlNamespaceContext {
    lazy val prefixToNamespaceURIMap : Map[XsdNCName, XsdAnyURI] = optPrefixToNamespaceURIMap.getOrElse(Map.empty)

    def toXmlElementMetadata = XmlElement.Metadata(
        optAttributesOrder  =   optAttributesOrder,
        optPrefixesOrder    =   optPrefixesOrder,
        optLocator          =   optLocator
      )
  }
  object Metadata {
    def apply(element : XmlElement) = new Metadata(
      optAttributesOrder          =   element.optMetadata flatMap { _.optAttributesOrder },
      optPrefixesOrder            =   element.optMetadata flatMap { _.optPrefixesOrder },
      optPrefixToNamespaceURIMap  =   Some(element.prefixToNamespaceURIMap),
      optLocator                  =   element.optMetadata flatMap { _.optLocator }
    )
  }
}