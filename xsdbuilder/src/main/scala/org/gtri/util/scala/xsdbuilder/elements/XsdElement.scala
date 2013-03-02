package org.gtri.util.scala.xsdbuilder.elements

import org.gtri.util.scala.xmlbuilder.XmlElement
import org.gtri.util.xsddatatypes.XsdConstants._
import org.gtri.util.xsddatatypes.{XsdQName, XsdAnyURI, XsdNCName}

trait XsdElement {
  def   util           :   XsdElementUtil[_]
  def   qName          :   XsdQName
  def   optValue       :   Option[String]
  def   optMetadata    :   Option[XsdElement.Metadata]

  def prefixToNamespaceURIMap : Map[XsdNCName, XsdAnyURI] = {
    val o =
      for {
        metadata <- optMetadata
        prefixToNamespaceURIMap <- metadata.optPrefixToNamespaceURIMap
      } yield prefixToNamespaceURIMap
    o.getOrElse(Map.empty)
  }

//  lazy val attributes : Seq[(XsdQName,String)] = {
//    for {
//      key <- util.attributes.toSeq
//      value <- attribute(key)
//    } yield (key,value)
//  }

  def attributesMap(namespaceURIToPrefixResolver : XsdQName.NamespaceURIToPrefixResolver) : Map[XsdQName,String]

  def toXmlElement(namespaceURIToPrefixResolver : XsdQName.NamespaceURIToPrefixResolver) = XmlElement(
      qName                     =   qName,
      optValue                  =   optValue,
      attributesMap             =   attributesMap(namespaceURIToPrefixResolver),
      prefixToNamespaceURIMap   =   prefixToNamespaceURIMap,
      optMetadata               =   optMetadata map { _.toXmlElementMetadata }
    )

//  def attribute(qName : XsdQName) : Option[String]
}

object XsdElement {
  case class Metadata(
    optAttributesOrder            :   Option[Seq[XsdQName]]               = None,
    optPrefixesOrder              :   Option[Seq[XsdNCName]]              = None,
    optPrefixToNamespaceURIMap    :   Option[Map[XsdNCName, XsdAnyURI]]   = None,
    optLocator                    :   Option[Any]                         = None
  ) {
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