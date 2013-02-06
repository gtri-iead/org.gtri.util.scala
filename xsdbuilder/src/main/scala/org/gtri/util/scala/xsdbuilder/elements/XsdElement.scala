package org.gtri.util.scala.xsdbuilder.elements

import org.gtri.util.scala.xmlbuilder.XmlElement
import org.gtri.util.xsddatatypes.XsdConstants._
import org.gtri.util.xsddatatypes.{XsdQName, XsdAnyURI, XsdNCName}

trait XsdElement {
  def   qName       :   XsdQName
  def   value       :   Option[String]
  def   optMetadata    :   Option[XsdElement.Metadata]

  lazy val prefixToNamespaceURIMap : Map[XsdNCName, XsdAnyURI] = {
    val o =
      for {
        metadata <- optMetadata
        prefixToNamespaceURIMap <- metadata.optPrefixToNamespaceURIMap
      } yield prefixToNamespaceURIMap
    o.getOrElse(Map.empty)
//    if(metadata.isDefined && metadata.get.prefixToNamespaceURIMap.isDefined) {
//      metadata.get.prefixToNamespaceURIMap.get
//    } else {
//      Map.empty
//    }
  }
//  def pushTo(contract : XsdContract)

  def toAttributes    : Seq[(XsdQName,String)]
  def toXmlElement = XmlElement(
      qName                     =   qName,
      value                     =   value,
      attributesMap             =   toAttributes.toMap,
      prefixToNamespaceURIMap   =   prefixToNamespaceURIMap,
      optMetadata               =   optMetadata map { _.toXmlElementMetadata }
    )
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