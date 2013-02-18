/*
    Copyright 2012 Georgia Tech Research Institute

    Author: lance.gatlin@gtri.gatech.edu

    This file is part of org.gtri.util.scala.xmlbuilder library.

    org.gtri.util.scala.xmlbuilder library is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    org.gtri.util.scala.xmlbuilder library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with org.gtri.util.scala.xmlbuilder library. If not, see <http://www.gnu.org/licenses/>.

*/
package org.gtri.util.scala.xmlbuilder

import org.gtri.util.scala.exelog.noop._
import org.gtri.util.xsddatatypes._
import org.gtri.util.xsddatatypes.XsdQName._
import org.gtri.util.scala.xmlbuilder.XmlElement.Metadata


object XmlElement {

  implicit val thisclass =    classOf[XmlElement]
  implicit val log : Log =    Logger.getLog(thisclass)

  case class Metadata(
    optRawAttributesOrder   :   Option[Seq[String]]         = None, // Note: this includes xmlns:XXX
    optAttributesOrder      :   Option[Seq[XsdQName]]       = None, // This does *not* include xmlns
    optPrefixesOrder        :   Option[Seq[XsdNCName]]      = None,
    optLocator              :   Option[DiagnosticLocator]   = None
  )

  def apply(
    qName                   :   XsdQName,
    optValue                :   Option[String],
    attributes              :   Seq[(XsdQName, String)],
    prefixes                :   Seq[(XsdNCName, XsdAnyURI)]
  ) = new XmlElement(
    qName                     =   qName,
    optValue                  =   optValue,
    attributesMap             =   attributes.toMap,
    prefixToNamespaceURIMap   =   prefixes.toMap,
    optMetadata               =   Some(Metadata(
      optRawAttributesOrder =   None,
      optAttributesOrder    =   Some(attributes map { _._1 }),
      optPrefixesOrder      =   Some(prefixes map { _._1 }),
      optLocator            =   None
    ))
  )
  def apply(
    qName                   :   XsdQName,
    optValue                :   Option[String],
    attributes              :   Seq[(XsdQName, String)],
    prefixes                :   Seq[(XsdNCName, XsdAnyURI)],
    optLocator              :   Option[DiagnosticLocator]
  ) = new XmlElement(
    qName                     =   qName,
    optValue                  =   optValue,
    attributesMap             =   attributes.toMap,
    prefixToNamespaceURIMap   =   prefixes.toMap,
    optMetadata               =   Some(Metadata(
      optRawAttributesOrder =  None,
      optAttributesOrder    =  Some(attributes map { _._1 }),
      optPrefixesOrder      =  Some(prefixes map { _._1 }),
      optLocator            =  optLocator
    ))
  )
  def apply(
    qName                   :   XsdQName,
    optValue                :   Option[String],
    attributes              :   Seq[(XsdQName, String)],
    prefixes                :   Seq[(XsdNCName, XsdAnyURI)],
    optLocator              :   Option[DiagnosticLocator],
    optRawAttributes        :   Option[Seq[(String,String)]]
  ) = new XmlElement(
    qName                     =   qName,
    optValue                  =   optValue,
    attributesMap             =   attributes.toMap,
    prefixToNamespaceURIMap   =   prefixes.toMap,
    optMetadata               =   Some(Metadata(
      optRawAttributesOrder =  optRawAttributes map { _ map { _._1 } },
      optAttributesOrder    =  Some(attributes map { _._1 }),
      optPrefixesOrder      =  Some(prefixes map { _._1 }),
      optLocator            =  optLocator
    ))
  )
}
case class XmlElement(
  qName                     :  XsdQName,
  optValue                  :  Option[String]                  = None,
  attributesMap             :  Map[XsdQName, String]           = Map.empty,
  prefixToNamespaceURIMap   :  Map[XsdNCName, XsdAnyURI]       = Map.empty,
  optMetadata                  :  Option[Metadata]             = None
)extends NamespaceURIToPrefixResolver with PrefixToNamespaceURIResolver {

  lazy val metadataOrderedAttributes : Option[Seq[(XsdQName,String)]] =
    for{
      metadata <- optMetadata
      attributesOrder <- metadata.optAttributesOrder
    } yield for {
      qName <- attributesOrder
      value <- attributesMap.get(qName)
    } yield (qName, value)

  lazy val orderedAttributes : Seq[(XsdQName, String)] =
    metadataOrderedAttributes getOrElse {
      // If no metadata then sort by attribute name
      attributesMap.toSeq.sortWith { (t1,t2) => t1._1.toString < t2._1.toString }
    }

  //  {
//    log.block("orderAttributes") {
//      +"Order attributes by attributesOrder optMetadata (if defined) or sort lexographically by name"
//      val o : Option[Seq[(XsdQName,String)]] =
//      val retv : Seq[(XsdQName,String)] =
//        o.getOrElse(
//          attributesMap.toSeq.sortWith { (t1,t2) => t1._1.toString < t2._1.toString }
//        )
//
//      if(optMetadata.isDefined && optMetadata.get.optAttributesOrder.isDefined) {
//        ~"Sort by optMetadata attributesOrder"
//        optMetadata.get.optAttributesOrder.get.map({
//          qName => attributesMap.get(qName).map { optValue => (qName,optValue) }
//        }).flatten
//      } else {
//        ~"Sort by name lexographically"
//        attributesMap.toSeq.sortWith { (t1,t2) => t1._1.toString < t2._1.toString }
//      }
//    }
//  }

  lazy val metadataOrderedPrefixes : Option[Seq[(XsdNCName, XsdAnyURI)]] =
    for{
      metadata <- optMetadata
      prefixesOrder <- metadata.optPrefixesOrder
    } yield for {
      prefix <- prefixesOrder
      uri <- prefixToNamespaceURIMap.get(prefix)
    } yield (prefix,uri)

  lazy val orderedPrefixes : Seq[(XsdNCName, XsdAnyURI)] =
    metadataOrderedPrefixes getOrElse {
      // If no metadata, sort by prefix name
      prefixToNamespaceURIMap.toSeq.sortWith { (t1,t2) => t1._1.toString < t2._1.toString }
    }
//  {
//    log.block("orderedPrefixes") {
//      +"Order prefixes by prefixOrder optMetadata (if defined) or sort lexographically name"
//      if(optMetadata.isDefined && optMetadata.get.optPrefixesOrder.isDefined) {
//        ~"Sort by optMetadata prefixOrder"
//        optMetadata.get.optPrefixesOrder.get.map({
//          prefix => prefixToNamespaceURIMap.get(prefix).map { uri => (prefix,uri) }
//        }).flatten
//      } else {
//        ~"Sort by name lexographically"
//        prefixToNamespaceURIMap.toSeq.sortWith { (t1,t2) => t1._1.toString < t2._1.toString }
//      }
//    }
//  }

  lazy val namespaceURIToPrefixMap = prefixToNamespaceURIMap.map(_.swap)

  def isValidPrefixForNamespaceURI(prefix: XsdNCName, namespaceURI: XsdAnyURI) =
    // Convert to Map[XsdNCName, Boolean] that is true if it matches namespaceURI and provide a default optValue false if prefix isn't mapped
    prefixToNamespaceURIMap mapValues { _ == namespaceURI } getOrElse(prefix, false)

//  {
//    log.block("isValidPrefixForNamespaceURI", Seq("prefix" -> prefix, "namespaceURI" -> namespaceURI)) {
//      +"TRUE if prefix is defined with the given namespaceURI otherwise FALSE"
//      val optionNsURI = prefixToNamespaceURIMap.get(prefix)
//      if(optionNsURI.isDefined) {
//        optionNsURI.get == namespaceURI
//      } else {
//        false
//      }
//    }
//  }

  def getPrefixForNamespaceURI(namespaceURI: XsdAnyURI) = namespaceURIToPrefixMap.get(namespaceURI).orNull

  def getNamespaceURIForPrefix(prefix : XsdNCName) = prefixToNamespaceURIMap.get(prefix).orNull
}