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

import org.gtri.util.xsddatatypes._
import org.gtri.util.scala.xmlbuilder.XmlElement.Metadata


object XmlElement {

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
final case class XmlElement(
  qName                     :  XsdQName,
  optValue                  :  Option[String]                  = None,
  attributesMap             :  Map[XsdQName, String]           = Map.empty,
  prefixToNamespaceURIMap   :  Map[XsdNCName, XsdAnyURI]       = Map.empty,
  optMetadata               :  Option[Metadata]                = None
) extends XmlNamespaceContext {

  def optPrefixesOrder = optMetadata flatMap(_.optPrefixesOrder)

  private def optOrderedAttributesTuples : Option[Seq[(XsdQName,String)]] =
    for{
      metadata <- optMetadata
      attributesOrder <- metadata.optAttributesOrder
    } yield for {
      qName <- attributesOrder
      value <- attributesMap.get(qName)
    } yield (qName, value)

  // If no metadata then sort by attribute name
  lazy val orderedAttributes : Seq[(XsdQName, String)] = optOrderedAttributesTuples.getOrElse(attributesMap.toSeq.sortBy(_._1))
}
