package org.gtri.util.scala.xmlbuilder

import org.gtri.util.xsddatatypes._
import org.gtri.util.xsddatatypes.XsdQName._

trait XmlNamespaceContext extends NamespaceURIToPrefixResolver with PrefixToNamespaceURIResolver {

  def prefixToNamespaceURIMap  : Map[XsdNCName, XsdAnyURI]
  def optPrefixesOrder : Option[Seq[(XsdNCName)]]

  private def optOrderedPrefixTuples : Option[Seq[(XsdNCName, XsdAnyURI)]] =
    for{
      prefixesOrder <- optPrefixesOrder
    } yield for {
      prefix <- prefixesOrder
      uri <- prefixToNamespaceURIMap.get(prefix)
    } yield (prefix,uri)

  // If no prefixes order defined, sort by prefix name
  lazy val orderedPrefixToNamespaceURITuples : Seq[(XsdNCName, XsdAnyURI)] = optOrderedPrefixTuples.getOrElse(prefixToNamespaceURIMap.toSeq.sortBy(_._1))

  // Using orderedPrefixes to select the first prefix if there are multiple prefixes mapped to a particular uri
  lazy val namespaceURIToPrefixMap : Map[XsdAnyURI,XsdNCName] = orderedPrefixToNamespaceURITuples.groupBy(_._2).mapValues(_.head._1)

  def isValidPrefixForNamespaceURI(prefix: XsdNCName, namespaceURI: XsdAnyURI) = prefixToNamespaceURIMap.mapValues(_ == namespaceURI).getOrElse(prefix, false)

  def getPrefixForNamespaceURI(namespaceURI: XsdAnyURI) = namespaceURIToPrefixMap.get(namespaceURI).orNull

  def getNamespaceURIForPrefix(prefix : XsdNCName) = prefixToNamespaceURIMap.get(prefix).orNull
}
