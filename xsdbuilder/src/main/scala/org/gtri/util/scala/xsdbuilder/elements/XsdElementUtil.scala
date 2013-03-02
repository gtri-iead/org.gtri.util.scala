package org.gtri.util.scala.xsdbuilder.elements

import org.gtri.util.scala.statemachine._
import org.gtri.util.xsddatatypes._
import org.gtri.util.xsddatatypes.XsdConstants._
import org.gtri.util.scala.xmlbuilder.XmlElement

trait XsdElementUtil[+E <: XsdElement] {
  def qName : XsdQName
//  def   qNameToXsdAttributeMap : Map[XsdQName, XsdAttribute]
  def attributes : Set[XsdQName]
  def parser[EE >: E](prefixToNamespaceURIResolver : XsdQName.PrefixToNamespaceURIResolver) : Parser[XmlElement,EE]

  def allowedChildElements(children: Seq[XsdElementUtil[XsdElement]]) : Seq[XsdElementUtil[XsdElement]]
}