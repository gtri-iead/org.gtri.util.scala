package org.gtri.util.scala.xsdbuilder.elements

import org.gtri.util.scala.statemachine._
import org.gtri.util.xsddatatypes._
import org.gtri.util.scala.xmlbuilder.{XmlNamespaceContext, XmlElement}

trait XsdElementUtil[+E <: XsdElement] {
  def qName : XsdQName
  def attributes : Set[XsdQName]
  def parser[EE >: E](context: Seq[XmlNamespaceContext]) : Parser[XmlElement,EE]

  def allowedChildElements(children: Seq[XsdElementUtil[XsdElement]]) : Seq[XsdElementUtil[XsdElement]]
}