package org.gtri.util.scala.xsdbuilder

import elements.{XsdElementUtil, XsdElement}
import org.gtri.util.scala.xmlbuilder.XmlEvent

sealed trait XsdEvent {
  def locator : Any
}

case class StartXsdDocumentEvent(encoding : String, version : String, isStandAlone : Boolean, characterEncodingScheme : String, locator : Any) extends XsdEvent {
//  def pushTo(contract : XsdContract) {
//  }
}

case class EndXsdDocumentEvent(locator : Any) extends XsdEvent {
//  def pushTo(contract : XsdContract) {
//  }
}

case class StartXsdEvent(item : XsdElement, locator : Any) extends XsdEvent {
//  def pushTo(contract : XsdContract) = item.pushTo(contract)
}
object StartXsdEvent {
//  def parse[E <: XsdElement](e : XmlElement, locator : Any, util : XsdElementUtil[E]) : Box[StartXsdEvent] = {
//    for(inner <- util.parse(e))
//    yield for(item <- inner)
//    yield StartXsdEvent(item, locator)
//  }
}

case class EndXsdEvent(item : XsdElement, locator : Any) extends XsdEvent {
//  def pushTo(contract : XsdContract) = contract.endXsdElement()
}

case class AddXsdXmlLexicalEvent(xmlEvent : XmlEvent, locator : Any) extends XsdEvent {
//  def pushTo(contract : XsdContract) = xmlEvent.pushTo(contract)
}
