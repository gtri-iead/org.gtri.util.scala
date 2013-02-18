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
import org.gtri.util.xsddatatypes.XsdQName

sealed trait XmlEvent {
  def locator : DiagnosticLocator
}

//object StartXmlDocumentEvent {
//  implicit val thisclass = classOf[StartXmlDocumentEvent]
//  implicit val log = Logger.getLog(thisclass)
//}
case class StartXmlDocumentEvent(
  encoding                  :   String,
  version                   :   String,
  isStandAlone              :   Boolean,
  characterEncodingScheme   :   String,
  locator                   :   DiagnosticLocator
) extends XmlEvent
//{
//  import StartXmlDocumentEvent._
//  def pushTo(contract: XmlContract) {
//    log.block("pushTo", Seq("contract" -> contract)) {
//      +"noop"
//    }
//  }
//}

//object EndXmlDocumentEvent {
//  implicit val thisclass = classOf[EndXmlDocumentEvent]
//  implicit val log = Logger.getLog(thisclass)
//}
case class EndXmlDocumentEvent(
  locator                   :   DiagnosticLocator
) extends XmlEvent
//{
//  import EndXmlDocumentEvent._
//  def pushTo(contract: XmlContract) {
//    log.block("pushTo", Seq("contract" -> contract )) {
//      +"noop"
//    }
//  }
//}

//object StartXmlElementEvent {
//  implicit val thisclass = classOf[StartXmlElementEvent]
//  implicit val log = Logger.getLog(thisclass)
//}
case class StartXmlElementEvent(
  element                   :   XmlElement,
  locator                   :   DiagnosticLocator
) extends XmlEvent
// {
//  import StartXmlElementEvent._
//  def pushTo(contract: XmlContract) {
//    log.block("pushTo", Seq("contract" -> contract)) {
//      +"Pushing StartXmlElementEvent to XmlContract"
//      ~"Build prefixToNamespaceURIMap"
//      val prefixToNamespaceURIMap = {
//        val builder = ImmutableMap.builder[XsdNCName, XsdAnyURI]()
//        for ((prefix, namespaceUri) <- element.prefixToNamespaceURIMap) {
//          builder.put(prefix, namespaceUri)
//        }
//        builder.build()
//      }
//      ~"Build attributes"
//      val attributes = {
//        val builder = ImmutableMap.builder[XsdQName, String]()
//        for ((name, optValue) <- element.attributesMap) {
//          builder.put(name, optValue)
//        }
//        builder.build()
//      }
//      val qName = element.qName
//      val optValue = element.optValue.orNull
//      ~s"contract.addXmlElement($qName, $optValue, $attributes, $prefixToNamespaceURIMap)"
//      contract.addXmlElement(qName, optValue, attributes, prefixToNamespaceURIMap)
//    }
//  }
//}

//object EndXmlElementEvent {
//  implicit val thisclass = classOf[EndXmlElementEvent]
//  implicit val log = Logger.getLog(thisclass)
//}
case class EndXmlElementEvent(
  qName                     :   XsdQName,
  locator                   :   DiagnosticLocator
) extends XmlEvent
//{
//  import EndXmlElementEvent._
//  def pushTo(contract: XmlContract) {
//    log.block("pushTo",Seq("contract" -> contract)) {
//      +"Pushing EndXmlElementEvent to XmlContract"
//      ~"contract.endXmlElement()"
//      contract.endXmlElement()
//    }
//  }
//}

//object AddXmlCommentEvent {
//  implicit val thisclass = classOf[AddXmlCommentEvent]
//  implicit val log = Logger.getLog(thisclass)
//}
case class AddXmlCommentEvent(
  comment                   :   String,
  locator                   :   DiagnosticLocator
) extends XmlEvent
//{
//  import AddXmlCommentEvent._
//  def pushTo(contract: XmlContract) {
//    log.block("pushTo", Seq("contract" -> contract)) {
//      +"Pushing AddXmlCommentEvent to XmlContract"
//      ~s"contract.addXmlComment($comment)"
//      contract.addXmlComment(comment)
//    }
//  }
//}

//object AddXmlTextEvent {
//  implicit val thisclass = classOf[AddXmlTextEvent]
//  implicit val log = Logger.getLog(thisclass)
//}
case class AddXmlTextEvent(
  text                      :   String,
  locator                   :   DiagnosticLocator
) extends XmlEvent
//{
//  import AddXmlTextEvent._
//  def pushTo(contract: XmlContract) {
//    log.block("pushTo", Seq("contract" -> contract)) {
//      +"Pushing AddXmlTextEvent to XmlContract"
//      ~s"contract.addXmlText($text)"
//      contract.addXmlText(text)
//    }
//  }
//}
