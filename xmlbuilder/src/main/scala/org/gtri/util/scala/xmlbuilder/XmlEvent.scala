package org.gtri.util.scala.xmlbuilder

import org.gtri.util.scala.exelog.noop._
import org.gtri.util.xsddatatypes.XsdQName

sealed trait XmlEvent {
  def locator : DiagnosticLocator
}

object StartXmlDocumentEvent {
  implicit val thisclass = classOf[StartXmlDocumentEvent]
  implicit val log = Logger.getLog(thisclass)
}
case class StartXmlDocumentEvent(encoding : String, version : String, isStandAlone : Boolean, characterEncodingScheme : String, locator : DiagnosticLocator) extends XmlEvent {
  import StartXmlDocumentEvent._
//  def pushTo(contract: XmlContract) {
//    log.block("pushTo", Seq("contract" -> contract)) {
//      +"noop"
//    }
//  }
}

object EndXmlDocumentEvent {
  implicit val thisclass = classOf[EndXmlDocumentEvent]
  implicit val log = Logger.getLog(thisclass)
}
case class EndXmlDocumentEvent(locator : DiagnosticLocator) extends XmlEvent {
  import EndXmlDocumentEvent._
//  def pushTo(contract: XmlContract) {
//    log.block("pushTo", Seq("contract" -> contract )) {
//      +"noop"
//    }
//  }
}

object StartXmlElementEvent {
  implicit val thisclass = classOf[StartXmlElementEvent]
  implicit val log = Logger.getLog(thisclass)
}
case class StartXmlElementEvent(element : XmlElement, locator : DiagnosticLocator) extends XmlEvent {
  import StartXmlElementEvent._

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
//        for ((name, value) <- element.attributesMap) {
//          builder.put(name, value)
//        }
//        builder.build()
//      }
//      val qName = element.qName
//      val value = element.value.orNull
//      ~s"contract.addXmlElement($qName, $value, $attributes, $prefixToNamespaceURIMap)"
//      contract.addXmlElement(qName, value, attributes, prefixToNamespaceURIMap)
//    }
//  }
}

object EndXmlElementEvent {
  implicit val thisclass = classOf[EndXmlElementEvent]
  implicit val log = Logger.getLog(thisclass)
}
case class EndXmlElementEvent(qName : XsdQName, locator : DiagnosticLocator) extends XmlEvent {
  import EndXmlElementEvent._
//  def pushTo(contract: XmlContract) {
//    log.block("pushTo",Seq("contract" -> contract)) {
//      +"Pushing EndXmlElementEvent to XmlContract"
//      ~"contract.endXmlElement()"
//      contract.endXmlElement()
//    }
//  }
}

object AddXmlCommentEvent {
  implicit val thisclass = classOf[AddXmlCommentEvent]
  implicit val log = Logger.getLog(thisclass)
}
case class AddXmlCommentEvent(comment : String, locator : DiagnosticLocator) extends XmlEvent {
  import AddXmlCommentEvent._
//  def pushTo(contract: XmlContract) {
//    log.block("pushTo", Seq("contract" -> contract)) {
//      +"Pushing AddXmlCommentEvent to XmlContract"
//      ~s"contract.addXmlComment($comment)"
//      contract.addXmlComment(comment)
//    }
//  }
}

object AddXmlTextEvent {
  implicit val thisclass = classOf[AddXmlTextEvent]
  implicit val log = Logger.getLog(thisclass)
}
case class AddXmlTextEvent(text : String, locator : DiagnosticLocator) extends XmlEvent {
  import AddXmlTextEvent._
//  def pushTo(contract: XmlContract) {
//    log.block("pushTo", Seq("contract" -> contract)) {
//      +"Pushing AddXmlTextEvent to XmlContract"
//      ~s"contract.addXmlText($text)"
//      contract.addXmlText(text)
//    }
//  }
}
