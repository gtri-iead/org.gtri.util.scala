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

import javax.xml.stream.{XMLInputFactory, XMLStreamConstants, XMLStreamReader}
import org.gtri.util.scala.exelog.noop._
import org.gtri.util.xsddatatypes._
import org.gtri.util.scala.statemachine._
import java.io.InputStream
import annotation.tailrec


object XmlReader {
  implicit val thisclass  =   classOf[XmlReader]
  implicit val log        =   Logger.getLog(thisclass)

}
case class XmlReader(
  in              :   InputStream,
  totalByteSize   :   Int               = 0,
  chunkSize       :   Int               = StateMachine.STD_CHUNK_SIZE
) extends Enumerator[XmlEvent] {
  import XmlReader._
  import Enumerator._

  require(chunkSize > 0)

  private val reader = XMLInputFactory.newInstance().createXMLStreamReader(in)

  def s0() = {
    log.block("s0") {
        Cont(totalByteSize)
//      try {
//        +"Trying to create reader"
//        val result = factory.create()
//        +"Created reader"
//        Cont(result.reader(), new Progress(0,0,result.totalByteSize))
//      } catch {
//        case e : Exception =>
//          log.fatal("Failed to create reader",e)
//          val msg : String = e.getMessage
//          val issue : Issue = Issues.INSTANCE.fatalError(msg)
//          Failure[XmlEvent](
//            progress = Progress.empty,
//            issues = Chunk(issue)
//          )
//      }
    }
  }

  case class Cont(totalByteSize : Int) extends State.Continuation[XmlEvent] {

    def apply(x : Unit) = {
      log.block("apply") {
        // TODO: what happens if reader fails?
        +"Filling buffer"
        ~s"Creating buffer of size=$chunkSize"
        val buffer = new collection.mutable.ArrayBuffer[XmlEvent](chunkSize)
        ~s"Filling buffer with nextEvents"
        while(buffer.size < chunkSize && reader.hasNext) {
          buffer ++= nextEvents().reverse
        }
        ~s"Filled buffer#: $buffer"

        +"Calculating nextProgress"
        val nextProgress = {
          if(totalByteSize > 0) {
            val charOffset = reader.getLocation.getCharacterOffset
            ~s"Got charOffset=$charOffset -1 is returned after eoi reached"
            if(charOffset == -1) {
              ~s"At eoi"
              new Progress(totalByteSize,totalByteSize)
            } else {
              ~s"Some progress"
              new Progress(reader.getLocation.getCharacterOffset,totalByteSize)
            }
          } else {
            ~s"Not possible to calc nextProgress setting to empty"
            Progress.empty
          }
        }
        ~s"nextProgress=$nextProgress"

        +s"If buffer is empty we are done"
        if(buffer.isEmpty) {
          +s"Buffer is empty - close reader and return Success"
          reader.close()
          Succeed(
            output = Seq(EndXmlDocumentEvent(getLocatorFromReader)),
            metadata = Seq(nextProgress)
          )
        } else {
          +s"Buffer not empty - make immutable copy and return result"
          Continue(
            state = this,
            output = buffer.toSeq,
            metadata = Seq(nextProgress)
          )
        }
      }
    }

    def apply(eoi : EndOfInput) = {
      reader.close()
      Succeed()
    }

    private def nextEvents() : List[XmlEvent] = {
      log.block("nextEvents") {
        val eventType = reader.getEventType()
        ~s"MATCH reader.getEventType=$eventType"
        eventType match {
  // TODO: these don't work for some reason
  //        case XMLStreamConstants.ATTRIBUTE => {
  //          val i = reader.getAttributeCount - 1
  //          println("getAttributeCount=" + reader.getAttributeCount)
  //          println("getAttributeLocalName=" + reader.getAttributeName(i))
  //          println("getAttributeCount=" + reader.getAttributeValue(i))
  //          nextEvents()
  //        }
  //        case XMLStreamConstants.NAMESPACE => {
  //          val i = reader.getNamespaceCount - 1
  //          println("getNamespaceCount=" + reader.getNamespaceCount)
  //          println("getNamespacePrefix=" + reader.getNamespacePrefix(i))
  //          println("getNamespaceURI=" + reader.getNamespaceURI(i))
  //          nextEvents()
  //        }
          case XMLStreamConstants.START_DOCUMENT => {
            ~s"CASE START_DOCUMENT"
            val retv = List(StartXmlDocumentEvent(
              reader.getEncoding,
              reader.getVersion,
              reader.isStandalone,
              reader.getCharacterEncodingScheme,
              getLocatorFromReader
            ))
            ~"reader.next()"
            reader.next()
            retv 
          }
          case XMLStreamConstants.END_DOCUMENT => {
            ~s"CASE END_DOCUMENT"
            val retv = List(EndXmlDocumentEvent(getLocatorFromReader))
            retv 
          }
          case XMLStreamConstants.START_ELEMENT => {
            ~s"CASE START_ELEMENT"
            val (qName, attributes, prefixes) = getElementInfoFromReader()
            val locator = getLocatorFromReader
            ~s"reader.next()"
            reader.next()
            val (value, peekQueue) = peekParseElementValue()
            ~s"Peek parsed value=$value peekQueue=$peekQueue"
            val retv = peekQueue :::
              StartXmlElementEvent(XmlElement(qName, value, attributes, prefixes, Some(locator)), locator) :: Nil
            retv 
          }
          case XMLStreamConstants.END_ELEMENT => {
            ~s"CASE END_ELEMENT"
            val retv = List(EndXmlElementEvent(getElementQNameFromReader, getLocatorFromReader))
            ~s"reader.next()"
            reader.next()
            retv 
          }
          case XMLStreamConstants.CHARACTERS => {
            ~s"CASE CHARACTERS"
            val retv = List(AddXmlTextEvent(reader.getText(), getLocatorFromReader))
            ~s"reader.next()"
            reader.next()
            retv 
          }
          case XMLStreamConstants.CDATA => {
            ~s"CASE CDATA"
            val retv = List(AddXmlTextEvent(reader.getText(), getLocatorFromReader))
            ~s"reader.next()"
            reader.next()
            retv 
          }
          case XMLStreamConstants.COMMENT => {
            ~s"CASE COMMENT"
            val retv = List(AddXmlCommentEvent(reader.getText(), getLocatorFromReader))
            ~s"reader.next()"
            reader.next()
            retv 
          }
          case _ =>
            log warn s"Unhandled event: $eventType"
            ~s"reader.next()"
            reader.next()
            ~s"Recursing nextEvents"
            nextEvents()
        }
      }
    }

    private def getElementQNameFromReader : XsdQName = {
      val prefix = newXsdNCName(reader.getPrefix())
      val uri = newXsdAnyURI(reader.getNamespaceURI())
      val localName = new XsdNCName(reader.getLocalName())
      new XsdQName(prefix, uri, localName)
    }

    private def getElementInfoFromReader() : (XsdQName, Seq[(XsdQName, String)], Seq[(XsdNCName, XsdAnyURI)]) = {
      log.block(s"getElementInfoFromReader") {

        val qName = getElementQNameFromReader

        ~"Building attributes from reader"
        val attributes = {
          for(i <- 0 until reader.getAttributeCount())
          yield {
            val prefix = newXsdNCName(reader.getPrefix())
            val uri = newXsdAnyURI(reader.getAttributeNamespace(i))
            val localName = new XsdNCName(reader.getAttributeLocalName(i))
            val qName = new XsdQName(prefix, uri, localName)
            val value = reader.getAttributeValue(i)
            qName -> value
          }
        }

        ~"Building prefixes from reader"
        val prefixes = {
          for(i <- 0 until reader.getNamespaceCount())
          yield {
            val prefix = newXsdNCName(reader.getNamespacePrefix(i))
            val uri = newXsdAnyURI(reader.getNamespaceURI(i))
            prefix -> uri
          }
        }
  
        (qName, attributes, prefixes)
      }
    }

    private def newXsdAnyURI(uri: String) = {
      if(uri == null) {
        XmlConstants.NULL_NS_URI
      } else {
        new XsdAnyURI(uri)
      }
    }
    private def newXsdNCName(name : String) = {
      if(name == null) {
        XmlConstants.DEFAULT_NS_PREFIX
      } else {
        new XsdNCName(name)
      }
    }
    private def getLocatorFromReader : XmlFileLocator = {
      val l = reader.getLocation()
      XmlFileLocator(l.getCharacterOffset, l.getColumnNumber, l.getLineNumber, l.getPublicId, l.getSystemId)
    }
    // Peek at the next few XmlEvents - if it is a string of text events followed by an end event then compress
    // the sequence by extracting the combined "value" of the text events and throw away the individual
    // text events
    @tailrec
    private def peekParseElementValue(peekQueue : List[XmlEvent] = Nil) : (Option[String], List[XmlEvent]) = {
      log.begin("peekParseElementValue", Seq("peekQueue#" -> peekQueue))
      val events = nextEvents()
      ~s"MATCH nextEvents=$events"
      events match {
        case List(e:AddXmlTextEvent) => {
          ~"CASE List(e:AddXmlTextEvent)"
          ~s"Got an events list with exactly one AddXmlTextEvent=$e"
          peekParseElementValue(e :: peekQueue)
        }
        case List(e:EndXmlElementEvent) => {
          ~"CASE List(e:EndXmlElementEvent)"
          ~s"Got an events list with exactly one EndXmlElementEvent=$e"
          if(peekQueue.nonEmpty) {
            val result = peekQueue.foldLeft(new StringBuilder) {
              (s,event) =>
                event match {
                  case AddXmlTextEvent(text,_) => s.append(text)
                  case _ => s
                }
            }
            val retv = (Some(result.toString), events)
            log.end("peekParseElementValue", retv)
            retv
          } else {
            val retv = (None, events)
            log.end("peekParseElementValue", retv)
            retv
          }
        }
        case _ => {
          ~"CASE _"
          val retv = (None, events ::: peekQueue)
          log.end("peekParseElementValue", retv)
          retv
        }
      }
    }
  }
}