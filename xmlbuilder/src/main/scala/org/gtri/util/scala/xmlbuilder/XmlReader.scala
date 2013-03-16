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
import org.gtri.util.xsddatatypes._
import org.gtri.util.scala.statemachine._
import org.gtri.util.scala.statemachine.Enumerator._
import java.io.InputStream
import annotation.tailrec
import scala.collection.immutable.Seq


final case class XmlReader(
  in              :   InputStream,
  chunkSize       :   Int               = Enumerator.STD_CHUNK_SIZE
) extends Enumerator[XmlEvent] {

  require(chunkSize > 0)

  private val reader = XMLInputFactory.newInstance().createXMLStreamReader(in)
  private val totalByteSize = in.available()

  def s0() = XmlReaderCont()

  final case class XmlReaderCont() extends State.Continuation[XmlEvent] {

    def apply(x : Unit) = {
      try {
        // TODO: what happens if reader fails?
        val buffer = new collection.mutable.ArrayBuffer[XmlEvent](chunkSize)
        while(buffer.size < chunkSize && reader.hasNext) {
          buffer ++= nextEvents().reverse
        }

        val nextProgress = {
          if(totalByteSize > 0) {
            // TODO: there are reporting bugs here when reporting Progress(getCharacterOffset,in.available) where getCharacterOffset sometimes > in.available
            val charOffset = reader.getLocation.getCharacterOffset
            if(charOffset == -1) {
              new Progress(totalByteSize,totalByteSize)
            } else {
              new Progress(reader.getLocation.getCharacterOffset,totalByteSize)
            }
          } else {
            Progress.empty
          }
        }

        if(buffer.isEmpty) {
          Succeed(
            output = Seq(EndXmlDocumentEvent(getLocatorFromReader)),
            metadata = Seq(nextProgress)
          )
        } else {
          // TODO: buffer may exceed chunkSize, should limit this by passing remaining back to state
          Continue(
            state = this,
            output = buffer.toVector,
            metadata = Seq(nextProgress)
          )
        }
      } catch {
        case ex : Exception =>
          Halt.fatal(ex.getMessage, Some(ex))
      }
    }

    def apply(eoi : EndOfInput) = {
      try {
        reader.close()
        Succeed()
      } catch {
        case ex : Exception =>
          Halt.fatal(ex.getMessage, Some(ex))
      }
    }

    private def nextEvents() : List[XmlEvent] = {
      val eventType = reader.getEventType()
      eventType match {
  // TODO: reader doesn't return these events for some reason ?
  //        case XMLStreamConstants.ATTRIBUTE =>
  //        case XMLStreamConstants.NAMESPACE =>
        case XMLStreamConstants.START_DOCUMENT => {
          val retv = List(StartXmlDocumentEvent(
            reader.getEncoding,
            reader.getVersion,
            reader.isStandalone,
            reader.getCharacterEncodingScheme,
            getLocatorFromReader
          ))
          reader.next()
          retv
        }
        case XMLStreamConstants.END_DOCUMENT =>
          List(EndXmlDocumentEvent(getLocatorFromReader))
        case XMLStreamConstants.START_ELEMENT =>
          val (qName, attributes, prefixes) = getElementInfoFromReader()
          val locator = getLocatorFromReader
          reader.next()
          val (value, peekQueue) = peekParseElementValue()
          peekQueue ::: StartXmlElementEvent(XmlElement(qName, value, attributes, prefixes, Some(locator)), locator) :: Nil
        case XMLStreamConstants.END_ELEMENT =>
          val retv = List(EndXmlElementEvent(getElementQNameFromReader, getLocatorFromReader))
          reader.next()
          retv
        case XMLStreamConstants.CHARACTERS =>
          val retv = List(AddXmlTextEvent(reader.getText(), getLocatorFromReader))
          reader.next()
          retv
        case XMLStreamConstants.CDATA =>
          val retv = List(AddXmlTextEvent(reader.getText(), getLocatorFromReader))
          reader.next()
          retv
        case XMLStreamConstants.COMMENT =>
          val retv = List(AddXmlCommentEvent(reader.getText(), getLocatorFromReader))
          reader.next()
          retv
        case _ =>
          reader.next()
          nextEvents()
      }
    }

    private def getElementQNameFromReader : XsdQName = {
      val prefix = newXsdNCName(reader.getPrefix())
      val uri = newXsdAnyURI(reader.getNamespaceURI())
      val localName = new XsdNCName(reader.getLocalName())
      new XsdQName(prefix, uri, localName)
    }

    private def getElementInfoFromReader() : (XsdQName, Seq[(XsdQName, String)], Seq[(XsdNCName, XsdAnyURI)]) = {
      val qName = getElementQNameFromReader

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

    // Handle null
    private def newXsdAnyURI(uri: String) = {
      if(uri == null) {
        XmlConstants.NULL_NS_URI
      } else {
        new XsdAnyURI(uri)
      }
    }

    // Handle null
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
    // the sequence by extracting the combined "optValue" of the text events and throw away the individual
    // text events
    @tailrec
    private def peekParseElementValue(peekQueue : List[XmlEvent] = Nil) : (Option[String], List[XmlEvent]) = {
      val events = nextEvents()
      events match {
        case List(e:AddXmlTextEvent) =>
          peekParseElementValue(e :: peekQueue)
        case List(e:EndXmlElementEvent) =>
          if(peekQueue.nonEmpty) {
            val result = peekQueue.foldLeft(new StringBuilder) {
              (s,event) =>
                event match {
                  case AddXmlTextEvent(text,_) => s.append(text)
                  case _ => s
                }
            }
            val retv = (Some(result.toString), events)
            retv
          } else {
            val retv = (None, events)
            retv
          }
        case _ =>
          (None, events ::: peekQueue)
      }
    }
  }
}