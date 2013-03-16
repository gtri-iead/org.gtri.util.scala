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

import javax.xml.XMLConstants
import org.gtri.util.scala.statemachine._
import org.gtri.util.scala.statemachine.Iteratee._
import java.io.OutputStream
import net.sf.saxon.s9api.Processor
import net.sf.saxon.s9api.Serializer

final case class XmlWriter(out : OutputStream) extends Iteratee[XmlEvent, Unit] {

  // Build a writer using saxon to get nice pretty printing
  private val writer = {
    val p = new Processor(false)
    val s = new Serializer(out)
    s.setProcessor(p)
    s.setOutputProperty(Serializer.Property.ENCODING, "UTF-8")
    s.setOutputProperty(Serializer.Property.INDENT, "yes")
    s.setOutputProperty(Serializer.Property.SAXON_INDENT_SPACES, "2")
    s.setOutputProperty(Serializer.Property.SAXON_LINE_LENGTH, "80")

    s.getXMLStreamWriter()
  }

  def s0 = XmlWriterCont(Nil)

  final case class XmlWriterCont(stack : List[XmlElement]) extends State.Continuation[XmlEvent, Unit] {

    def apply(xmlEvent: XmlEvent) = {
      try {
        xmlEvent match {
          case e:StartXmlDocumentEvent => {
            writer.writeStartDocument()
            Continue(this)
          }
          case e:EndXmlDocumentEvent => {
            writer.writeEndDocument()
            Continue(this)
          }
          case e:AddXmlCommentEvent => {
            writer.writeComment(e.comment)
            Continue(this)
          }
          case e:StartXmlElementEvent => {
            val newStack = e.element :: stack

            val qName = e.element.qName
            val localName = qName.getLocalName.toString
            val nsURI = qName.getNamespaceURI.toString
            val optionPrefix = Option(qName.resolvePrefix(newStack)).map { _.toString }
            val prefix = optionPrefix.getOrElse { XMLConstants.DEFAULT_NS_PREFIX }
            writer.writeStartElement(prefix, localName, nsURI)

            for((namespacePrefix, namespaceURI) <- e.element.orderedPrefixToNamespaceURITuples) {
              // Skip the prefix for the element
              if(prefix != namespacePrefix.toString) {
                val namespacePrefixString = namespacePrefix.toString
                val namespaceURIString = namespaceURI.toString
                writer.writeNamespace(namespacePrefixString, namespaceURIString)
              }
            }

            for((qName,value) <- e.element.orderedAttributes) {
              val localName = qName.getLocalName.toString
              val nsURI = qName.getNamespaceURI.toString
              val optionPrefix = Option(qName.resolvePrefix(newStack)).map { _.toString }
              val prefix = optionPrefix.getOrElse { XMLConstants.DEFAULT_NS_PREFIX }
              writer.writeAttribute(prefix, nsURI, localName, value)
            }

            val value = e.element.optValue
            if(value.isDefined) {
              val v = value.get
              writer.writeCharacters(v)
            }
            Continue(XmlWriterCont(newStack))
          }
          case e:EndXmlElementEvent =>
            writer.writeEndElement()
            Continue(XmlWriterCont(stack.tail))
          case e:AddXmlTextEvent =>
            writer.writeCharacters(e.text)
            Continue(this)
        }
      } catch {
        case ex : Exception =>
          Halt.fatal(ex.getMessage, Some(ex))
      }
    }

    def apply(eoi : EndOfInput) = {
      try {
        writer.flush()
        writer.close()
        if(stack.isEmpty) {
          Succeed(())
        } else {
          Halt.fatal("Expected closing tag for " + stack.last)
        }
      } catch {
        case ex : Exception =>
          Halt.fatal(ex.getMessage, Some(ex))
      }
    }
  }

}