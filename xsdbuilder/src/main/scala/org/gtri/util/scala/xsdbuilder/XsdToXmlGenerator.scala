/*
    Copyright 2012 Georgia Tech Research Institute

    Author: lance.gatlin@gtri.gatech.edu

    This file is part of org.gtri.util.xsdbuilder library.

    org.gtri.util.xsdbuilder library is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    org.gtri.util.xsdbuilder library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with org.gtri.util.xsdbuilder library. If not, see <http://www.gnu.org/licenses/>.

*/
package org.gtri.util.scala.xsdbuilder

import elements._
import org.gtri.util.scala.statemachine._
import org.gtri.util.scala.xmlbuilder._

case class XsdToXmlGenerator() extends Translator[XsdEvent, XmlEvent]{

  def s0 = NoContextGenerator()

  import Translator._
  type Generator = State.Continuation[XsdEvent,XmlEvent]
  type GeneratorResult = Transition[XsdEvent,XmlEvent]
  type PartialGenerator = PartialFunction[XsdEvent, GeneratorResult]

  /**
   * PartialGenerator to guard against unexpected XsdEvents
   * @param next
   * @return
   */
  def guardUnexpectedXsdEvent(next : => Generator) : PartialGenerator = {
    case ev : XsdEvent =>
      Halt.error(
        message = "Unexpected '" + ev + "'",
        recover = () => Continue(
          state = next,
          metadata = Issue.warn("Ignoring unexpected '" + ev + "'") :: ev.locator :: Nil
        ),
        metadata = ev.locator :: Nil
      )
  }

  /**
   * PartialGenerator to convert Xsd lexical events back to XmlEvents
   * @param next
   * @return
   */
  def genLexicalEvents(next : => Generator) : PartialGenerator = {
    case ev@AddXsdXmlLexicalEvent(xmlEvent, locator) =>
      Continue(
        state = next,
        output = xmlEvent :: Nil
      )
  }

  /**
   * PartialGenerator to generate a StartXmlElementEvent for a StartXsdEvent of a particular type of element
   * @param next
   * @param util
   * @return
   */
  def createStartEventGenerator(stack: List[XmlElement], next: List[XmlElement] => Generator, util : XsdElementUtil[XsdElement]) : PartialGenerator = {
    case ev@StartXsdEvent(item, locator) if item.util.qName == util.qName =>
      val element = item.toXmlElement(stack)
      val newStack = element :: stack
      Continue(
        state = next(newStack),
        output = StartXmlElementEvent(element, locator) :: Nil
      )
  }

  /**
   * PartialGenerator to generate a EndXmlElementEvent for a EndXsdEvent of particular type of element
   * @param next
   * @param util
   * @return
   */
  def createEndEventGenerator(next: => Generator, util : XsdElementUtil[XsdElement]) : PartialGenerator = {
    case ev@EndXsdEvent(item, locator) if item.util.qName == util.qName =>
      Continue(
        state = next,
        output = EndXmlElementEvent(util.qName, locator) :: Nil
      )
  }

  /**
   * PartialGenerator to generate a StartXmlDocumentEvent from a StartXsdDocumentEvent
   * @param next
   * @return
   */
  def parseStartDocumentEvent(next : => Generator) : PartialGenerator = {
    case StartXsdDocumentEvent(encoding, version, isStandAlone, characterEncodingScheme, locator) =>
      Continue(
        state = next,
        output = StartXmlDocumentEvent(encoding, version, isStandAlone, characterEncodingScheme, locator) :: Nil
      )
  }

  /**
   * PartialGenerator to generate a EndXmlDocumentEvent from a EndXsdDocumentEvent
   * @param next
   * @return
   */
  def parseEndDocumentEvent(next : => Generator) : PartialGenerator = {
    case EndXsdDocumentEvent(locator) =>
      Continue(
        state = next,
        output = EndXmlDocumentEvent(locator) :: Nil
      )
  }

  /**
   * The starting Generator - awaiting the StartXsdDocumentEvent
   */
  case class NoContextGenerator() extends Generator {
    
    private val doApply = (
      parseStartDocumentEvent(DocRootGenerator(this))
        orElse guardUnexpectedXsdEvent(this)
      )

    def apply(event : XsdEvent) = doApply(event)

    def apply(eoi: EndOfInput) = Succeed()
  }

  /**
   * The document root generator - awaiting the StartXsdEvent(XsdSchema)
   * @param parent
   */
  case class DocRootGenerator(parent : Generator) extends Generator {
    
    private val doApply = (
        createStartEventGenerator(Nil, stack => ElementGenerator(stack,XsdSchema.util,this), XsdSchema.util)
        orElse parseEndDocumentEvent(parent)
        orElse genLexicalEvents(this)
        orElse guardUnexpectedXsdEvent(this)
      )

    def apply(event : XsdEvent) = doApply(event)

    def apply(eoi : EndOfInput) = Halt.fatal("Expected EndXsdDocumentEvent")
  }

  /**
   * Element generator
   * @param util
   * @param parent
   */
  case class ElementGenerator(stack: List[XmlElement], util : XsdElementUtil[XsdElement], parent: Generator) extends Generator {
    val doApply : PartialGenerator = {
      val endEventGenerator = createEndEventGenerator(parent,util)
      val childGenerators =
        util.allowedChildElements(Seq.empty).foldLeft(endEventGenerator)
        { (z,childUtil) =>
          z orElse createStartEventGenerator(
            stack=stack,
            next=newStack => ElementGenerator(newStack,childUtil, this),
            util=childUtil
          )
        }
      (
        childGenerators
        orElse genLexicalEvents(this)
        orElse guardUnexpectedXsdEvent(this)
      )
    }

    def apply(event : XsdEvent) = doApply(event)

    def apply(eoi : EndOfInput) = Halt.fatal("Expected EndXsdEvent(" + util.qName.getLocalName + ")")
  }
}
