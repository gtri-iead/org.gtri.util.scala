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

*/package org.gtri.util.scala.xsdbuilder

import org.gtri.util.scala.statemachine._
import elements._
import org.gtri.util.xsddatatypes.XsdQName
import org.gtri.util.scala.xmlbuilder._
import org.gtri.util.scala.statemachine.StateMachine.State.Halted


case class XmlToXsdParser() extends Translator[XmlEvent, XsdEvent]{

  def s0 = NoContextParser()

  import Translator._
  type ParserDelta = Transition[XmlEvent,XsdEvent]
  type ParserState = State.Continuation[XmlEvent,XsdEvent]
  type PartialParser = PartialFunction[XmlEvent,ParserDelta]

  /**
   * Parser to ignore an element. If a further child element with the same qName is encountered, parser will recurse on
   * itself to ignore the child element as well. Returns to parent once the original element's closing tag is
   * encountered.
   * @param qName
   * @param parent
   */
  case class IgnoreElementParser(qName : XsdQName, parent : ParserState) extends ParserState {
    def apply(event : XmlEvent) = {
      event match {
        case add@StartXmlElementEvent(element, locator) if element.qName == qName =>
          Continue(IgnoreElementParser(qName, this))
        case end@EndXmlElementEvent(eventQName, locator) if eventQName == qName =>
          Continue(parent)
        case _ =>
          Continue(this)
      }
    }

    def apply(eoi : EndOfInput) = Halt.fatal("Expected </" + qName + ">")
  }

  /**
   * PartialParser to guard against unexpected XmlEvents.
   * @param next
   * @return
   */
  def guardUnexpectedXmlEvent(next : => ParserState) : PartialParser = {
    // Handle start of unexpected element
    case ev@StartXmlElementEvent(element, locator) =>
      Halt.error(
        message = "Unexpected element '" + element.qName + "'",
        recover = () => Continue(
          state = IgnoreElementParser(element.qName, next),
          metadata = Issue.warn("Ignoring unexpected element '" + element.qName + "'") :: locator :: Nil
        ),
        metadata = locator :: Nil
      )
    // Handle any other unexpected event
    case ev : XmlEvent =>
      Halt.error(
        message = "Unexpected " + ev,
        recover = () => Continue(
          state = next,
          metadata = Issue.warn("Ignoring unexpected " + ev) :: ev.locator :: Nil
        ),
        metadata = ev.locator :: Nil
      )
  }

  /**
   * PartialParser to parse lexical (formatting etc) XmlEvents
   * @param next
   * @return
   */
  def parseLexicalEvents(next: => ParserState) : PartialParser = {
    // Turn whitespace into lexical event
    case ev@AddXmlTextEvent(text, locator) if text.matches("\\s+") =>
      Continue(
        state = next,
        output = AddXsdXmlLexicalEvent(ev, locator) :: Nil
      )
    // Turn comments into lexical event
    case ev@AddXmlCommentEvent(comment, locator) =>
      Continue(
        state = next,
        output = AddXsdXmlLexicalEvent(ev, locator) :: Nil
      )
  }

  def mapHaltedRecover[E <: XsdElement](s: Plan.State[E]) : ParserDelta = {
    s match {
      case q : Plan.State.Continuation[E] =>
        Halt.fatal("Failed to parse " + element)
      case q:  Plan.State.Success[E] =>
        val startEvent = StartXsdEvent(q.value,locator)
        Continue(
          state = next(startEvent),
          output = startEvent :: Nil
        )
      case q:  Plan.State.Halted[E] =>
        Transition(
          state = Halted(
            issues = q.issues,
            optRecover = q.optRecover map { recover => () =>
              val r0 : Plan.Transition[E] = recover()

              val retv : Transition[XmlEvent,XsdEvent] = Halt.fatal("")
              retv
            }
          )
        )
    }
  }
  /**
   * PartialParser to parse the start of an Xml Element
   * @param next
   * @param util
   * @return
   */
  def createStartElementParser[E <: XsdElement](next: StartXsdEvent => ParserState, util : XsdElementUtil[E]) : PartialParser = {
    case ev@StartXmlElementEvent(element, locator) if element.qName == util.qName =>
      val plan : Plan[E] = List(element).toEnumerator compose util.parser
      val result : Plan.Transition[E] = plan.run()
      result.state match {
        case q : Plan.State.Continuation[E] =>
          Halt.fatal("Failed to parse " + element)
        case q:  Plan.State.Success[E] =>
          val startEvent = StartXsdEvent(q.value,locator)
          Continue(
            state = next(startEvent),
            output = startEvent :: Nil
          )
        case q:  Plan.State.Halted[E] =>
          Transition(
            state = Halted(
              issues = q.issues,
              optRecover = q.optRecover map { recover => () =>
                val r0 : Plan.Transition[E] = recover()

                val retv : Transition[XmlEvent,XsdEvent] = Halt.fatal("")
                retv
              }
            )
          )
      }
  }

  /**
   * PartialParser to parse the end of an XmlElement
   * @param e
   * @param next
   * @return
   */
  def createEndEventParser(e : XsdElement, next: EndXsdEvent => ParserState) : PartialParser = {
    case ev@EndXmlElementEvent(eventQName, locator) if eventQName == e.qName =>
      val endEvent = EndXsdEvent(e,locator)
      Continue(
        state = next(endEvent),
        output = endEvent :: Nil
      )
  }

  /**
   * PartialParser to parse the StartXmlDocumentEvent
   * @param next
   * @return
   */
  def parseStartDocEvent(next: => ParserState) : PartialParser = {
    case StartXmlDocumentEvent(encoding, version, isStandAlone, characterEncodingScheme, locator) =>
      Continue(
        state = next,
        output = StartXsdDocumentEvent(encoding, version, isStandAlone, characterEncodingScheme, locator) :: Nil
      )
  }

  /**
   * PartialParser to parse EndXmlDocumentEvent
   * @param next
   * @return
   */
  def parseEndDocEvent(next: => ParserState) : PartialParser = {
    case EndXmlDocumentEvent(locator) =>
      Continue(
        state = next,
        output = EndXsdDocumentEvent(locator) :: Nil
      )
  }

  /**
   * Parser for the starting context - awaiting the StartXmlDocumentEvent
   */
  case class NoContextParser() extends ParserState {
    private val doApply = (
        parseStartDocEvent(DocRootParser(this))
        orElse guardUnexpectedXmlEvent(this)
      )

    def apply(event : XmlEvent) = {
      doApply(event)
    }

    def apply(eoi: EndOfInput) = Succeed()
  }

  /**
   * Parser for the document root - awaiting the xsd:schema element
   * @param parent
   */
  case class DocRootParser(parent : ParserState) extends ParserState {
    private val doApply = (
      createStartElementParser(
        { event =>
          ElementParser(
            util = XsdSchema.util,
            parent = this,
            item = event.item
          )
        },
        XsdSchema.util
      )
      orElse parseEndDocEvent(parent)
      orElse parseLexicalEvents(this)
      orElse guardUnexpectedXmlEvent(this)
    )

    def apply(event : XmlEvent) = doApply(event)

    def apply(eoi : EndOfInput) = Halt.fatal("Expected EndXmlDocumentEvent")
  }

  /**
   * Element parser
   * @param util
   * @param parent
   * @param item
   * @param children
   */
  case class ElementParser(util : XsdElementUtil[XsdElement], parent : ParserState, item : XsdElement, children : Seq[XsdElementUtil[XsdElement]] = Seq.empty) extends ParserState {
    val doApply : PartialParser = {
      val endEventParser = createEndEventParser(item, { _ => parent })
      val childParsers =
        util.allowedChildElements(children).foldLeft(endEventParser) { (z,childUtil) =>
          z orElse createStartElementParser(
            { event =>
              ElementParser(
                util = childUtil,
                parent = this,
                item = event.item
              )
            },
            childUtil
          )
        }
      (
        childParsers
        orElse parseLexicalEvents(this)
        orElse guardUnexpectedXmlEvent(this)
      )
    }

    def apply(event : XmlEvent) = doApply(event)

    def apply(eoi : EndOfInput) = Halt.fatal("Expected </" + util.qName + ">")
  }
}
