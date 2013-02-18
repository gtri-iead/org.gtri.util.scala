package org.gtri.util.scala.xsdbuilder.elements

import org.gtri.util.scala.statemachine._
import org.gtri.util.xsddatatypes._
import org.gtri.util.xsddatatypes.XsdConstants._
import org.gtri.util.scala.xsdbuilder.XmlParser._
import org.gtri.util.scala.xmlbuilder.XmlElement

final case class XsdAnnotation(
  optId        :   Option[XsdId]                 = None,
  optMetadata  :   Option[XsdElement.Metadata]   = None
) extends XsdElement {

  def qName = XsdAnnotation.util.qName

  def optValue = None

  def toAttributes = {
    optId.map({ id => (ATTRIBUTES.SOURCE.QNAME,id.toString)}).toList
  }

}

object XsdAnnotation {

  implicit object util extends XsdElementUtil[XsdAnnotation] {

    def qName = ELEMENTS.ANNOTATION.QNAME

    def parser[EE >: XsdAnnotation] : Iteratee[XmlElement, EE] = {
      for{
        element <- QNamePeekParser(qName)
        optId <- OptionalAttributePeekParser(ATTRIBUTES.ID.QNAME, XsdId.parseString)
      } yield {
          XsdAnnotation(
            optId = optId,
            optMetadata = Some(XsdElement.Metadata(element))
          )
      }
    }

    def allowedChildElements(children: Seq[XsdElementUtil[XsdElement]]) = Seq(XsdDocumentation.util, XsdAppInfo.util)
  }
}