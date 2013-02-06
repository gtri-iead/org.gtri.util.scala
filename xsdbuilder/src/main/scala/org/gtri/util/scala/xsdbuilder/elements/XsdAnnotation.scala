package org.gtri.util.scala.xsdbuilder.elements

import org.gtri.util.scala.statemachine._
import org.gtri.util.xsddatatypes._
import org.gtri.util.xsddatatypes.XsdConstants._
import org.gtri.util.scala.xsdbuilder.XmlParser._
import org.gtri.util.scala.xmlbuilder.XmlElement

case class XsdAnnotation(
  id        :   Option[XsdId]                 = None,
  optMetadata  :   Option[XsdElement.Metadata]   = None
) extends XsdElement {

  def qName = XsdAnnotation.util.qName

  def value = None

  def toAttributes = {
    id.map({ id => (ATTRIBUTES.SOURCE.QNAME,id.toString)}).toList
  }

//  def pushTo(contract: XsdContract) {
//    contract.addXsdAnnotation(
//      /* XsdId _id => */id.orNull,
//      /* ImmutableMap<XsdNCName, XsdAnyURI> _prefixToNamespaceURIMap  => */prefixToNamespaceURIMap
//    )
//  }

}

object XsdAnnotation {

  implicit object util extends XsdElementUtil[XsdAnnotation] {

    def qName = ELEMENTS.ANNOTATION.QNAME

    def parser[EE >: XsdAnnotation] : Iteratee[XmlElement, EE] = {
      for{
        element <- QNamePeekParser(qName)
        id <- OptionalAttributePeekParser(ATTRIBUTES.ID.QNAME, XsdId.parseString)
      } yield {
          XsdAnnotation(
            id = id,
            optMetadata = Some(XsdElement.Metadata(element))
          )
      }
    }

    def allowedChildElements(children: Seq[XsdElementUtil[XsdElement]]) = Seq(XsdDocumentation.util, XsdAppInfo.util)

//    def downcast(element: XsdElement) : Option[XsdAnnotation] = element match {
//      case e : XsdAnnotation => Some(e)
//      case _ => None
//    }
  }
}