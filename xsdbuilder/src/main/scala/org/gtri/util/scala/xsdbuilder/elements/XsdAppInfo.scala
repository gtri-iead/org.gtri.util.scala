package org.gtri.util.scala.xsdbuilder.elements

import org.gtri.util.scala.statemachine._
import org.gtri.util.xsddatatypes._
import org.gtri.util.xsddatatypes.XsdConstants._
import org.gtri.util.scala.xsdbuilder.XmlParser._
import org.gtri.util.scala.xmlbuilder.XmlElement

final case class XsdAppInfo(
  optSource    :   Option[XsdAnyURI]             = None,
  optMetadata  :   Option[XsdElement.Metadata]   = None
) extends XsdElement {

  def qName = XsdAppInfo.util.qName

  def optValue = None

  def toAttributes = {
    optSource.map({ source => (XsdConstants.ATTRIBUTES.SOURCE.QNAME,source.toString)}).toList
  }

}

object XsdAppInfo {

  implicit object util extends XsdElementUtil[XsdAppInfo] {

    def qName = ELEMENTS.APPINFO.QNAME

    def parser[EE >: XsdAppInfo] : Iteratee[XmlElement,EE] = {
      for{
        element <- QNamePeekParser(qName)
        optSource <- OptionalAttributePeekParser(ATTRIBUTES.SOURCE.QNAME, XsdAnyURI.parseString)
      } yield
          XsdAppInfo(
            optSource = optSource,
            optMetadata = Some(XsdElement.Metadata(element))
          )
    }


    def allowedChildElements(children: Seq[XsdElementUtil[XsdElement]]) = Seq()
  }
}