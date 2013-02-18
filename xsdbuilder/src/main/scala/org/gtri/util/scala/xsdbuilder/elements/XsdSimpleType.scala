package org.gtri.util.scala.xsdbuilder.elements

import org.gtri.util.scala.statemachine._
import org.gtri.util.xsddatatypes._
import org.gtri.util.xsddatatypes.XsdCodes._
import org.gtri.util.xsddatatypes.XsdConstants._
import org.gtri.util.scala.xsdbuilder.XmlParser._
import org.gtri.util.scala.xmlbuilder.XmlElement

case class XsdSimpleType(
  optId            :   Option[XsdId]                                             = None,
  name             :   XsdName,
  optFinalCodes    :   Option[Either[AllOrNoneCode, Set[SimpleTypeFinalCode]]]   = None,
  optMetadata      :   Option[XsdElement.Metadata]                               = None
) extends XsdElement {
  def   qName   =   XsdSimpleType.util.qName
  def   optValue   =   None

  def toAttributes = {
    optId.map({ id => ATTRIBUTES.ID.QNAME -> id.toString}).toList :::
    List(ATTRIBUTES.NAME.QNAME -> name.toString) :::
    optFinalCodes.map({ finalCodes => ATTRIBUTES.FINAL.QNAME -> finalCodes.fold({ _.toString },{ _.mkString(" ") })}).toList
  }
}

object XsdSimpleType {

  implicit object util extends XsdElementUtil[XsdSimpleType] {
    def qName = ELEMENTS.SIMPLETYPE.QNAME

    def randomString = java.lang.Long.toHexString(java.lang.Double.doubleToLongBits(java.lang.Math.random()))
    def genRandomName = new XsdName(new StringBuilder().append("SimpleTypeGeneratedName").append(randomString).append(randomString).toString())

    def parser[EE >: XsdSimpleType] : Iteratee[XmlElement,EE] = {
      for{
        element <- QNamePeekParser(qName)
        optId <- OptionalAttributePeekParser(ATTRIBUTES.ID.QNAME, XsdId.parseString)
        name <- RequiredAttributePeekParser(ATTRIBUTES.NAME.QNAME,XsdName.parseString, Some(genRandomName _))
        optFinalCodes <- OptionalAttributePeekParser(ATTRIBUTES.FINAL.QNAME, parseAllOrNone(SimpleTypeFinalCode.parseString))
      } yield
          XsdSimpleType(
            optId = optId,
            name = name,
            optFinalCodes = optFinalCodes,
            optMetadata = Some(XsdElement.Metadata(element))
          )
    }

    def allowedChildElements(children: Seq[XsdElementUtil[XsdElement]]) = {
      Seq(XsdAnnotation.util)
    }

  }
}