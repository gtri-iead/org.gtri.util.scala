package org.gtri.util.scala.xsdbuilder.elements

import org.gtri.util.scala.statemachine._
import org.gtri.util.xsddatatypes._
import org.gtri.util.xsddatatypes.XsdCodes._
import org.gtri.util.xsddatatypes.XsdConstants._
import org.gtri.util.scala.xsdbuilder.XmlParser._
import org.gtri.util.scala.xmlbuilder.XmlElement
import org.gtri.util.scala.xsdbuilder.XsdAllOrNone

case class XsdSimpleType(
  optId            :   Option[XsdId]                                             = None,
  name             :   XsdName,
  optFinalCodes    :   Option[XsdAllOrNone[SimpleTypeFinalCode]]   = None,
  optMetadata      :   Option[XsdElement.Metadata]                               = None
) extends XsdElement {

  def   qName   =   XsdSimpleType.util.qName

  def   optValue   =   None

  def util = XsdAnnotation.util

  def getAttributeValue(qName: XsdQName) = {
    qName.getLocalName match {
      case ATTRIBUTES.ID.LOCALNAME => optId
      case ATTRIBUTES.NAME.LOCALNAME => Some(name)
      case ATTRIBUTES.FINAL.LOCALNAME => optFinalCodes
    }
  }
//  def toAttributes = {
//    optId.map({ id => ATTRIBUTES.ID.QNAME -> id.toString}).toList :::
//    List(ATTRIBUTES.NAME.QNAME -> name.toString) :::
//    optFinalCodes.map({ finalCodes => ATTRIBUTES.FINAL.QNAME -> finalCodes.fold({ _.toString },{ _.mkString(" ") })}).toList
//  }
}

object XsdSimpleType {

  implicit object util extends XsdElementUtil[XsdSimpleType] {
    def qName = ELEMENTS.SIMPLETYPE.QNAME

    def randomString = java.lang.Long.toHexString(java.lang.Double.doubleToLongBits(java.lang.Math.random()))
    def genRandomName = new XsdName(new StringBuilder().append("SimpleTypeGeneratedName").append(randomString).append(randomString).toString())

    def parser[EE >: XsdSimpleType] : Parser[XmlElement,EE] = {
      for{
        element <- Parser.tell[XmlElement]
        optId <- optionalAttributeParser(ATTRIBUTES.ID.QNAME, Try.parser(XsdId.parseString))
        name <- requiredAttributeParser(ATTRIBUTES.NAME.QNAME,Try.parser(XsdName.parseString), Some(genRandomName _))
        optFinalCodes <- optionalAttributeParser(ATTRIBUTES.FINAL.QNAME, XsdAllOrNone.parser(Try.parser(SimpleTypeFinalCode.parseString)))
      } yield
          XsdSimpleType(
            optId = optId,
            name = name,
            optFinalCodes = optFinalCodes,
            optMetadata = Some(XsdElement.Metadata(element))
          )
    }

    def attributes = Seq(
      ATTRIBUTES.ID.QNAME,
      ATTRIBUTES.NAME.QNAME,
      ATTRIBUTES.FINAL.QNAME
    )

    def allowedChildElements(children: Seq[XsdElementUtil[XsdElement]]) = {
      Seq(XsdAnnotation.util)
    }

  }
}