package org.gtri.util.scala.xsdbuilder.elements

import org.gtri.util.scala.statemachine._
import org.gtri.util.xsddatatypes._
import org.gtri.util.xsddatatypes.XsdConstants._
import org.gtri.util.scala.xsdbuilder.XmlParser._
import org.gtri.util.scala.xmlbuilder.XmlElement

final case class XsdAttribute(
  optId        :   Option[XsdId]                 = None,
  name         :   XsdName,
  _type        :   XsdQName,
  optDefault   :   Option[String]                = None,
  optFixed     :   Option[String]                = None,
  optMetadata  :   Option[XsdElement.Metadata]   = None
) extends XsdElement {

  def qName = XsdAttribute.util.qName

  def optValue = None

  def util = XsdAttribute.util

  def getAttributeValue(qName: XsdQName) = {
    qName.getLocalName match {
      case ATTRIBUTES.ID.LOCALNAME => optId
      case ATTRIBUTES.NAME.LOCALNAME => Some(name)
      case ATTRIBUTES.TYPE.LOCALNAME => Some(_type)
      case ATTRIBUTES.DEFAULT.LOCALNAME => optDefault
      case ATTRIBUTES.FIXED.LOCALNAME => optFixed
    }
  }
//  def toAttributes = {
//    optId.map({ id => (ATTRIBUTES.SOURCE.QNAME,id.toString)}).toList
//  }
}

object XsdAttribute {

  implicit object util extends XsdElementUtil[XsdAttribute] {

    def qName = ELEMENTS.ANNOTATION.QNAME

    def randomString = java.lang.Long.toHexString(java.lang.Double.doubleToLongBits(java.lang.Math.random()))
    def genRandomName = new XsdName(new StringBuilder().append("AttributeGeneratedName").append(randomString).append(randomString).toString())

    def parser[EE >: XsdAttribute] : Parser[XmlElement, EE] = {
      for{
        element <- Parser.tell[XmlElement]
        optId <- optionalAttributeParser(ATTRIBUTES.ID.QNAME, Try.parser(XsdId.parseString))
        name <- requiredAttributeParser[XsdName](ATTRIBUTES.NAME.QNAME, Try.parser(XsdName.parseString), Some(() => genRandomName))
        // TODO: how to resolve qname prefix properly?
        _type <- requiredAttributeParser(ATTRIBUTES.TYPE.QNAME, Try.parser((s:String) => XsdQName.parseStringWithPrefix(s,element)),Some(() => XsdConstants.BUILTIN_DATATYPES.STRING.QNAME))
        optDefault <- optionalAttributeParser(ATTRIBUTES.DEFAULT.QNAME, Parser.tell[String])
        optFixed <- optionalAttributeParser(ATTRIBUTES.FIXED.QNAME, Parser.tell[String])
      } yield {
        XsdAttribute(
          optId = optId,
          name = name,
          _type = _type,
          optDefault = optDefault,
          optFixed = optFixed,
          optMetadata = Some(XsdElement.Metadata(element))
        )
      }
    }

    def attributes = Seq(
      ATTRIBUTES.ID.QNAME,
      ATTRIBUTES.NAME.QNAME,
      ATTRIBUTES.TYPE.QNAME,
      ATTRIBUTES.DEFAULT.QNAME,
      ATTRIBUTES.FIXED.QNAME
    )

    def allowedChildElements(children: Seq[XsdElementUtil[XsdElement]]) = Seq(XsdDocumentation.util, XsdAppInfo.util)

    //    def downcast(element: XsdElement) : Option[XsdAttribute] = element match {
    //      case e : XsdAttribute => Some(e)
    //      case _ => None
    //    }
  }
}