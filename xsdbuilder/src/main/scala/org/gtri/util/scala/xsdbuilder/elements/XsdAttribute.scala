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

  def toAttributes = {
    optId.map({ id => (ATTRIBUTES.SOURCE.QNAME,id.toString)}).toList
  }
}

object XsdAttribute {

  implicit object util extends XsdElementUtil[XsdAttribute] {

    def qName = ELEMENTS.ANNOTATION.QNAME

    def randomString = java.lang.Long.toHexString(java.lang.Double.doubleToLongBits(java.lang.Math.random()))
    def genRandomName = new XsdName(new StringBuilder().append("AttributeGeneratedName").append(randomString).append(randomString).toString())

    def parser[EE >: XsdAttribute] : Iteratee[XmlElement, EE] = {
      for{
        element <- QNamePeekParser(qName)
        optId <- OptionalAttributePeekParser(ATTRIBUTES.ID.QNAME, XsdId.parseString)
        name <- RequiredAttributePeekParser(ATTRIBUTES.NAME.QNAME, XsdName.parseString, Some(() => genRandomName _))
        _type <- RequiredAttributePeekParser(ATTRIBUTES.TYPE.QNAME, { (s:String) => XsdQName.parseStringWithPrefix(s,element) },Some(() => XsdConstants.BUILTIN_DATATYPES.STRING.QNAME))
      } yield {
        XsdAttribute(
          optId = optId,
          name = name,
          optMetadata = Some(XsdElement.Metadata(element))
        )
      }
    }

    def allowedChildElements(children: Seq[XsdElementUtil[XsdElement]]) = Seq(XsdDocumentation.util, XsdAppInfo.util)

    //    def downcast(element: XsdElement) : Option[XsdAttribute] = element match {
    //      case e : XsdAttribute => Some(e)
    //      case _ => None
    //    }
  }
}