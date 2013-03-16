package org.gtri.util.scala.xsdbuilder.elements

import org.gtri.util.scala.statemachine._
import org.gtri.util.xsddatatypes._
import org.gtri.util.xsddatatypes.XsdConstants._
import org.gtri.util.scala.xmlbuilder.{XmlNamespaceContext, XmlElement}
import org.gtri.util.scala.xsdbuilder.XmlParser._
import org.gtri.util.scala.xsdbuilder.elements.XsdGenVal._
import org.gtri.util.xsddatatypes.XsdQName.PrefixToNamespaceURIResolver

final case class XsdAttribute(
  optId        :   Option[XsdId]                 = None,
  name         :   XsdName,
  _type        :   XsdQName,
  optDefault   :   Option[String]                = None,
  optFixed     :   Option[String]                = None,
  optMetadata  :   Option[XsdElement.Metadata]   = None
) extends XsdElement {

  def optValue = None

  def util = XsdAttribute.util

  def toAttributesMap(context: Seq[XmlNamespaceContext]) = {
    {
      optId.map { (ATTRIBUTES.ID.QNAME -> _.toString) } ::
      Some(ATTRIBUTES.NAME.QNAME -> name.toString) ::
      Some(ATTRIBUTES.TYPE.QNAME -> _type.toStringWithPrefix(context)) ::
      optDefault.map { (ATTRIBUTES.DEFAULT.QNAME -> _ )} ::
      optFixed.map { (ATTRIBUTES.FIXED.QNAME -> _ )} ::
        Nil
    }.flatten.toMap
  }
}

object XsdAttribute {

  implicit object util extends XsdElementUtil[XsdAttribute] {

    def qName = ELEMENTS.ANNOTATION.QNAME


    def parser[EE >: XsdAttribute](context: Seq[XmlNamespaceContext]) : Parser[XmlElement, EE] = {
      for{
        element <- Parser.tell[XmlElement]
        optId <- optionalAttributeParser(ATTRIBUTES.ID.QNAME, Try.parser(XsdId.parseString))
        name <- requiredAttributeParser[XsdName](ATTRIBUTES.NAME.QNAME, Try.parser(XsdName.parseString), Some(() => genRandomName))
        // TODO: how to resolve qname prefix properly?
        _type <- requiredAttributeParser(ATTRIBUTES.TYPE.QNAME, Try.parser((s:String) => XsdQName.parseStringWithPrefix(s,context)),Some(() => XsdConstants.BUILTIN_DATATYPES.STRING.QNAME))
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

    def attributes = Set(
      ATTRIBUTES.ID.QNAME,
      ATTRIBUTES.NAME.QNAME,
      ATTRIBUTES.TYPE.QNAME,
      ATTRIBUTES.DEFAULT.QNAME,
      ATTRIBUTES.FIXED.QNAME
    )

    def allowedChildElements(children: Seq[XsdElementUtil[XsdElement]]) = Seq(XsdAppInfo.util)

    //    def downcast(element: XsdElement) : Option[XsdAttribute] = element match {
    //      case e : XsdAttribute => Some(e)
    //      case _ => None
    //    }
  }
}