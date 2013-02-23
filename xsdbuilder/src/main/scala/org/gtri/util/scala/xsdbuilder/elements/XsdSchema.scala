package org.gtri.util.scala.xsdbuilder.elements

import org.gtri.util.scala.statemachine._
import org.gtri.util.xsddatatypes._
import org.gtri.util.xsddatatypes.XsdCodes._
import org.gtri.util.xsddatatypes.XsdConstants._
import org.gtri.util.scala.xsdbuilder.XmlParser._
import org.gtri.util.scala.xmlbuilder.XmlElement
import org.gtri.util.scala.xsdbuilder.XsdAllOrNone

case class XsdSchema(
  optId                     :   Option[XsdId]                         = None,
  targetNamespace           :   XsdAnyURI,
  version                   :   XsdToken,
  optAttributeFormDefault   :   Option[FormChoiceCode]                = None,
  optElementFormDefault     :   Option[FormChoiceCode]                = None,
  optBlockDefaultCodes      :   Option[XsdAllOrNone[BlockDefaultCode]]   = None,
  optFinalDefaultCodes      :   Option[XsdAllOrNone[FinalDefaultCode]]   = None,
  optXmlLang                :   Option[XsdToken]                      = None,
  optMetadata               :   Option[XsdElement.Metadata]           = None
) extends XsdElement {

  def   qName      =   XsdSchema.util.qName
  def   optValue   =   None

//  def toAttributes = {
//    optId.map({ id => ATTRIBUTES.ID.QNAME -> id.toString}).toList :::
//    List(ATTRIBUTES.VERSION.QNAME -> version.toString) :::
//    List(ATTRIBUTES.TARGETNAMESPACE.QNAME -> targetNamespace.toString) :::
//    optAttributeFormDefault.map({ attributeFormDefault => ATTRIBUTES.ATTRIBUTEFORMDEFAULT.QNAME -> attributeFormDefault.toString}).toList :::
//    optElementFormDefault.map({ elementFormDefault => ATTRIBUTES.ELEMENTFORMDEFAULT.QNAME -> elementFormDefault.toString}).toList :::
//    optBlockDefaultCodes.map({ blockDefaultCodes => ATTRIBUTES.BLOCKDEFAULT.QNAME -> blockDefaultCodes.fold({ _.toString },{ _.mkString(" ") })}).toList :::
//    optFinalDefaultCodes.map({ finalDefaultCodes => ATTRIBUTES.FINALDEFAULT.QNAME -> finalDefaultCodes.fold({ _.toString },{ _.mkString(" ") })}).toList :::
//    optXmlLang.map({ xml_lang => ATTRIBUTES.XML_LANG.QNAME -> xml_lang.toString}).toList
//  }
  def util = XsdSchema.util

  def getAttributeValue(qName: XsdQName) = {
    qName.getLocalName match {
      case ATTRIBUTES.ID.LOCALNAME => optId
      case ATTRIBUTES.TARGETNAMESPACE.LOCALNAME => Some(targetNamespace.toString)
      case ATTRIBUTES.VERSION.LOCALNAME => Some(version.toString)
      case ATTRIBUTES.ATTRIBUTEFORMDEFAULT.LOCALNAME => optAttributeFormDefault
      case ATTRIBUTES.ELEMENTFORMDEFAULT.LOCALNAME => optElementFormDefault
      case ATTRIBUTES.BLOCKDEFAULT.LOCALNAME => optBlockDefaultCodes
      case ATTRIBUTES.FINALDEFAULT.LOCALNAME => optFinalDefaultCodes
      case ATTRIBUTES.XML_LANG.LOCALNAME => optXmlLang
    }
  }
}

object XsdSchema {

  implicit object util extends XsdElementUtil[XsdSchema] {
//    def apply(qNameToAnyMap : Map[XsdQName,Any]) : XsdSchema = {
//      XsdSchema(
//        optId = qNameToAnyMap.mapValues({ _.asInstanceOf[XsdId]}).get(ATTRIBUTES.ID.QNAME),
//        targetNamespace = qNameToAnyMap.mapValues({ _.asInstanceOf[XsdAnyURI]}).apply(ATTRIBUTES.ID.QNAME),
//        version = qNameToAnyMap.mapValues({ _.asInstanceOf[XsdToken]}).apply(ATTRIBUTES.ID.QNAME),
//        optAttributeFormDefault = qNameToAnyMap.mapValues({ _.asInstanceOf[FormChoiceCode]}).get(ATTRIBUTES.ID.QNAME),
//        optElementFormDefault = qNameToAnyMap.mapValues({ _.asInstanceOf[FormChoiceCode]}).get(ATTRIBUTES.ID.QNAME),
//        optBlockDefaultCodes = qNameToAnyMap.mapValues({ _.asInstanceOf[XsdAllOrNone[BlockDefaultCode]]}).get(ATTRIBUTES.ID.QNAME),
//        optFinalDefaultCodes = qNameToAnyMap.mapValues({ _.asInstanceOf[XsdAllOrNone[FinalDefaultCode]]}).get(ATTRIBUTES.ID.QNAME),
//        optXmlLang = qNameToAnyMap.mapValues({ _.asInstanceOf[XsdToken]}).get(ATTRIBUTES.ID.QNAME),
//        optMetadata = Some(XsdElement.Metadata(element))
//      )
//    }
    def qName = ELEMENTS.SCHEMA.QNAME

    def randomString = java.lang.Long.toHexString(java.lang.Double.doubleToLongBits(java.lang.Math.random()))
    def genRandomURN = new XsdAnyURI(new StringBuilder().append("urn:").append(randomString).append(":").append(randomString).toString())

    def parser[EE >: XsdSchema] : Parser[XmlElement,EE] =
      for{
        element <- Parser.tell[XmlElement]
        optId <- optionalAttributeParser(ATTRIBUTES.ID.QNAME, Try.parser(XsdId.parseString))
        targetNamespace <- requiredAttributeParser[XsdAnyURI](ATTRIBUTES.TARGETNAMESPACE.QNAME,Try.parser(XsdAnyURI.parseString), Some(genRandomURN _))
        version <- requiredAttributeParser(ATTRIBUTES.VERSION.QNAME, Try.parser(XsdToken.parseString), Some(() => new XsdToken("1")))
        optAttributeFormDefault <- optionalAttributeParser(ATTRIBUTES.ATTRIBUTEFORMDEFAULT.QNAME, Try.parser(FormChoiceCode.parseString))
        optElementFormDefault <- optionalAttributeParser(ATTRIBUTES.ELEMENTFORMDEFAULT.QNAME, Try.parser(FormChoiceCode.parseString))
        optBlockDefaultCodes <- optionalAttributeParser(ATTRIBUTES.BLOCKDEFAULT.QNAME, XsdAllOrNone.parser(Try.parser(BlockDefaultCode.parseString)))
        optFinalDefaultCodes <- optionalAttributeParser(ATTRIBUTES.FINALDEFAULT.QNAME, XsdAllOrNone.parser(Try.parser(FinalDefaultCode.parseString)))
        optXmlLang <- optionalAttributeParser(ATTRIBUTES.XML_LANG.QNAME, Try.parser(XsdToken.parseString))
      } yield
            XsdSchema(
              optId = optId,
              targetNamespace = targetNamespace,
              version = version,
              optAttributeFormDefault = optAttributeFormDefault,
              optElementFormDefault = optElementFormDefault,
              optBlockDefaultCodes = optBlockDefaultCodes,
              optFinalDefaultCodes = optFinalDefaultCodes,
              optXmlLang = optXmlLang,
              optMetadata = Some(XsdElement.Metadata(element))
            )

    
    def attributes = Seq(
      ATTRIBUTES.ID.QNAME,
      ATTRIBUTES.TARGETNAMESPACE.QNAME,
      ATTRIBUTES.VERSION.QNAME,
      ATTRIBUTES.ATTRIBUTEFORMDEFAULT.QNAME,
      ATTRIBUTES.ELEMENTFORMDEFAULT.QNAME,
      ATTRIBUTES.BLOCKDEFAULT.QNAME,
      ATTRIBUTES.FINALDEFAULT.QNAME,
      ATTRIBUTES.XML_LANG.QNAME
    )
    
    def allowedChildElements(children: Seq[XsdElementUtil[XsdElement]]) = {
      Seq(XsdAnnotation.util, XsdSimpleType.util)
    }

//    def downcast(element: XsdElement) : Option[XsdSchema] = element match {
//      case e : XsdSchema => Some(e)
//      case _ => None
//    }
  }
}