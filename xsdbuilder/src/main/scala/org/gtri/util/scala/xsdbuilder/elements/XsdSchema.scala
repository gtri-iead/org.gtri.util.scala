package org.gtri.util.scala.xsdbuilder.elements

import org.gtri.util.scala.statemachine._
import org.gtri.util.xsddatatypes._
import org.gtri.util.xsddatatypes.XsdCodes._
import org.gtri.util.xsddatatypes.XsdConstants._
import org.gtri.util.scala.xsdbuilder.XmlParser._
import org.gtri.util.scala.xmlbuilder.XmlElement
import org.gtri.util.scala.xsdbuilder.XsdAllOrNone
import org.gtri.util.scala.xsdbuilder.elements.XsdGenVal._

case class XsdSchema(
  optId                     :   Option[XsdId]                         = None,
  targetNamespace           :   XsdAnyURI,
  version                   :   XsdToken,
  optAttributeFormDefault   :   Option[FormChoiceCode]                = None,
  optElementFormDefault     :   Option[FormChoiceCode]                = None,
  optBlockDefault      :   Option[XsdAllOrNone[BlockDefaultCode]]   = None,
  optFinalDefault      :   Option[XsdAllOrNone[FinalDefaultCode]]   = None,
  optXmlLang                :   Option[XsdToken]                      = None,
  optMetadata               :   Option[XsdElement.Metadata]           = None
) extends XsdElement {

  def   qName      =   XsdSchema.util.qName
  def   optValue   =   None

  def util = XsdSchema.util

  def attributesMap(namespaceURIToPrefixResolver : XsdQName.NamespaceURIToPrefixResolver) = {
    {
      optId.map { (ATTRIBUTES.ID.QNAME -> _.toString) } ::
      Some(ATTRIBUTES.TARGETNAMESPACE.QNAME -> targetNamespace.toString) ::
      Some(ATTRIBUTES.VERSION.QNAME -> version.toString) ::
      optAttributeFormDefault.map { (ATTRIBUTES.ATTRIBUTEFORMDEFAULT.QNAME -> _.toString )} ::
      optElementFormDefault.map { (ATTRIBUTES.ELEMENTFORMDEFAULT.QNAME -> _.toString )} ::
      optBlockDefault.map { (ATTRIBUTES.BLOCKDEFAULT.QNAME -> _.toString )} ::
      optFinalDefault.map { (ATTRIBUTES.FINALDEFAULT.QNAME -> _.toString )} ::
      optXmlLang.map { (ATTRIBUTES.XML_LANG.QNAME -> _.toString )} ::
      Nil
    }.flatten.toMap
  }
}

object XsdSchema {

  implicit object util extends XsdElementUtil[XsdSchema] {

    def qName = ELEMENTS.SCHEMA.QNAME

    def parser[EE >: XsdSchema](prefixToNamespaceURIResolver : XsdQName.PrefixToNamespaceURIResolver) : Parser[XmlElement,EE] =
      for{
        element <- Parser.tell[XmlElement]
        optId <- optionalAttributeParser(ATTRIBUTES.ID.QNAME, Try.parser(XsdId.parseString))
        targetNamespace <- requiredAttributeParser[XsdAnyURI](ATTRIBUTES.TARGETNAMESPACE.QNAME,Try.parser(XsdAnyURI.parseString), Some(genRandomURN _))
        version <- requiredAttributeParser(ATTRIBUTES.VERSION.QNAME, Try.parser(XsdToken.parseString), Some(() => new XsdToken("1")))
        optAttributeFormDefault <- optionalAttributeParser(ATTRIBUTES.ATTRIBUTEFORMDEFAULT.QNAME, Try.parser(FormChoiceCode.parseString))
        optElementFormDefault <- optionalAttributeParser(ATTRIBUTES.ELEMENTFORMDEFAULT.QNAME, Try.parser(FormChoiceCode.parseString))
        optBlockDefault <- optionalAttributeParser(ATTRIBUTES.BLOCKDEFAULT.QNAME, XsdAllOrNone.parser(Try.parser(BlockDefaultCode.parseString)))
        optFinalDefault <- optionalAttributeParser(ATTRIBUTES.FINALDEFAULT.QNAME, XsdAllOrNone.parser(Try.parser(FinalDefaultCode.parseString)))
        optXmlLang <- optionalAttributeParser(ATTRIBUTES.XML_LANG.QNAME, Try.parser(XsdToken.parseString))
      } yield
            XsdSchema(
              optId = optId,
              targetNamespace = targetNamespace,
              version = version,
              optAttributeFormDefault = optAttributeFormDefault,
              optElementFormDefault = optElementFormDefault,
              optBlockDefault = optBlockDefault,
              optFinalDefault = optFinalDefault,
              optXmlLang = optXmlLang,
              optMetadata = Some(XsdElement.Metadata(element))
            )

    
    def attributes = Set(
      ATTRIBUTES.ID.QNAME,
      ATTRIBUTES.TARGETNAMESPACE.QNAME,
      ATTRIBUTES.VERSION.QNAME,
      ATTRIBUTES.ATTRIBUTEFORMDEFAULT.QNAME,
      ATTRIBUTES.ELEMENTFORMDEFAULT.QNAME,
      ATTRIBUTES.BLOCKDEFAULT.QNAME,
      ATTRIBUTES.FINALDEFAULT.QNAME,
      ATTRIBUTES.XML_LANG.QNAME
    )
    
    def allowedChildElements(children: Seq[XsdElementUtil[XsdElement]]) = Seq(
      XsdAnnotation.util,
      XsdSimpleType.util,
      XsdImport.util
    )

  }
}