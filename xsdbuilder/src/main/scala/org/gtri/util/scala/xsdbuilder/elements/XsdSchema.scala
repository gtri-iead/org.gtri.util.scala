package org.gtri.util.scala.xsdbuilder.elements

import org.gtri.util.scala.statemachine._
import org.gtri.util.xsddatatypes._
import org.gtri.util.xsddatatypes.XsdCodes._
import org.gtri.util.xsddatatypes.XsdConstants._
import org.gtri.util.scala.xsdbuilder.XmlParser._
import org.gtri.util.scala.xmlbuilder.XmlElement

case class XsdSchema(
  id                      :   Option[XsdId]                                         = None,
  targetNamespace         :   XsdAnyURI,
  version                 :   XsdToken,
  attributeFormDefault    :   Option[FormChoiceCode]                                = None,
  elementFormDefault      :   Option[FormChoiceCode]                                = None,
  blockDefaultCodes       :   Option[Either[AllOrNoneCode, Set[BlockDefaultCode]]]  = None,
  finalDefaultCodes       :   Option[Either[AllOrNoneCode, Set[FinalDefaultCode]]]  = None,
  xml_lang                :   Option[XsdToken]                                      = None,
  optMetadata                :   Option[XsdElement.Metadata]                           = None
) extends XsdElement {

  def   qName   =   XsdSchema.util.qName
  def   value   =   None

  def toAttributes = {
    id.map({ id => ATTRIBUTES.ID.QNAME -> id.toString}).toList :::
    List(ATTRIBUTES.VERSION.QNAME -> version.toString) :::
    List(ATTRIBUTES.TARGETNAMESPACE.QNAME -> targetNamespace.toString) :::
    attributeFormDefault.map({ attributeFormDefault => ATTRIBUTES.ATTRIBUTEFORMDEFAULT.QNAME -> attributeFormDefault.toString}).toList :::
    elementFormDefault.map({ elementFormDefault => ATTRIBUTES.ELEMENTFORMDEFAULT.QNAME -> elementFormDefault.toString}).toList :::
    blockDefaultCodes.map({ blockDefaultCodes => ATTRIBUTES.BLOCKDEFAULT.QNAME -> blockDefaultCodes.fold({ _.toString },{ _.mkString(" ") })}).toList :::
    finalDefaultCodes.map({ finalDefaultCodes => ATTRIBUTES.FINALDEFAULT.QNAME -> finalDefaultCodes.fold({ _.toString },{ _.mkString(" ") })}).toList :::
    xml_lang.map({ xml_lang => ATTRIBUTES.XML_LANG.QNAME -> xml_lang.toString}).toList
  }

//  def pushTo(contract: XsdContract) {
//    val blockDefaultAllOrNoneCode : AllOrNoneCode = blockDefaultCodes.orNull.left.toOption.orNull
//    val blockDefaultCodesSet : Set[BlockDefaultCode] = {
//      val temp : Either[AllOrNoneCode, Set[BlockDefaultCode]] = blockDefaultCodes.getOrElse(Right(Set.empty[BlockDefaultCode]))
//      temp.right.getOrElse(Set.empty[BlockDefaultCode])
//    }
//    val finalDefaultAllOrNoneCode : AllOrNoneCode = finalDefaultCodes.orNull.left.toOption.orNull
//    val finalDefaultCodesSet : Set[FinalDefaultCode] = {
//      val temp : Either[AllOrNoneCode, Set[FinalDefaultCode]] = finalDefaultCodes.getOrElse(Right(Set.empty[FinalDefaultCode]))
//      temp.right.getOrElse(Set.empty[FinalDefaultCode])
//    }
//    contract.addXsdSchema(
//      /* XsdId _id => */id.orNull,
//      /* XsdAnyURI _targetNamespace  => */targetNamespace,
//      /* XsdToken _version  => */version,
//      /* XsdContract.FormChoiceCode _attributeFormDefault  => */attributeFormDefault.orNull,
//      /* XsdContract.FormChoiceCode _elementFormDefault  => */elementFormDefault.orNull,
//      /* ImmutableSet<XsdContract.BlockDefaultCode> _blockDefaultCodes  => */blockDefaultCodesSet,
//      /* XsdContract.AllOrNoneCode _blockDefaultAllOrNoneCode  => */blockDefaultAllOrNoneCode,
//      /* ImmutableSet<XsdContract.FinalDefaultCode> _finalDefaultCodes  => */finalDefaultCodesSet,
//      /* XsdContract.AllOrNoneCode _finalDefaultAllOrNoneCode  => */finalDefaultAllOrNoneCode,
//      /* XsdToken _xml_lang  => */xml_lang.orNull,
//      /* ImmutableMap<XsdNCName, XsdAnyURI> _prefixToNamespaceURIMap  => */prefixToNamespaceURIMap
//    )
//  }
}

object XsdSchema {

  implicit object util extends XsdElementUtil[XsdSchema] {
    def qName = ELEMENTS.SCHEMA.QNAME

    def randomString = java.lang.Long.toHexString(java.lang.Double.doubleToLongBits(java.lang.Math.random()))
    def genRandomURN = new XsdAnyURI(new StringBuilder().append("urn:").append(randomString).append(":").append(randomString).toString())

    def parser[EE >: XsdSchema] : Iteratee[XmlElement,EE] = {
      for{
        element <- QNamePeekParser(qName)
        id <- OptionalAttributePeekParser(ATTRIBUTES.ID.QNAME, XsdId.parseString)
        targetNamespace <- RequiredAttributePeekParser(ATTRIBUTES.TARGETNAMESPACE.QNAME,XsdAnyURI.parseString, genRandomURN _)
        version <- RequiredAttributePeekParser(ATTRIBUTES.VERSION.QNAME, XsdToken.parseString, () => new XsdToken("1"))
        attributeFormDefault <- OptionalAttributePeekParser(ATTRIBUTES.ATTRIBUTEFORMDEFAULT.QNAME, FormChoiceCode.parseString)
        elementFormDefault <- OptionalAttributePeekParser(ATTRIBUTES.ELEMENTFORMDEFAULT.QNAME, FormChoiceCode.parseString)
        blockDefaultCodes <- OptionalAttributePeekParser(ATTRIBUTES.BLOCKDEFAULT.QNAME, parseAllOrNone(BlockDefaultCode.parseString))
        finalDefaultCodes <- OptionalAttributePeekParser(ATTRIBUTES.FINALDEFAULT.QNAME, parseAllOrNone(FinalDefaultCode.parseString))
        xml_lang <- OptionalAttributePeekParser(ATTRIBUTES.XML_LANG.QNAME, XsdToken.parseString)
      } yield
            XsdSchema(
              id = id,
              targetNamespace = targetNamespace,
              version = version,
              attributeFormDefault = attributeFormDefault,
              elementFormDefault = elementFormDefault,
              blockDefaultCodes = blockDefaultCodes,
              finalDefaultCodes = finalDefaultCodes,
              xml_lang = xml_lang,
              optMetadata = Some(XsdElement.Metadata(element))
            )
    }


    def allowedChildElements(children: Seq[XsdElementUtil[XsdElement]]) = {
      Seq(XsdAnnotation.util, XsdSimpleType.util)
    }

//    def downcast(element: XsdElement) : Option[XsdSchema] = element match {
//      case e : XsdSchema => Some(e)
//      case _ => None
//    }
  }
}