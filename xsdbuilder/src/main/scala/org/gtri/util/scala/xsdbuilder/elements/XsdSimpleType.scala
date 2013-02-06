package org.gtri.util.scala.xsdbuilder.elements

import org.gtri.util.scala.statemachine._
import org.gtri.util.xsddatatypes._
import org.gtri.util.xsddatatypes.XsdCodes._
import org.gtri.util.xsddatatypes.XsdConstants._
import org.gtri.util.scala.xsdbuilder.XmlParser._
import org.gtri.util.scala.xmlbuilder.XmlElement

case class XsdSimpleType(
  id            :   Option[XsdId]                                             = None,
  name          :   XsdName,
  finalCodes    :   Option[Either[AllOrNoneCode, Set[SimpleTypeFinalCode]]]   = None,
  optMetadata      :   Option[XsdElement.Metadata]                               = None
) extends XsdElement {
  def   qName   =   XsdSimpleType.util.qName
  def   value   =   None

  def toAttributes = {
    id.map({ id => ATTRIBUTES.ID.QNAME -> id.toString}).toList :::
    List(ATTRIBUTES.NAME.QNAME -> name.toString) :::
    finalCodes.map({ finalCodes => ATTRIBUTES.FINAL.QNAME -> finalCodes.fold({ _.toString },{ _.mkString(" ") })}).toList
  }

//  def pushTo(contract: XsdContract) {
//    val finalAll : Boolean = finalCodes == Some(Left(AllOrNoneCode.ALL))
//    val finalCodesSet : Set[SimpleTypeFinalCode] = {
//      val temp : Either[AllOrNoneCode, Set[SimpleTypeFinalCode]] = finalCodes.getOrElse(Right(Set.empty))
//      temp.right.getOrElse(Set.empty)
//    }
//    contract.addXsdSimpleType(
//      /* XsdId _id => */id.orNull,
//      /* XsdName _name => */name,
//      /* ImmutableSet<XsdContract.SimpleTypeFinalCode> _finalCodes => */finalCodesSet,
//      /* Boolean _finalAll => */finalAll,
//        /* ImmutableMap<XsdNCName, XsdAnyURI> _prefixToNamespaceURIMap  => */prefixToNamespaceURIMap
//    )
//  }
}

object XsdSimpleType {

  implicit object util extends XsdElementUtil[XsdSimpleType] {
    def qName = ELEMENTS.SIMPLETYPE.QNAME

    def randomString = java.lang.Long.toHexString(java.lang.Double.doubleToLongBits(java.lang.Math.random()))
    def genRandomName = new XsdName(new StringBuilder().append("RndName").append(randomString).append(randomString).toString())

    def parser[EE >: XsdSimpleType] : Iteratee[XmlElement,EE] = {
      for{
        element <- QNamePeekParser(qName)
        id <- OptionalAttributePeekParser(ATTRIBUTES.ID.QNAME, XsdId.parseString)
        name <- RequiredAttributePeekParser(ATTRIBUTES.NAME.QNAME,XsdName.parseString, genRandomName _)
        finalCodes <- OptionalAttributePeekParser(ATTRIBUTES.FINAL.QNAME, parseAllOrNone(SimpleTypeFinalCode.parseString))
      } yield
          XsdSimpleType(
            id = id,
            name = name,
            finalCodes = finalCodes,
            optMetadata = Some(XsdElement.Metadata(element))
          )
    }

    def allowedChildElements(children: Seq[XsdElementUtil[XsdElement]]) = {
      Seq(XsdAnnotation.util)
    }

//    def downcast(element: XsdElement) : Option[XsdSimpleType] = element match {
//      case e : XsdSimpleType => Some(e)
//      case _ => None
//    }
  }
}