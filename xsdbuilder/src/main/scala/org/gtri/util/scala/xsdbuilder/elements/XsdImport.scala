package org.gtri.util.scala.xsdbuilder.elements

import org.gtri.util.scala.statemachine._
import org.gtri.util.xsddatatypes._
import org.gtri.util.xsddatatypes.XsdConstants._
import org.gtri.util.scala.xsdbuilder.XmlParser._
import org.gtri.util.scala.xmlbuilder.{XmlNamespaceContext, XmlElement}

final case class XsdImport(
  optId              :   Option[XsdId]                 = None,
  optNamespace       :   Option[XsdAnyURI]             = None,
  optSchemaLocation  :   Option[XsdAnyURI]             = None,
  optMetadata        :   Option[XsdElement.Metadata]   = None
) extends XsdElement {

  def optValue = None

  def util = XsdImport.util

  def toAttributesMap(context: Seq[XmlNamespaceContext]) = {
    {
      optId.map { (ATTRIBUTES.ID.QNAME -> _.toString) } ::
      optNamespace.map { (ATTRIBUTES.NAMESPACE.QNAME -> _.toString )} ::
      optSchemaLocation.map { (ATTRIBUTES.SCHEMALOCATION.QNAME -> _.toString )} ::
      Nil
    }.flatten.toMap
  }
}

object XsdImport {

  implicit object util extends XsdElementUtil[XsdImport] {

    def qName = ELEMENTS.IMPORT.QNAME

    def parser[EE >: XsdImport](context: Seq[XmlNamespaceContext]) : Parser[XmlElement, EE] = {
      for{
        element <- Parser.tell[XmlElement]
        optId <- optionalAttributeParser(ATTRIBUTES.ID.QNAME, Try.parser(XsdId.parseString))
        optNamespace <- optionalAttributeParser(ATTRIBUTES.NAMESPACE.QNAME, Try.parser(XsdAnyURI.parseString))
        optSchemaLocation <- optionalAttributeParser(ATTRIBUTES.SCHEMALOCATION.QNAME, Try.parser(XsdAnyURI.parseString))
      } yield {
        XsdImport(
          optId = optId,
          optNamespace = optNamespace,
          optSchemaLocation = optSchemaLocation,
          optMetadata = Some(XsdElement.Metadata(element))
        )
      }
    }

    def attributes = Set(
      ATTRIBUTES.ID.QNAME,
      ATTRIBUTES.NAMESPACE.QNAME,
      ATTRIBUTES.SCHEMALOCATION.QNAME
    )

    def allowedChildElements(children: Seq[XsdElementUtil[XsdElement]]) = Seq(XsdAppInfo.util)

    //    def downcast(element: XsdElement) : Option[XsdImport] = element match {
    //      case e : XsdImport => Some(e)
    //      case _ => None
    //    }
  }
}