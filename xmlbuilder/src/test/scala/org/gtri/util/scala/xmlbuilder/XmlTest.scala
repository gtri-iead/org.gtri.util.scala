package org.gtri.util.scala.xmlbuilder

import org.scalatest.FeatureSpec
import org.scalatest.GivenWhenThen
import javax.xml.stream.XMLInputFactory
import java.io.{FileOutputStream, FileInputStream, File, InputStream}

class XmlTest extends FeatureSpec with GivenWhenThen {
  feature("Callers can connect an XmlReader to an XmlWriter to form a plan that will read and write an XML File") {
    scenario("std") {
      Given("An XML file, an XMLStreamReader, an XmlReader and an XmlWriter")
      val xmlReader = XmlReader(new FileInputStream("test.xml"))
      val xmlWriter = XmlWriter(new FileOutputStream("test.xml"))
      
    }
  }
}
