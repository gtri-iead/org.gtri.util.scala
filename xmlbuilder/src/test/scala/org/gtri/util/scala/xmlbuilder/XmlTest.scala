package org.gtri.util.scala.xmlbuilder

import org.scalatest.FeatureSpec
import org.scalatest.GivenWhenThen
import java.io._
import org.gtri.util.scala.statemachine._
import annotation.tailrec
import difflib.{Patch, DiffUtils}
import scala.collection.JavaConversions._

class XmlTest extends FeatureSpec with GivenWhenThen {
  feature("Callers can connect an XmlReader to an XmlWriter to form a plan that will read and write an XML File") {
    scenario("std") {
      Given("An an XmlReader and XmlWriter composed into a Plan")
//      XmlTest.listPath(new File("."))
      val xmlReader = XmlReader(new FileInputStream("src/test/resources/test.xml"))
      val xmlWriter = XmlWriter(new FileOutputStream("target/test.xml"))
      val plan = xmlReader compose xmlWriter
      When("that plan is run")
      val result = plan.run()
      Then("the result should be a success")
      assert(result.isSuccess)
      And("the input and output files should match")
      assert(XmlTest.streamsAreEqual(new FileInputStream("src/test/resources/test.xml"), new FileInputStream("target/test.xml")))
    }
  }
}

object XmlTest {

  def readLines(in : InputStream) : List[String] = readLines(new BufferedReader(new InputStreamReader(in)))
  @tailrec def readLines(reader : BufferedReader, xs : List[String] = Nil) : List[String] = {
    val line = reader.readLine()
    if(line == null) {
      xs
    } else {
      readLines(reader,line :: xs)
    }
  }

  def streamsAreEqual(in : InputStream, compare : InputStream) : Boolean = {
    /*
     * COMPARE the first output to the second
     */
    val l1 = readLines(in)
    val l2 = readLines(compare)

    val patch = DiffUtils.diff(l1, l2)

    for(delta <- patch.getDeltas()) {
      println(delta.getOriginal())
      println(delta.getRevised())
      println()
    }

    patch.getDeltas().isEmpty()
  }

//  def listPath(path : File, indentLevel : Int = 0) {
//    val files = path.listFiles().sortBy({ file => file.getName() })
//    val indent = List.fill(indentLevel)(' ').mkString
//    for(file <- files) {
//      1 to indentLevel foreach { _ => print(' ')}
//      println(file.toString)
//      if(file.isDirectory()) {
//        listPath(file,indentLevel + 1)
//      }
//    }
//  }
}