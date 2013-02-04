/*
    Copyright 2012 Georgia Tech Research Institute

    Author: lance.gatlin@gtri.gatech.edu

    This file is part of org.gtri.util.scala.xmlbuilder library.

    org.gtri.util.scala.xmlbuilder library is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    org.gtri.util.scala.xmlbuilder library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with org.gtri.util.scala.xmlbuilder library. If not, see <http://www.gnu.org/licenses/>.

*/
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