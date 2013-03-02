package org.gtri.util.scala.xsdbuilder.elements

import org.gtri.util.xsddatatypes._

object XsdGenVal {
  def randomString = java.lang.Long.toHexString(java.lang.Double.doubleToLongBits(java.lang.Math.random()))
  def genRandomName = new XsdName(new StringBuilder().append("GenXsdName").append(randomString).append(randomString).toString())
  def genRandomURN = new XsdAnyURI(new StringBuilder().append("urn:").append(randomString).append(":").append(randomString).toString())
}
