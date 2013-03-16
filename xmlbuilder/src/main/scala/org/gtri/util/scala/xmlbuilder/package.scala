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
package org.gtri.util.scala

import org.gtri.util.xsddatatypes._
import org.gtri.util.xsddatatypes.XsdQName._
import annotation.tailrec

package object xmlbuilder {
  type DiagnosticLocator = Any

  implicit class implicitXmlElementStackPrefixToNamespaceURIResolver(stack: Seq[PrefixToNamespaceURIResolver]) extends PrefixToNamespaceURIResolver {

    // Note: rejected this b/c it calls getNamespaceURIForPrefix twice
//    def getNamespaceURIForPrefix(prefix: XsdNCName) = stack.find(_.getNamespaceURIForPrefix(prefix) != null).map(_.getNamespaceURIForPrefix(prefix)).orNull
    def getNamespaceURIForPrefix(prefix: XsdNCName) = doGetNamespaceURIForPrefix(stack, prefix)

    @tailrec
    private def doGetNamespaceURIForPrefix(stack: Seq[PrefixToNamespaceURIResolver], prefix: XsdNCName) : XsdAnyURI = {
      stack match {
        case Nil => null
        case head :: tail =>
          val result = head.getNamespaceURIForPrefix(prefix)
          // Note: Have to use if/else to get tailrec to work here
          if(result != null) {
            result
          } else {
            doGetNamespaceURIForPrefix(tail, prefix)
          }
      }
    }
  }

  implicit class implicitXmlElementStackNamespaceURIToPrefixResolver(stack: Seq[NamespaceURIToPrefixResolver]) extends NamespaceURIToPrefixResolver {
    def isValidPrefixForNamespaceURI(prefix: XsdNCName, namespaceURI: XsdAnyURI) = doIsValidPrefixForNamespaceURI(stack, prefix, namespaceURI)

    def getPrefixForNamespaceURI(namespaceURI: XsdAnyURI) : XsdNCName = doGetPrefixForNamespaceURI(stack, namespaceURI)

    @tailrec
    private def doIsValidPrefixForNamespaceURI(stack : Seq[NamespaceURIToPrefixResolver], prefix: XsdNCName, namespaceURI: XsdAnyURI) : Boolean = {
      stack match {
        case Nil => false
        case head :: tail =>
          // Note: Have to use if/else to get tailrec to work here
          if(head.isValidPrefixForNamespaceURI(prefix, namespaceURI)) {
            true
          } else {
            doIsValidPrefixForNamespaceURI(tail, prefix, namespaceURI)
          }
      }
    }

    @tailrec
    private def doGetPrefixForNamespaceURI(stack : Seq[NamespaceURIToPrefixResolver], namespaceURI: XsdAnyURI) : XsdNCName = {
      stack match {
        case Nil => null
        case head :: tail =>
          val result = head.getPrefixForNamespaceURI(namespaceURI)
          // Note: Have to use if/else to get tailrec to work here
          if(result != null) {
            result
          } else {
            doGetPrefixForNamespaceURI(tail, namespaceURI)
          }
      }
    }
  }
}
