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

  implicit class implicitXmlElementPrefixToNamespaceURIResolver(self: XmlElement) extends PrefixToNamespaceURIResolver {
    def getNamespaceURIForPrefix(prefix : XsdNCName) = self.prefixToNamespaceURIMap.get(prefix).orNull
  }

  implicit class implicitXmlElementNamespaceURIToPrefixResolver(self: XmlElement) extends NamespaceURIToPrefixResolver {

    def isValidPrefixForNamespaceURI(prefix: XsdNCName, namespaceURI: XsdAnyURI) =
      self.prefixToNamespaceURIMap mapValues { _ == namespaceURI } getOrElse(prefix, false)

    def getPrefixForNamespaceURI(namespaceURI: XsdAnyURI) = self.namespaceURIToPrefixMap.get(namespaceURI).orNull
  }

  implicit class implicitXmlElementStack(stack: Seq[XmlElement]) extends NamespaceURIToPrefixResolver {
    def isValidPrefixForNamespaceURI(prefix: XsdNCName, namespaceURI: XsdAnyURI) = doIsValidPrefixForNamespaceURI(stack, prefix, namespaceURI)

    def getPrefixForNamespaceURI(namespaceURI: XsdAnyURI) : XsdNCName = doGetPrefixForNamespaceURI(stack, namespaceURI)

    @tailrec
    private def doIsValidPrefixForNamespaceURI(stack : Seq[XmlElement], prefix: XsdNCName, namespaceURI: XsdAnyURI) : Boolean = {
      if(stack.isEmpty) {
        false
      } else {
        val head :: tail = stack
        if(head.isValidPrefixForNamespaceURI(prefix, namespaceURI)) {
          true
        } else {
          doIsValidPrefixForNamespaceURI(tail, prefix, namespaceURI)
        }
      }
    }

    @tailrec
    private def doGetPrefixForNamespaceURI(stack : Seq[XmlElement], namespaceURI: XsdAnyURI) : XsdNCName = {
      stack match {
        case Nil => null
        case head :: tail =>
          val result = head.getPrefixForNamespaceURI(namespaceURI)
          if(result != null) {
            result
          } else {
            doGetPrefixForNamespaceURI(tail, namespaceURI)
          }
      }
    }
  }
}
