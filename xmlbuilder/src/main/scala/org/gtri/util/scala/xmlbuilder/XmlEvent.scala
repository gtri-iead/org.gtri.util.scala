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

import org.gtri.util.xsddatatypes.XsdQName

sealed trait XmlEvent {
  def locator : DiagnosticLocator
}

final case class StartXmlDocumentEvent(
  encoding                  :   String,
  version                   :   String,
  isStandAlone              :   Boolean,
  characterEncodingScheme   :   String,
  locator                   :   DiagnosticLocator
) extends XmlEvent

final case class EndXmlDocumentEvent(
  locator                   :   DiagnosticLocator
) extends XmlEvent

final case class StartXmlElementEvent(
  element                   :   XmlElement,
  locator                   :   DiagnosticLocator
) extends XmlEvent

final case class EndXmlElementEvent(
  qName                     :   XsdQName,
  locator                   :   DiagnosticLocator
) extends XmlEvent

final case class AddXmlCommentEvent(
  comment                   :   String,
  locator                   :   DiagnosticLocator
) extends XmlEvent

final case class AddXmlTextEvent(
  text                      :   String,
  locator                   :   DiagnosticLocator
) extends XmlEvent
