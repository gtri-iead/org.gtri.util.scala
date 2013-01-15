/*
    Copyright 2012 Georgia Tech Research Institute

    Author: lance.gatlin@gtri.gatech.edu

    This file is part of org.gtri.util.scala library.

    org.gtri.util.scala library is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    org.gtri.util.scala library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with org.gtri.util.scala library. If not, see <http://www.gnu.org/licenses/>.

*/
package org.gtri.util.scala.exelog.noop

final class NoopLog {

  @inline def name : String = ""

  @inline def info(message: => String) { }

  @inline def debug(message: => String) { }

  @inline def trace(message: => String) { }

  @inline def warn(message: => String) { }

  @inline def warn(cause: Throwable) { }

  @inline def warn(message: => String, cause: Throwable) { }

  @inline def error(message: => String) { }

  @inline def error(cause: Throwable) { }

  @inline def error(message: => String, cause: Throwable) { }

  @inline def fatal(message: => String) { }

  @inline def fatal(cause: Throwable) { }

  @inline def fatal(message: => String, cause: Throwable) { }
}
