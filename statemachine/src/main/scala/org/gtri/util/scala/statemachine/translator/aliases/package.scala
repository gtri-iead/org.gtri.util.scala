/*
    Copyright 2012 Georgia Tech Research Institute

    Author: lance.gatlin@gtri.gatech.edu

    This file is part of org.gtri.util.iteratee library.

    org.gtri.util.iteratee library is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    org.gtri.util.iteratee library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with org.gtri.util.iteratee library. If not, see <http://www.gnu.org/licenses/>.

*/
package org.gtri.util.scala.statemachine.translator

package object aliases {
  /*
  ∑ => input alphabet
  Γ => output alphabet
  S => set of states
  s0 => initial state (s0 ∈ S)
  ∂ => transition function
  F => set of final states (F ⊂ S)
  ∅ => 1) the type of the empty set 2) instance of the empty set
   */
  type  S  [∑,Γ]   =   State                [∑,Γ]
  type  F  [∑,Γ]   =   Done                 [∑,Γ]
  type  ∂  [∑,Γ]   =   Transitor            [∑,Γ]

  type  EOI        =   EndOfInput
  val   EOI        =   EndOfInput

  type  ∅          =   Unit
  val   ∅          =   Unit

  type  ⊳  [∑,Γ]   =   Continue             [∑,Γ]
  val   ⊳          =   Continue
  type  ⊡  [∑,Γ]   =   Success              [∑,Γ]
  val   ⊡          =   Success
  type  ⊠  [∑,Γ]   =   Failure              [∑,Γ]
  val   ⊠          =   Failure
}
