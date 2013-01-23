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
package org.gtri.util.scala.statemachine.statemachine

import org.gtri.util.scala.statemachine._

package object aliases {
    /*
    ∑ => input alphabet
    Γ => output alphabet
    S => set of states
    s0 => initial state (s0 ∈ S)
    ∂ => transition function
    F => set of final states (F ⊂ S)
    A => final success value type
    ∅ => 1) the type of the empty set 2) instance of the empty set
    b => instance of empty input
    EOI => 1) type of end of input 2) instance of end of input
     */
    type  S  [∑,Γ,A]   =   State                [∑,Γ,A]
    type  F  [∑,Γ,A]   =   Done                 [∑,Γ,A]
    type  ∂  [∑,Γ,A]   =   Transitor   [∑,Γ,A]

    type  EOI          =   EndOfInput
    val   EOI          =   EndOfInput

    type  ∅           =   Unit
    val   ∅           =   Unit

    type  ⊳  [∑,Γ,A]   =   Continue             [∑,Γ,A]
    val   ⊳            =   Continue
    type  ⊡  [∑,Γ,A]   =   Success              [∑,Γ,A]
    val   ⊡            =   Success
    type  ⊠  [∑,Γ,A]   =   Failure              [∑,Γ,A]
    val   ⊠            =   Failure
}
