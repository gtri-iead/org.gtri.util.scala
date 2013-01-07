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
package org.gtri.util.scala.optrecover

import language.higherKinds
import scalaz._
import Scalaz._

/**
* Created with IntelliJ IDEA.
* User: Lance
* Date: 12/16/12
* Time: 10:01 PM
* To change this template use File | Settings | File Templates.
*/

object OptRecoverM extends OptRecoverMTypeClassImplicits {

  def empty[M[+_],A] : OptRecoverM[M,A] = NoGo()
  def apply[M[+_],A](a : A) : OptRecoverM[M,A] = Go(a)
  def fromOption[M[+_],A](opt : Option[A]) : OptRecoverM[M,A] = {
    if(opt.isDefined) {
      Go(opt.get)
    } else {
      NoGo()
    }
  }
  def recover[M[+_],A](recoverable: => M[Option[A]]) : OptRecoverM[M,A] = Recover(recoverable)
}

case class NoGo[M[+_],A]() extends OptRecoverM[M,A] {
  override def isNoGo : Boolean = true

  def fold[X](ifNoGo: => X, ifRecover: (=> M[Option[A]]) => X, ifGo: A => X) : X = ifNoGo
}

final class Recover[M[+_], +A](__recoverable : => M[Option[A]]) extends OptRecoverM[M, A] {
  private lazy val _recoverable = __recoverable

  override def recoverable = _recoverable

  override def isRecover : Boolean = true

  def fold[X](ifNoGo: => X, ifRecover: (=> M[Option[A]]) => X, ifGo: A => X) : X = ifRecover(recoverable)
}

object Recover {
  def apply[M[+_],A](recoverable: => M[Option[A]]) : Recover[M,A] = new Recover[M,A](recoverable)
  def unapply[M[+_],A](box : OptRecoverM[M,A]) : Option[() => M[Option[A]]] = {
    if(box.isRecover) {
      Some({ () => box.recoverable })
    } else {
      None
    }
  }
}

final case class Go[M[+_], +A] (override val get : A) extends OptRecoverM[M,A] {
  override def isGo : Boolean = true

  def fold[X](ifNoGo: => X, ifRecover: (=> M[Option[A]]) => X, ifGo: A => X) : X = ifGo(get)
}


sealed trait OptRecoverM[M[+_],+A] {

  // Override in inherited classes
  def fold[X](ifNoGo: => X, ifRecover: (=> M[Option[A]]) => X, ifGo: A => X) : X
  def isNoGo : Boolean = false
  def isRecover : Boolean = false
  def isGo : Boolean = false
  def get : A = throw new NoSuchElementException
  def recoverable : M[Option[A]] = throw new IllegalStateException

  def recover(implicit M0: Monad[M]) : M[Option[A]] = fold(
    ifNoGo = { None.pure[M] },
    ifRecover = { recoverable => recoverable },
    ifGo = { a => Some(a).pure[M] }
  )

  def toOption : Option[A] = fold(
    ifNoGo = { None },
    ifRecover = { _ => None },
    ifGo = { a => Some(a) }
  )

  def flatMap[B](f: A => OptRecoverM[M,B])(implicit M0: Monad[M]) : OptRecoverM[M,B] = fold(
      ifNoGo = { OptRecoverM.empty },
      ifRecover = {
        recoverable =>
          lazy val inner = {
            recoverable.flatMap { oa =>
              if(oa.isDefined) {
                f(oa.get).recover
              } else {
                None.pure[M]
              }
            }
          }
          Recover(inner)
      },
      ifGo = { a => f(a) }
    )

    def map[B](f: A => B)(implicit M0: Monad[M]) : OptRecoverM[M,B] = flatMap { a => OptRecoverM(f(a)) }
  //  def map[B](f: A => B)(implicit F0: Functor[M]) : OptRecoverM[M,B] = fold(
//      ifNoGo = { OptRecoverM.empty },
//      ifRecover = {
//        recoverable =>
//          lazy val inner = {
//            for(oa <- recoverable) yield for(a <- oa) yield f(a)
//          }
//          Recover(inner)
//      },
//      ifGo = { a => OptRecoverM(f(a)) }
//    )

  def ap[B](fbox: => OptRecoverM[M, (A) => B])(implicit M0: Monad[M]) : OptRecoverM[M,B] =
    for(f <- fbox;a <- this) yield f(a)
  
  def foreach[U](f: A => U) : Unit = fold(
    ifNoGo = { () },
    ifRecover = { _ => () },
    ifGo = { a => f(a) }
  )
  
}

trait OptRecoverMTypeClassImplicits {

  implicit def OptRecoverMFunctor = new OptRecoverMFunctor[Id] {
    implicit def M = idInstance
  }

  implicit def OptRecoverMFunctor[M[+_]](implicit M0: Monad[M]) = new OptRecoverMFunctor[M] {
    implicit def M = M0
  }

  implicit def OptRecoverMApply = new OptRecoverMApply[Id] {
    implicit def M = idInstance
  }

  implicit def OptRecoverMApply[M[+_]](implicit M0: Monad[M]) = new OptRecoverMApply[M] {
    implicit def M = M0
  }

  implicit def OptRecoverMApplicative = new OptRecoverMApplicative[Id] {
    implicit def M = idInstance
  }

  implicit def OptRecoverMApplicative[M[+_]](implicit M0: Monad[M]) = new OptRecoverMApplicative[M] {
    implicit def M = M0
  }

  implicit def OptRecoverMEach = new OptRecoverMEach[Id] {
    implicit def M = idInstance
  }

  implicit def OptRecoverMEach[M[+_]](implicit M0: Monad[M]) = new OptRecoverMEach[M] {
    implicit def M = M0
  }

  implicit def OptRecoverMMonad = new OptRecoverMMonad[Id] {
    implicit def M = idInstance
  }

  implicit def OptRecoverMMonad[M[+_]](implicit M0: Monad[M]) : Monad[({type λ[α] = OptRecoverM[M,α]})#λ] = new OptRecoverMMonad[M] {
    implicit def M = M0
  }

  implicit def OptRecoverMEqual[M[+_],A](implicit M0: Monad[M]) = new OptRecoverMEqual[M,A] {
    implicit def M = M0
  }

}

private[box] trait OptRecoverMFunctor[M[+_]] extends Functor[({type λ[+α]=OptRecoverM[M, α]})#λ] {
  implicit def M: Monad[M]

  override def map[A, B](fa: OptRecoverM[M, A])(f: (A) => B) = fa map f
}

private[box] trait OptRecoverMApply[M[+_]] extends Apply[({type λ[+α]=OptRecoverM[M, α]})#λ] with OptRecoverMFunctor[M] {
  implicit def M: Monad[M]

  override def ap[A, B](fa: => OptRecoverM[M, A])(f: => OptRecoverM[M, (A) => B]) = fa ap f
}

private[box] trait OptRecoverMApplicative[M[+_]] extends Applicative[({type λ[+α]=OptRecoverM[M, α]})#λ] with OptRecoverMApply[M] {
  implicit def M: Monad[M]
  
  def point[A](a: => A) = OptRecoverM(a)
}

private[box] trait OptRecoverMEach[M[+_]] extends Each[({type λ[+α]=OptRecoverM[M, α]})#λ] {
  implicit def M: Monad[M]

  def each[A](fa: OptRecoverM[M, A])(f: (A) => Unit) = fa foreach f
}

private[box] trait OptRecoverMMonad[M[+_]] extends Monad[({type λ[α] = OptRecoverM[M,α]})#λ] with OptRecoverMApplicative[M] {
  implicit def M: Monad[M]

  def bind[A, B](fa: OptRecoverM[M,A])(f: (A) => OptRecoverM[M,B]): OptRecoverM[M,B] = fa flatMap f
}

private[box] trait OptRecoverMEqual[M[+_],A] extends Equal[OptRecoverM[M,A]]  {
  implicit def M: Monad[M]

  def equal(a1: OptRecoverM[M,A], a2: OptRecoverM[M,A]) = a1 == a2
}
