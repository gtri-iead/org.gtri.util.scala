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
package org.gtri.util.scala.box

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

object BoxM extends BoxMTypeClassImplicits {

  def empty[M[+_],A] : BoxM[M,A] = NoGo()
  def apply[M[+_],A](a : A) : BoxM[M,A] = Go(a)
  def fromOption[M[+_],A](opt : Option[A]) : BoxM[M,A] = {
    if(opt.isDefined) {
      Go(opt.get)
    } else {
      NoGo()
    }
  }
  def recover[M[+_],A](recoverable: => M[Option[A]]) : BoxM[M,A] = Recover(recoverable)
}

case class NoGo[M[+_],A]() extends BoxM[M,A] {
  override def isNoGo : Boolean = true

  def fold[X](ifNoGo: => X, ifRecover: (=> M[Option[A]]) => X, ifGo: A => X) : X = ifNoGo
}

final class Recover[M[+_], +A](__recoverable : => M[Option[A]]) extends BoxM[M, A] {
  private lazy val _recoverable = __recoverable

  override def recoverable = _recoverable

  override def isRecover : Boolean = true

  def fold[X](ifNoGo: => X, ifRecover: (=> M[Option[A]]) => X, ifGo: A => X) : X = ifRecover(recoverable)
}

object Recover {
  def apply[M[+_],A](recoverable: => M[Option[A]]) : Recover[M,A] = new Recover[M,A](recoverable)
  def unapply[M[+_],A](box : BoxM[M,A]) : Option[() => M[Option[A]]] = {
    if(box.isRecover) {
      Some({ () => box.recoverable })
    } else {
      None
    }
  }
}

final case class Go[M[+_], +A] (override val get : A) extends BoxM[M,A] {
  override def isGo : Boolean = true

  def fold[X](ifNoGo: => X, ifRecover: (=> M[Option[A]]) => X, ifGo: A => X) : X = ifGo(get)
}


sealed trait BoxM[M[+_],+A] {

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

  def flatMap[B](f: A => BoxM[M,B])(implicit M0: Monad[M]) : BoxM[M,B] = fold(
      ifNoGo = { BoxM.empty },
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

    def map[B](f: A => B)(implicit M0: Monad[M]) : BoxM[M,B] = flatMap { a => BoxM(f(a)) }
  //  def map[B](f: A => B)(implicit F0: Functor[M]) : BoxM[M,B] = fold(
//      ifNoGo = { BoxM.empty },
//      ifRecover = {
//        recoverable =>
//          lazy val inner = {
//            for(oa <- recoverable) yield for(a <- oa) yield f(a)
//          }
//          Recover(inner)
//      },
//      ifGo = { a => BoxM(f(a)) }
//    )

  def ap[B](fbox: => BoxM[M, (A) => B])(implicit M0: Monad[M]) : BoxM[M,B] =
    for(f <- fbox;a <- this) yield f(a)
  
  def foreach[U](f: A => U) : Unit = fold(
    ifNoGo = { () },
    ifRecover = { _ => () },
    ifGo = { a => f(a) }
  )
  
}

trait BoxMTypeClassImplicits {

  implicit def boxMFunctor = new BoxMFunctor[Id] {
    implicit def M = idInstance
  }

  implicit def boxMFunctor[M[+_]](implicit M0: Monad[M]) = new BoxMFunctor[M] {
    implicit def M = M0
  }

  implicit def boxMApply = new BoxMApply[Id] {
    implicit def M = idInstance
  }

  implicit def boxMApply[M[+_]](implicit M0: Monad[M]) = new BoxMApply[M] {
    implicit def M = M0
  }

  implicit def boxMApplicative = new BoxMApplicative[Id] {
    implicit def M = idInstance
  }

  implicit def boxMApplicative[M[+_]](implicit M0: Monad[M]) = new BoxMApplicative[M] {
    implicit def M = M0
  }

  implicit def boxMEach = new BoxMEach[Id] {
    implicit def M = idInstance
  }

  implicit def boxMEach[M[+_]](implicit M0: Monad[M]) = new BoxMEach[M] {
    implicit def M = M0
  }

  implicit def boxMMonad = new BoxMMonad[Id] {
    implicit def M = idInstance
  }

  implicit def boxMMonad[M[+_]](implicit M0: Monad[M]) : Monad[({type λ[α] = BoxM[M,α]})#λ] = new BoxMMonad[M] {
    implicit def M = M0
  }

  implicit def boxMEqual[M[+_],A](implicit M0: Monad[M]) = new BoxMEqual[M,A] {
    implicit def M = M0
  }

}

private[box] trait BoxMFunctor[M[+_]] extends Functor[({type λ[+α]=BoxM[M, α]})#λ] {
  implicit def M: Monad[M]

  override def map[A, B](fa: BoxM[M, A])(f: (A) => B) = fa map f
}

private[box] trait BoxMApply[M[+_]] extends Apply[({type λ[+α]=BoxM[M, α]})#λ] with BoxMFunctor[M] {
  implicit def M: Monad[M]

  override def ap[A, B](fa: => BoxM[M, A])(f: => BoxM[M, (A) => B]) = fa ap f
}

private[box] trait BoxMApplicative[M[+_]] extends Applicative[({type λ[+α]=BoxM[M, α]})#λ] with BoxMApply[M] {
  implicit def M: Monad[M]
  
  def point[A](a: => A) = BoxM(a)
}

private[box] trait BoxMEach[M[+_]] extends Each[({type λ[+α]=BoxM[M, α]})#λ] {
  implicit def M: Monad[M]

  def each[A](fa: BoxM[M, A])(f: (A) => Unit) = fa foreach f
}

private[box] trait BoxMMonad[M[+_]] extends Monad[({type λ[α] = BoxM[M,α]})#λ] with BoxMApplicative[M] {
  implicit def M: Monad[M]

  def bind[A, B](fa: BoxM[M,A])(f: (A) => BoxM[M,B]): BoxM[M,B] = fa flatMap f
}

private[box] trait BoxMEqual[M[+_],A] extends Equal[BoxM[M,A]]  {
  implicit def M: Monad[M]

  def equal(a1: BoxM[M,A], a2: BoxM[M,A]) = a1 == a2
}
