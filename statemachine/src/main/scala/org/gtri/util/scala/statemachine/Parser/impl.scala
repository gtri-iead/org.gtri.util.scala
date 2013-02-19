package org.gtri.util.scala.statemachine.Parser

import org.gtri.util.scala.statemachine._
import scala.collection.immutable.Seq

object impl {


//  def mapParserTransition[A,B](s : Parser.Transition[A], f: A => B) : Parser.Transition[B] = {
//    flatMapParserTransition[A,B](s, { (a : A) => bindParserTransition(f(a)) } )
//  }
//
//  def bindParserTransition[A](value : A) : Parser.Transition[A] = Parser.Succeed(value)
//
//  def flatMapParserTransition[A,B](t : Parser.Transition[A], f: A => Parser.Transition[B]) : Parser.Transition[B] = {
//    t.state.fold(
//      ifContinuation = { q =>
//        // Should never reach here - but it is possible if user is determined
//        Parser.Halt.fatal("Parser in continuation state")
//      },
//      ifSuccess = { q =>
//        val t1 = f(q.value)
//        t1.copy(metadata = t1.metadata ++ t.metadata)
//      },
//      ifHalted = { q =>
//        val optRecover : Option[() => Parser.Transition[B]] = q.optRecover map { recover => { () =>
//          flatMapParserTransition(recover(), f)
//        }}
//        Parser.Halt(
//          issues = q.issues,
//          optRecover = optRecover,
//          metadata = t.metadata
//        )
//      }
//    )
//  }

}
