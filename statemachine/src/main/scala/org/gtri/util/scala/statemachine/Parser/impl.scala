package org.gtri.util.scala.statemachine.Parser

import org.gtri.util.scala.statemachine._
import org.gtri.util.scala.statemachine.Parser._
import scala.collection.immutable.Seq

object impl {

//  def flatMapTransition[B,C](t0: Transition[B], f: B => Transition[C]) : Transition[C] = {
//    t0.fold[Transition[C]](
//      ifContinue = t0 => throw new IllegalStateException,
//      ifSucceed = t0 => {
//        val t1 = f(t0.state.value)
//        t1.fold[Transition[C]](
//          ifContinue = t1 => throw new IllegalStateException,
//          ifSucceed = t1 => t0.copy(metadata = t1.metadata ++ t0.metadata),
//          ifHalt = t1 => t0.copy(metadata = t1.metadata ++ t0.metadata)
//        )
//      },
//      ifHalt = t0 => {
//        val optRecover : Option[() => Transition[C]] = t0.state.optRecover map { recover => { () =>
//          flatMapTransition(recover(),f)
//        }}
//        new Halt(
//          state = new State.Halted(
//            issues = t0.state.issues,
//            optRecover = optRecover
//          ),
//          output = Seq.empty,
//          overflow = Seq.empty,
//          metadata = t0.metadata
//        )
//      }
//    )
//  }

  def flatMapParser[A,B,C](p: Parser[A,B],f: B => Parser[A,C]) = new Parser[A,C] {
    def apply(a: A) = Enumerable.impl.flatMapEnumerableDoneTransition[Unit,B,Unit,Unit,C](p(a),{ b => f(b)(a) })
  }

  def mapParser[A,B,C](p: Parser[A,B],f: B => C) = new Parser[A,C] {
    def apply(a: A) = Enumerable.impl.flatMapEnumerableDoneTransition[Unit,B,Unit,Unit,C](p(a),{ b => Succeed(f(b)) })
  }

//  def flatMapTransition[A,B,C](t0: Transition[B], f: B => Transition[C]) : Transition[C] = {
//    t0.fold(
//      ifSucceed = t0 => f(t0.state.value),
//      ifHalt = t0 => {
//        val optRecover : Option[() => Transition[C]] = t0.state.optRecover map { recover => { () =>
//          recover().fold(
//            // TODO: remove me once apply(EndOfInput) returns DoneTransition
//            ifContinue = t1 => throw new RuntimeException("Invalid Parser Transition "),
//            ifSucceed = t1 => flatMapTransition(t1, f),
//            ifHalt = t1 => flatMapTransition(t1,f)
//          )
//
//        }}
//        StateMachine.Halt(
//          issues = t0.state.issues,
//          optRecover = optRecover,
//          metadata = t0.metadata
//        )
//      }
//    )
//  }
//
//  def flatMapParser[A,B,C](p: Parser[A,B], f: B => Parser[A,C]) = new Parser[A,C] {
//      def apply(a: A) = {
//        val t0 : Parser.Transition[B] = p(a)
//        flatMapTransition[A,B,C](t0,b => f(b)(a))
//      }
//    }
//
//  def mapParser[A,B,C](p: Parser[A,B], f: B => C) = new Parser[A,C] {
//    def apply(a: A) = {
//      val t0 : Parser.Transition[B] = p(a)
//      flatMapTransition[A,B,C](t0,b => Succeed(f(b)))
//    }
//  }


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
