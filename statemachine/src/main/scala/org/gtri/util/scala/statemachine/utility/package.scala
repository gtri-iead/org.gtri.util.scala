package org.gtri.util.scala.statemachine

import org.gtri.util.scala.statemachine._
import org.gtri.util.scala.statemachine.StateMachine._
import annotation.tailrec

package object utility {

  def forceDoneState[I,O,A](state: State[I,O,A]) : Result[I,O,A] = {
    state.fold(
      ifContinue = { s => s.apply(EndOfInput) },
      ifSuccess = { s => Result(s) },
      ifFailure = { s => Result(s) }
    )
  }

  def forceDoneResult[I,O,A](r: Result[I,O,A]) : Result[I,O,A] = {
    r.state.fold(
      ifContinue = { s => fold(r, s.apply(EndOfInput)) },
      ifSuccess = { s => r },
      ifFailure = { s => r }
    )
  }

  def fold[I,O,A](lhs : Result[I,O,A], rhs : Result[I,O,A]) : Result[I,O,A] = {
    Result(rhs.state, lhs.output ++ rhs.output, rhs.overflow, lhs.metadata ++ rhs.metadata)
  }

  /*
    Apply sequence of input to state one input item at a time
   */

  //  def applyInput[I,O,A](state: State.Continue[I,O,A], input: Seq[I]) : Result[I,O,A] = applyInput(Result(state), input)
  def applyInput[I,O,A](state: State[I,O,A], input: Seq[I]) : Result[I,O,A] = applyInput(Result(state), input)
  // TODO: optimize this by accumulating results in mutable Buffer
  @tailrec def applyInput[I,O,A](current: Result[I,O,A], input: Seq[I]) : Result[I,O,A] = {
    current.state match {
      case q : State.Continue[I,O,A] =>
        input match {
          case Nil => current
          case head :: tail => applyInput(fold(current,q(head)),tail)
          // Can't tail optimize current.state.fold
        }
      case q : State.Success[I,O,A] => current.copy(overflow = input ++ current.overflow)
      case q : State.Failure[I,O,A] => current.copy(overflow = input ++ current.overflow)
    }
  }

  /*
    Enumerator step/run
   */
  def step[I,O,A](state: State[Unit,O,A]) : Result[Unit,O,A] = {
    state.fold(
      ifContinue = { q => step(Result(q)) },
      ifSuccess = { q => Result(q) },
      ifFailure = { q => Result(q)}
    )
  }
  def step[O,A](current: Result[Unit,O,A]) : Result[Unit,O,A] = {
    current.state match {
      case q : State.Continue[Unit,O,A] => fold(current,q(()))
      case q : State.Success[Unit,O,A] => current
      case q : State.Failure[Unit,O,A] => current
    }
  }

  def run[I,O,A](state: State[Unit,O,A]) : Result[Unit,O,A] = {
    state.fold(
      ifContinue = { q => run(Result(q)) },
      ifSuccess = { q => Result(q) },
      ifFailure = { q => Result(q)}
    )
  }
  // TODO: optimize this by accumulating results in mutable Buffer
  @tailrec def run[O,A](current: Result[Unit,O,A]) : Result[Unit,O,A] = {
    current.state match {
      case q : State.Continue[Unit,O,A] => run(fold(current,q(())))
      case q : State.Success[Unit,O,A] => current
      case q : State.Failure[Unit,O,A] => current
    }
  }

  /*
    Compose
   */
  def compose[A,B,C,D,ZZ](r0: Result[A,B,ZZ], r1: Result[B,C,D]) : Result[A,C,D] = {
    Result(compose(r0.state, r1.state),r1.output)
  }

  def continueCompose[A,B,C,D,ZZ](s0: State.Continue[A,B,ZZ], s1: State.Continue[B,C,D]) : State.Continue[A,C,D] = {
    new State.Continue[A,C,D] {

      override def apply(xs : Seq[A]) : Result[A,C,D] = {
        val r0 : Result[A,B,ZZ] = s0(xs)
        val r1 : Result[B,C,D] = s1(r0.output)
        ifSuccess_ForceR1DoneAndCompose_Otherwise_JustCompose(r0,r1)
      }

      def apply(x: A) = {
        val r0 : Result[A,B,ZZ] = s0(x)
        val r1  : Result[B,C,D] = s1(r0.output)
        ifSuccess_ForceR1DoneAndCompose_Otherwise_JustCompose(r0,r1)
      }

      private def ifSuccess_ForceR1DoneAndCompose_Otherwise_JustCompose(r0 : Result[A,B,ZZ], r1 : Result[B,C,D]) = {
        r0.state.fold(
          ifContinue = { q => compose(r0,r1) },
          ifSuccess = { q =>
            // If r0 is Success, feed an EOI to r1 since r1 will not receive further input
            compose(r0, forceDoneResult(r1))
          },
          ifFailure = { q => compose(r0,r1)}
        )
      }

      def apply(x: EndOfInput) = {
        val eoi_r0 : Result[A,B,ZZ] = s0(x)
        val r1 : Result[B,C,D] = s1(eoi_r0.output)
        val eoi_r1 : Result[B,C,D] = forceDoneResult(r1)
        compose(eoi_r0, eoi_r1)
      }
    }
  }

  def compose[A,B,C,D,ZZ](s0 : State[A,B,ZZ], s1 : State[B,C,D]) : State[A,C,D] = {
    s0.fold(
      ifContinue = { s0 =>
        s1.fold(
          ifContinue = { s1 => continueCompose(s0, s1) },
          ifSuccess = { s1 => State.Success(s1.value) },
          ifFailure = { s1 => State.Failure() }
        )
      },
      ifSuccess = { s0 =>
        s1.fold(
          ifContinue = { s1 => State.Failure()},
          ifSuccess = { s1 => State.Success(s1.value) },
          ifFailure = { s1 => State.Failure() }
        )
      },
      ifFailure = { s0 =>
        s1.fold(
          ifContinue = { s1 => State.Failure() },
          ifSuccess = { s1 => State.Failure() },
          ifFailure = { s1 => State.Failure() }
        )
      }
    )
  }

  def compose[A,B,C,D,ZZ](m0 : StateMachine[A,B,ZZ], m1 : StateMachine[B,C,D]) : StateMachine[A,C,D] = new StateMachine[A,C,D] {
    def s0 = compose(m0.s0,m1.s0)
  }

  /*
    Iteratee.State map/FlatMap
   */
  def mapState[I,A,B](s : State[I,Unit,A], f: A => B) : State[I,Unit,B] = {
    flatMapState(s, { (a : A) => bindState(f(a)) } )
  }

  def bindState[I,A](value : A) : State[I,Unit,A] = State.Success(value)

  def flatMapResult[I,A,B](r : Result[I,Unit,A], f: A => State[I,Unit,B]) : Result[I,Unit,B] = {
    r.state.fold(
      ifContinue = { q => Result(
        state = ContinueStateFlatMap(q,f),
        metadata = r.metadata
      )},
      ifSuccess = { q =>
        val overflowResult = applyInput(f(q.value),r.overflow)
        overflowResult.copy(metadata = overflowResult.metadata ++ r.metadata)
      },
      ifFailure = { q => Result(
        state = State.Failure(),
        overflow = r.overflow,
        metadata = r.metadata
      )}
    )
  }
  
  private[utility] case class ContinueStateFlatMap[I,A,B](s : State.Continue[I,Unit,A], f: A => State[I,Unit,B]) extends State.Continue[I,Unit,B] {
    override def apply(xs: Seq[I]) = flatMapResult(s.apply(xs),f)
  
    def apply(x: I) = flatMapResult(s.apply(x),f)
  
    def apply(x: EndOfInput) = flatMapResult(s.apply(x),f)
  }
  
  def flatMapState[I,A,B](s : State[I,Unit,A], f: A => State[I,Unit,B]) : State[I,Unit,B] = {
    s.fold(
      ifContinue = { q => ContinueStateFlatMap(q,f) },
      ifSuccess = { q => f(q.value) },
      ifFailure = { q => State.Failure() }
    )
  }


  /*
    Iteratee map/FlatMap
   */
  def mapStateMachine[I,A,B](m : StateMachine[I,Unit,A], f: A => B) : StateMachine[I,Unit,B] = {
    flatMapStateMachine(m, { (a: A) => bindStateMachine(f(a)) })
  }

  def bindStateMachine[I,A](value : A) = new StateMachine[I,Unit,A] {
    def s0 = bindState(value)
  }

  def flatMapStateMachine[I,A,B](m : StateMachine[I,Unit,A], f: A => StateMachine[I,Unit,B]) : StateMachine[I,Unit,B] =
    new StateMachine[I,Unit,B] {
      def s0 = flatMapState(m.s0, { (a : A) => f(a).s0 })
    }

}

