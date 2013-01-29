package org.gtri.util.scala.statemachine.Enumerable

import org.gtri.util.scala.statemachine._
import org.gtri.util.scala.statemachine.utility

object impl {
  private[impl] def stepEnumerableResult[O,A](current: Result[O,A]) : Result[O,A] = {
    current.state.fold(
      ifContinue = { q => utility.foldResults(current,q(())) },
      ifSuccess = { _ => current },
      ifFailure = { _ => current }
    )
  }

  /**
   * Step an Enumerable.State (apply an instance of unit to the state)
   * @param s
   * @tparam O
   * @tparam A
   * @return
   */
  def stepEnumerableState[O,A](s: State[O,A]) : Result[O,A] = {
    s.fold(
      ifContinue = { q => stepEnumerableResult(Result(q)) },
      ifSuccess = { q => Result(q) },
      ifFailure = { q => Result(q)}
    )
  }

  /**
   * Step an Enumerable (apply an instance of unit to the s0 state)
   * @param m
   * @tparam O
   * @tparam A
   * @return
   */
  def stepEnumerable[O,A](m : Enumerable[O,A]) : Result[O,A] = stepEnumerableState(m.s0)

  private[impl] def runEnumerableResult[O,A](r0 : Result[O,A], recover : Boolean) : Result[O,A] = {
    var done = false
    val r = utility.AccResult(r0)
    do {
      r.state.fold(
        ifContinue = { q => r.append(q(())) },
        ifSuccess = { q => done = true },
        ifFailure = { q =>
          if(recover && q.optRecover.isDefined) {
            r.append(q.optRecover.get.apply())
          } else {
            done = true
          }
        }
      )
    } while(done == false)
    r.toResult
  }

  // Previous functional implementation of runEnumerableResult
  //  @tailrec def runEnumerableResult[O,A](current: Result[O,A], buffer: scala.collection.mutable.Buffer) : Result[O,A] = {
  //    // Note: can't use state.foldResults because of tailrec optimization
  //    current.state match {
  //      case q : State.Continue[O,A] => runEnumerableResult(foldResults(current,q(())))
  //      case q : State.Success[O,A] => current
  //      case q : State.Failure[O,A] => current
  //    }
  //  }

  /**
   * Step an Enumerable.State until it returns Success/Failure with the option to recover from all recoverable Failures
   * @param state
   * @param recover TRUE to recover from all recoverable Failures
   * @tparam O
   * @tparam A
   * @return
   */
  def runEnumerableState[O,A](s: State[O,A], recover: Boolean) : Result[O,A] = runEnumerableResult(Result(s), recover)

  /**
   * Step an Enumerable until it returns Success/Failure with the option to recover from all recoverable Failures
   * @param state
   * @param recover TRUE to recover from all recoverable Failures
   * @tparam O
   * @tparam A
   * @return
   */
  def runEnumerable[O,A](m: Enumerable[O,A], recover: Boolean) : Result[O,A] = runEnumerableState(m.s0, recover)

}
