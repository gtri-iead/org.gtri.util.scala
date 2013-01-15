package org.gtri.util.scala.exelog

package object sideffects {
  type Logger = Log4jLogger
  type Log = Log4jLog
  implicit val logger = new Logger

  implicit class logMethods(self: Log)(implicit c : Class[_]) {
    @inline def block[A](methodName: String)(f: => A) : A = block(methodName, Nil)(f)
    def block[A](methodName: String, args : => Seq[(String,Any)])(f: => A) : A = {
      begin(methodName, args)
      // Force evaluation of f here
      val retv : A = f
      end(methodName, retv)
      retv
    }


    @inline def begin(methodName : String) : Unit = begin(methodName, Nil)
    def begin(methodName : String, args: => Seq[(String,Any)]) {
      self trace { Helpers.enterMessage(c, methodName, args) }
    }


    @inline def end(methodName : String) : Unit = end(methodName, ())
    def end[A](methodName : String, retv : A) {
      self trace { Helpers.exitMessage(c, methodName, retv) }
    }

  }

  implicit class logMe(message : => String) {
    @inline def unary_~(implicit log : Log) = log trace message
    @inline def unary_+(implicit log : Log) = log debug message
  }

}
