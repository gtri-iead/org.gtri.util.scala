package org.gtri.util.scala

package object exelog {
  implicit class loggerMethods(self: Logger) {
    def enter(methodName: String, args: => Seq[(String,Any)] = Nil)(implicit c : Class[_]) : Log = {
      val log = self.getLog(c.getSimpleName + "." + methodName, c.getPackage.getName)
      log trace { "ENTER " + c.getSimpleName + "." + methodName + formatArgs(args) }
      log
    }

    def formatArg(arg : Any) : String = {
      if(arg == null) {
        "(null)"
      } else {
        arg match {
          // if a string, format with single-quotes and replace newlines with literal \n
          // TODO: limit string length
          case a : String => "'" + a.replaceAll("\n","""\\n""") + "'"
          // TODO: for known containers (traversable) limit length
          case a : Any => a.toString
        }
      }
    }

    // format args (arg0=val0,arg1=val1,...)
    def formatArgs(args: Seq[(String, Any)]) = {
      "(" + (args map { t => t._1 + "=" + formatArg(t._2) } mkString ",") + ")"
    }
  }

  def enter(methodName: String)(args: => Seq[(String,Any)] = Nil)(implicit c : Class[_], logger : Logger) : Log = {
    logger.enter(methodName, args)(c)
  }

  implicit class logMethods(self: Log) {
    def :~>[A](a : => A) : A = { exit(a); a }
    def <~:[A](a : => A) : A = { exit(a); a }
    def exit[A](a : => A) { self trace ("EXIT " + self.name + " => " + a.toString) }

  }

  implicit class logMe(message : => String) {
    def unary_~(implicit log : Log) = log trace message
    def unary_+(implicit log : Log) = log debug message
  }

}
