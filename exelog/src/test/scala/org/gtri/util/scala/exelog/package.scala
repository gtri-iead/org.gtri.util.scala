package org.gtri.util.scala

package object exelog {
  def runTimeTrial[A](n : Int)(f: => Unit) {
    val startTime = System.nanoTime()
    for(i <- 1 to n) {
      f
    }
    val endTime = System.nanoTime()
    println("Time (ms)=" + (endTime - startTime) / 1000000d)
  }
}
