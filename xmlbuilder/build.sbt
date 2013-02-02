scalaVersion := "2.10.0-RC5"

organization := "org.gtri.util"

name := "iteratee.impl"

version := "1.0-SNAPSHOT"

scalacOptions ++= Seq("-feature","-unchecked", "-deprecation")

resolvers += "iead-all" at "https://iead.ittl.gtri.org/artifactory/all" // need this for getting IEAD artifacts

libraryDependencies += "org.scalatest" % "scalatest_2.10.0-RC5" % "2.0.M5-B1"

//libraryDependencies += "org.gtri.util" %% "issue.impl" % "1.0-SNAPSHOT"

libraryDependencies += "org.gtri.util" %% "scala.exelog" % "1.0-SNAPSHOT"

//libraryDependencies += "org.gtri.util" %% "scala.optrecover" % "1.0-SNAPSHOT"

libraryDependencies += "org.gtri.util" % "xsddatatypes" % "1.0-SNAPSHOT"

libraryDependencies += "org.gtri.util" %% "scala.statemachine" % "1.0-SNAPSHOT"

//libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.0.0-M6" cross CrossVersion.full

publishTo <<= {    // set publish repository url according to whether `version` ends in "-SNAPSHOT"
  val releases = "iead-artifactory" at "https://iead.ittl.gtri.org/artifactory/internal"
  val snapshots = "iead-artifactory-snapshots" at "https://iead.ittl.gtri.org/artifactory/internal-snapshots"
  version { v =>
    if (v.endsWith("-SNAPSHOT"))
      Some(snapshots)
    else Some(releases)
  }
}