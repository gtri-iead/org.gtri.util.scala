scalaVersion := "2.10.0"

organization := "org.gtri.util"

name := "scala.xmlbuilder"

version := "1.0-SNAPSHOT"

scalacOptions ++= Seq("-feature","-unchecked", "-deprecation")

resolvers += "iead-all" at "https://iead.ittl.gtri.org/artifactory/all" // need this for getting IEAD artifacts

libraryDependencies += "org.scalatest" %% "scalatest" % "2.0.M5b"

//libraryDependencies += "org.gtri.util" %% "scala.exelog" % "1.0-SNAPSHOT"

libraryDependencies += "org.gtri.util" % "xsddatatypes" % "1.0-SNAPSHOT"

libraryDependencies += "org.gtri.util" %% "scala.statemachine" % "1.0-SNAPSHOT"

libraryDependencies += "net.sf.saxon" % "Saxon-HE" % "9.4"

libraryDependencies += "com.googlecode.java-diff-utils" % "diffutils" % "1.2.1" % "test"

publishTo <<= {    // set publish repository url according to whether `version` ends in "-SNAPSHOT"
  val releases = "iead-artifactory" at "https://iead.ittl.gtri.org/artifactory/internal"
  val snapshots = "iead-artifactory-snapshots" at "https://iead.ittl.gtri.org/artifactory/internal-snapshots"
  version { v =>
    if (v.endsWith("-SNAPSHOT"))
      Some(snapshots)
    else Some(releases)
  }
}