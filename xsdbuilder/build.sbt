scalaVersion := "2.10.0"

organization := "org.gtri.util"

name := "xsdbuilder.impl"

version := "1.0-SNAPSHOT"

scalacOptions ++= Seq("-feature", "-unchecked", "-deprecation")

resolvers += "iead-all" at "https://iead.ittl.gtri.org/artifactory/all" // need this for getting IEAD artifacts

resolvers += Resolver.file("my-local-repo", file("c:/users/lance/.ivy2/cache")) transactional()

libraryDependencies += "org.scalatest" %% "scalatest" % "2.0.M5b"

libraryDependencies += "org.gtri.util" % "xsddatatypes" % "1.0-SNAPSHOT"

libraryDependencies += "org.gtri.util" %% "scala.statemachine" % "1.0-SNAPSHOT"

libraryDependencies += "org.gtri.util" %% "scala.xmlbuilder" % "1.0-SNAPSHOT"

publishTo <<= {    // set publish repository url according to whether `version` ends in "-SNAPSHOT"
  val releases = "iead-artifactory" at "https://iead.ittl.gtri.org/artifactory/internal"
  val snapshots = "iead-artifactory-snapshots" at "https://iead.ittl.gtri.org/artifactory/internal-snapshots"
  version { v =>
    if (v.endsWith("-SNAPSHOT"))
      Some(snapshots)
    else Some(releases)
  }
}