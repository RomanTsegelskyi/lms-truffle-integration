name := "lms-truffle-integration"

scalaVersion := "2.10.2"

scalaOrganization := "org.scala-lang.virtualized"

resolvers += "Truffle" at "http://lafo.ssw.uni-linz.ac.at/nexus/content/repositories/releases/"

libraryDependencies += "com.oracle" % "truffle" % "0.5"

libraryDependencies += "org.scala-lang.virtualized" % "scala-compiler" % "2.10.2"

libraryDependencies += "org.scala-lang.virtualized" % "scala-library" % "2.10.2"

libraryDependencies += "org.scala-lang.virtualized" % "scala-reflect" % "2.10.2"

libraryDependencies += "EPFL" %% "lms" % "0.3-SNAPSHOT"

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "2.0.M5b" % "test"

libraryDependencies += "org.scala-lang.virtualized" % "scala-actors" % "2.10.2" % "test"

scalacOptions += "-Yvirtualize"
