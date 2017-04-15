name := "scala-chess-game"

version := "1.0"

scalaVersion := "2.11.7"

libraryDependencies ++= Seq("org.specs2" %% "specs2-core" % "3.6.2" % "test")

resolvers += "scalaz-bintray" at "http://dl.bintray.com/scalaz/releases"

scalacOptions in Test ++= Seq("-Yrangepos")