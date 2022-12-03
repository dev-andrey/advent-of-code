name         := "advent-of-code"
version      := "2022"
scalaVersion := "2.13.10"

scalacOptions ++= Seq("-Xlint", "-feature", "-Xsource:3") ++ Seq("-encoding", "utf8")

libraryDependencies += compilerPlugin("org.polyvariant" % "better-tostring" % "0.3.17" cross CrossVersion.full)
