Global / onChangedBuildSource := ReloadOnSourceChanges

name         := "advent-of-code"
version      := "2023"
scalaVersion := "3.3.1"

libraryDependencies ++= Seq(
  "org.scalanlp"     %% "breeze"       % "2.1.0",
  ("org.scala-graph" %% "graph-core"   % "2.0.1").cross(CrossVersion.for3Use2_13),
  "org.jgrapht"       % "jgrapht-core" % "1.5.1"
)
