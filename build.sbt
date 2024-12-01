Global / onChangedBuildSource := ReloadOnSourceChanges

name         := "advent-of-code"
version      := "2024"
scalaVersion := "3.5.2"

libraryDependencies ++= Seq(
  "org.scalanlp"     %% "breeze"       % "2.1.0",
  ("org.scala-graph" %% "graph-core"   % "2.0.2").cross(CrossVersion.for3Use2_13),
  "org.jgrapht"       % "jgrapht-core" % "1.5.2"
)
