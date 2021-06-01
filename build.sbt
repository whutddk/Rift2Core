

// See README.md for license details.

ThisBuild / scalaVersion     := "2.12.12"
ThisBuild / version          := "0.8.0"
ThisBuild / organization     := "WUT"

lazy val root = (project in file("."))
  .settings(
    name := "%NAME%",
    libraryDependencies ++= Seq(
      "edu.berkeley.cs" %% "chisel3" % "3.4.2",
      "edu.berkeley.cs" %% "chiseltest" % "0.3.2" % "test",
      "edu.berkeley.cs" %% "firrtl-interpreter" % "1.4.3",
      "edu.berkeley.cs" %% "chisel-iotesters" % "1.5.3"
      // "org.scalatest" % "scalatest_2.11" % "2.2.4",
      // "org.scalacheck" %% "scalacheck" % "1.12.4"
    ),
    scalacOptions ++= Seq(
      "-Xsource:2.11",
      "-language:reflectiveCalls",
      "-deprecation",
      "-feature",
      "-Xcheckinit"
    ),
    addCompilerPlugin("edu.berkeley.cs" % "chisel3-plugin" % "3.4.2" cross CrossVersion.full),
    addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full)
)
