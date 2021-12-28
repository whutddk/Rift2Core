

// See README.md for license details.

ThisBuild / scalaVersion     := "2.12.13"
ThisBuild / version          := "0.8.0"
ThisBuild / organization     := "WUT"

lazy val rocketchip = (project in file("./rocket-chip"))
  .settings(
    name := "rocketchip",
    libraryDependencies ++= Seq(
      "edu.berkeley.cs" %% "chisel3" % "3.4.3",
      "edu.berkeley.cs" %% "chiseltest" % "0.3.2" % "test",
      "edu.berkeley.cs" %% "firrtl-interpreter" % "1.4.3",
      "edu.berkeley.cs" %% "chisel-iotesters" % "1.5.3"
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

lazy val inclusiveCache = (project in file("./block-inclusivecache-sifive"))
  .dependsOn(rocketchip)
  .settings(
    name := "inclusiveCache",
    libraryDependencies ++= Seq(
      "edu.berkeley.cs" %% "chisel3" % "3.4.3",
      "edu.berkeley.cs" %% "chiseltest" % "0.3.2" % "test",
      "edu.berkeley.cs" %% "firrtl-interpreter" % "1.4.3",
      "edu.berkeley.cs" %% "chisel-iotesters" % "1.5.3"
      // "org.scalatest" % "scalatest_2.11" % "2.2.4",
      // "org.scalacheck" %% "scalacheck" % "1.12.4"
    ),
    scalaSource in Compile := baseDirectory.value / "design" / "craft" / "inclusivecache",
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

lazy val sifiveBlocks = (project in file("./sifive-blocks"))
  .dependsOn(rocketchip)
  .settings(
    name := "sifive-blocks",
    libraryDependencies ++= Seq(
      "edu.berkeley.cs" %% "chisel3" % "3.4.3",
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

lazy val hardfloat = (project in file("./berkeley-hardfloat"))
  .dependsOn(rocketchip)
  .settings(
    name := "berkeley-hardfloat",
    libraryDependencies ++= Seq(
      "edu.berkeley.cs" %% "chisel3" % "3.4.3",
      // "org.scalatest" % "scalatest_2.11" % "2.2.4",
      // "org.scalacheck" %% "scalacheck" % "1.12.4"
    ),
    scalacOptions ++= Seq(
      "-Xsource:2.11",
      "-language:reflectiveCalls",
      "-deprecation",
      "-feature",
      "-Xcheckinit"
    )
)

lazy val root = (project in file("."))
  .dependsOn(inclusiveCache)
  .dependsOn(rocketchip)
  .dependsOn(sifiveBlocks)
  .dependsOn(hardfloat)
  .settings(
    name := "%NAME%",
    libraryDependencies ++= Seq(
      "edu.berkeley.cs" %% "chisel3" % "3.4.3",
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

