

// See README.md for license details.

ThisBuild / scalaVersion     := "2.12.9"

ThisBuild / version          := "2.3.4"//-SNAPSHOT
ThisBuild / organization     := "io.github.whutddk"


lazy val rocketchip = (project in file("./rocket-chip"))
  .settings(
    name := "rocketchip",
    libraryDependencies ++= Seq(
      "edu.berkeley.cs" %% "chisel3" % "3.5.4",
    ),
    scalacOptions ++= Seq(
      "-language:reflectiveCalls",
      "-Xcheckinit"
    ),
    addCompilerPlugin("edu.berkeley.cs" % "chisel3-plugin" % "3.5.4" cross CrossVersion.full),
    addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full)
)

lazy val inclusiveCache = (project in file("./block-inclusivecache-sifive"))
  .dependsOn(rocketchip)
  .settings(
    name := "inclusiveCache",
    libraryDependencies ++= Seq(
      "edu.berkeley.cs" %% "chisel3" % "3.5.4",
    ),
    scalaSource in Compile := baseDirectory.value / "design" / "craft" / "inclusivecache",
    scalacOptions ++= Seq(
      "-Xsource:2.11",
      "-language:reflectiveCalls",
      "-Xcheckinit"
    ),
    addCompilerPlugin("edu.berkeley.cs" % "chisel3-plugin" % "3.5.4" cross CrossVersion.full),
    addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full)
)

lazy val sifiveBlocks = (project in file("./sifive-blocks"))
  .dependsOn(rocketchip)
  .settings(
    name := "sifive-blocks",
    libraryDependencies ++= Seq(
      "edu.berkeley.cs" %% "chisel3" % "3.5.4",
    ),
    scalacOptions ++= Seq(
      "-Xsource:2.11",
      "-language:reflectiveCalls",
      "-Xcheckinit"
    ),
    addCompilerPlugin("edu.berkeley.cs" % "chisel3-plugin" % "3.5.4" cross CrossVersion.full),
    addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full)
)

lazy val hardfloat = (project in file("./berkeley-hardfloat"))
  .dependsOn(rocketchip)
  .settings(
    name := "berkeley-hardfloat",
    libraryDependencies ++= Seq(
      "edu.berkeley.cs" %% "chisel3" % "3.5.4",
    ),
    scalacOptions ++= Seq(
      "-language:reflectiveCalls",
      "-Xcheckinit"
    ),
    addCompilerPlugin("edu.berkeley.cs" % "chisel3-plugin" % "3.5.4" cross CrossVersion.full),
    addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full)
)

lazy val root = (project in file("."))
  .dependsOn(inclusiveCache)
  .dependsOn(rocketchip)
  .dependsOn(sifiveBlocks)
  .dependsOn(hardfloat)
  .settings(
    name := "%NAME%",
    libraryDependencies ++= Seq(
      "edu.berkeley.cs" %% "chisel3" % "3.5.4",
    ),
    scalacOptions ++= Seq(
      "-Xsource:2.11",
      "-language:reflectiveCalls",
      "-Xcheckinit"
    ),
    addCompilerPlugin("edu.berkeley.cs" % "chisel3-plugin" % "3.5.4" cross CrossVersion.full),
    addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full)
  )


name := "Rift2Core"
homepage := Some(url("https://github.com/whutddk/Rift2Core"))
licenses :=
  Seq(
    "Apache 2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0"), 
    "Anti-996 1.0" -> url("https://github.com/996icu/996.ICU/blob/master/LICENSE")
  )

publishMavenStyle := true
publishArtifact in Test := false
pomIncludeRepository := { _ => false }
publishTo := {
  val nexus = "https://s01.oss.sonatype.org/"
  if (isSnapshot.value) Some("snapshots" at nexus + "content/repositories/snapshots")
  else Some("releases" at nexus + "service/local/staging/deploy/maven2")
}
sonatypeRepository := "https://s01.oss.sonatype.org/service/local"
sonatypeCredentialHost := "s01.oss.sonatype.org"
scmInfo := Some(
  ScmInfo(
    url("https://github.com/whutddk/Rift2Core"),
    "scm:git:git@github.com:whutddk/Rift2Core.git"
  )
)

developers := List(
  Developer("Ruige", "Ruige Lee", "295054118@whut.edu.cn", url("http://whutddk.github.io"))
)

