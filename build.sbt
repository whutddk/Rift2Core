

// See README.md for license details.

ThisBuild / scalaVersion     := "2.13.10"

ThisBuild / version          := "2.3.5"//-SNAPSHOT
ThisBuild / organization     := "io.github.whutddk"
val chiselVersion = "3.5.6"


lazy val commonSettings = Seq(
  // organization := "edu.berkeley.cs",
  // version := "1.6",
  assembly / assemblyMergeStrategy := { _ match {
    case PathList("META-INF", "MANIFEST.MF") => MergeStrategy.discard
    case _ => MergeStrategy.first}},
  scalacOptions ++= Seq(
    "-deprecation",
    "-unchecked",
    "-Ymacro-annotations"), // fix hierarchy API
)

lazy val chiselSettings = Seq(
  libraryDependencies ++= Seq("edu.berkeley.cs" %% "chisel3" % chiselVersion,
  "org.apache.commons" % "commons-lang3" % "3.12.0",
  "org.apache.commons" % "commons-text" % "1.9"),
  addCompilerPlugin("edu.berkeley.cs" % "chisel3-plugin" % chiselVersion cross CrossVersion.full))

/**
  * It has been a struggle for us to override settings in subprojects.
  * An example would be adding a dependency to rocketchip on midas's targetutils library,
  * or replacing dsptools's maven dependency on chisel with the local chisel project.
  *
  * This function works around this by specifying the project's root at src/ and overriding
  * scalaSource and resourceDirectory.
  */
def freshProject(name: String, dir: File): Project = {
  Project(id = name, base = dir / "src")
    .settings(
      Compile / scalaSource := baseDirectory.value / "main" / "scala",
      Compile / resourceDirectory := baseDirectory.value / "main" / "resources"
    )
}

// Rocket-chip dependencies (subsumes making RC a RootProject)
lazy val hardfloat  = (project in file("./dependencies/rocketchip/berkeley-hardfloat"))
  .settings(chiselSettings)
  .settings(commonSettings)
  .settings(name := "hardfloat", organization := "edu.berkeley.cs")
  .settings(
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      "org.json4s" %% "json4s-jackson" % "3.6.6",
      "org.scalatest" %% "scalatest" % "3.2.0" % "test"
    )
  )

lazy val rocketMacros  = (project in file("./dependencies/rocket-chip/macros"))
  .settings(chiselSettings)
  .settings(commonSettings)
  .settings(name := "rocketMacros", organization := "org.chipsalliance")
  .settings(
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      "org.json4s" %% "json4s-jackson" % "3.6.6",
      "org.scalatest" %% "scalatest" % "3.2.0" % "test"
    )
  )

lazy val rocketConfig = (project in file("./dependencies/rocket-chip/cde/cde/src/chipsalliance/rocketchip"))
  .settings(chiselSettings)
  .settings(commonSettings)
  .settings(name := "rocketConfig", organization := "org.chipsalliance")
  .settings(commonSettings)
  .settings(
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      "org.json4s" %% "json4s-jackson" % "3.6.6",
      "org.scalatest" %% "scalatest" % "3.2.0" % "test"
    )
  )

lazy val rocketchip = freshProject("rocketchip", file("./dependencies/rocket-chip"))
  .dependsOn(hardfloat, rocketMacros, rocketConfig)
  .settings(commonSettings)
  .settings(chiselSettings)
  .settings(name := "rocketchip", organization := "org.chipsalliance", version := "1.6")
  .settings(
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      "org.json4s" %% "json4s-jackson" % "3.6.6",
      "org.scalatest" %% "scalatest" % "3.2.0" % "test"
    )
  )
  .settings( // Settings for scalafix
    semanticdbEnabled := true,
    semanticdbVersion := "4.7.6",
    scalacOptions += "-Ywarn-unused"
  )
lazy val rocketLibDeps = (rocketchip / Keys.libraryDependencies)

lazy val constellation = (project in file("./dependencies/constellation"))
  .dependsOn(rocketchip)
  .settings(chiselSettings)
  .settings(commonSettings)
  .settings(name := "constellation", organization := "edu.berkeley.cs")
  .settings(
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      "org.json4s" %% "json4s-jackson" % "3.6.6",
      "org.scalatest" %% "scalatest" % "3.2.0" % "test"
    )
  )


lazy val inclusiveCache = (project in file("./dependencies/inclusivecache"))
  .dependsOn(rocketchip)
  .settings(chiselSettings)
  .settings(commonSettings)
  .settings(name := "inclusiveCache", organization := "org.chipsalliance")
  .settings(scalaSource in Compile := baseDirectory.value / "design" / "craft" / "inclusivecache")
  .settings(
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      "org.json4s" %% "json4s-jackson" % "3.6.6",
      "org.scalatest" %% "scalatest" % "3.2.0" % "test"
    )
  )

lazy val blocks = (project in file("./dependencies/blocks"))
  .dependsOn(rocketchip)
  .settings(chiselSettings)
  .settings(commonSettings)  
  .settings(name := "blocks", organization := "org.chipsalliance")
  .settings(
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      "org.json4s" %% "json4s-jackson" % "3.6.6",
      "org.scalatest" %% "scalatest" % "3.2.0" % "test"
    )
  )



lazy val root = (project in file("."))
  .dependsOn(inclusiveCache)
  .dependsOn(rocketchip)
  .dependsOn(blocks)
  .dependsOn(hardfloat)
  .dependsOn(constellation)  

  .settings(commonSettings)
  .settings(chiselSettings)

  .settings(
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      "org.json4s" %% "json4s-jackson" % "3.6.6",
      "org.scalatest" %% "scalatest" % "3.2.0" % "test"
    )
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

