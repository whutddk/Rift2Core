import mill._
import mill.scalalib._

import $file.dependencies.`rocket-chip`.common
import $file.dependencies.`rocket-chip`.cde.common
// import $file.dependencies.`rocket-chip`.cde.build
import $file.dependencies.`rocket-chip`.hardfloat.build

// import $file.`dependencies/block`.sifiveblocks
// import $file.`dependencies/constellation`.hardfloat.build
// import coursier.maven.MavenRepository

val defaultScalaVersion = "2.13.10"


def defaultVersions(chiselVersion: String) = chiselVersion match {
//   case "chisel" => Map(
//     "chisel"        -> ivy"org.chipsalliance::chisel:6.0.0-M3",
//     "chisel-plugin" -> ivy"org.chipsalliance:::chisel-plugin:6.0.0-M3",
//     "chiseltest"    -> ivy"edu.berkeley.cs::chiseltest:5.0.2"
//   )
  case "chisel3" => Map(
    "chisel"        -> ivy"edu.berkeley.cs::chisel3:3.6.0",
    "chisel-plugin" -> ivy"edu.berkeley.cs:::chisel3-plugin:3.6.0",
    "chiseltest"    -> ivy"edu.berkeley.cs::chiseltest:0.6.2"
  )
}

trait HasChisel extends SbtModule with Cross.Module[String] {
  def chiselModule: Option[ScalaModule] = None
  def chiselPluginJar: T[Option[PathRef]] = None
  def chiselIvy: Option[Dep] = Some(defaultVersions(crossValue)("chisel"))
  def chiselPluginIvy: Option[Dep] = Some(defaultVersions(crossValue)("chisel-plugin"))

  override def scalaVersion = defaultScalaVersion

  override def scalacOptions = super.scalacOptions() ++
    Agg("-language:reflectiveCalls", "-Ymacro-annotations", "-Ytasty-reader")

  override def ivyDeps = super.ivyDeps() ++ Agg(chiselIvy.get)

  override def scalacPluginIvyDeps = super.scalacPluginIvyDeps() ++ Agg(chiselPluginIvy.get)
}


object rocketchip extends Cross[RocketChip]("chisel3")
trait RocketChip extends millbuild.dependencies.`rocket-chip`.common.RocketChipModule with HasChisel {
  def scalaVersion: T[String] = T(defaultScalaVersion)

  override def millSourcePath = os.pwd / "dependencies"/ "rocket-chip"

  def macrosModule = macros

  def hardfloatModule = hardfloat(crossValue)

  def cdeModule = cde

  def mainargsIvy = ivy"com.lihaoyi::mainargs:0.5.4"

  def json4sJacksonIvy = ivy"org.json4s::json4s-jackson:4.0.6"

  object macros extends Macros

  trait Macros extends millbuild.dependencies.`rocket-chip`.common.MacrosModule with SbtModule {

    def scalaVersion: T[String] = T(defaultScalaVersion)

    def scalaReflectIvy = ivy"org.scala-lang:scala-reflect:${defaultScalaVersion}"
  }

  object hardfloat extends Cross[Hardfloat](crossValue)

  trait Hardfloat extends millbuild.dependencies.`rocket-chip`.hardfloat.common.HardfloatModule with HasChisel {

    def scalaVersion: T[String] = T(defaultScalaVersion)

    override def millSourcePath = os.pwd /"dependencies"/ "rocket-chip" / "hardfloat" / "hardfloat"

  }

  object cde extends CDE

  trait CDE extends millbuild.dependencies.`rocket-chip`.cde.common.CDEModule with ScalaModule {

    def scalaVersion: T[String] = T(defaultScalaVersion)

    override def millSourcePath = os.pwd /"dependencies"/ "rocket-chip" / "cde" / "cde"
  }
}



trait BlocksModule extends ScalaModule{
  def rocketModule: ScalaModule

  override def moduleDeps = super.moduleDeps ++ Seq(
    rocketModule,
  )  
}

object blocks extends Cross[Blocks]("chisel3")
trait Blocks extends BlocksModule with HasChisel {
  def rocketModule = rocketchip(crossValue)
  def scalaVersion: T[String] = T(defaultScalaVersion)

  override def millSourcePath = os.pwd /"dependencies"/ "blocks"
}

trait ConstellationModule extends ScalaModule{
  def rocketModule: ScalaModule
  def cdeModule: ScalaModule
  // override def ivyDeps = Agg(
  //   ivy"edu.berkeley.cs::rocketchip:1.6.0",
  //   ivy"edu.berkeley.cs::cde:1.6.0",
  //   ivy"edu.berkeley.cs::rocket-macros:1.6.0",
  //   ivy"edu.berkeley.cs::chiseltest:0.5.4",
  // )
  override def moduleDeps = super.moduleDeps ++ Seq(
    rocketModule,
    cdeModule,
  )  
}

object constellation extends Cross[Constellation]("chisel3")
trait Constellation extends ConstellationModule with HasChisel {
  def rocketModule = rocketchip(crossValue)
  def cdeModule = rocketchip(crossValue).cde
  def scalaVersion: T[String] = T(defaultScalaVersion)

  override def millSourcePath = os.pwd /"dependencies"/ "constellation"
}


trait InclusiveModule extends ScalaModule{
  def rocketModule: ScalaModule

  override def moduleDeps = super.moduleDeps ++ Seq(
    rocketModule,
  )
}
object inclusive extends Cross[Inclusive]("chisel3")
trait Inclusive extends InclusiveModule with HasChisel {
  def rocketModule = rocketchip(crossValue)
  def scalaVersion: T[String] = T(defaultScalaVersion)

  override def millSourcePath = os.pwd /"dependencies"/ "inclusivecache" / "design" / "craft" / "inclusivecache"
  override def sources = T.sources( millSourcePath / "src"  )
}




trait RiftModule extends ScalaModule{
  def rocketModule: ScalaModule
  def hardfloatModule: ScalaModule
  def blocksModule: ScalaModule
  def constellationModule: ScalaModule
  def inclusiveModule: ScalaModule
  // def cdeModule: ScalaModule


  override def moduleDeps = super.moduleDeps ++ Seq(
    rocketModule,
    hardfloatModule,
    inclusiveModule,
    blocksModule,
    constellationModule,
    // cdeModule,
  )
}

object rift2Core extends Cross[Rift2Core]("chisel3")
trait Rift2Core extends RiftModule with HasChisel{
  override def millSourcePath = os.pwd

    def rocketModule = rocketchip(crossValue)
    def hardfloatModule = rocketchip(crossValue).hardfloat(crossValue)
    def blocksModule = blocks(crossValue)
    def constellationModule = constellation(crossValue)
    def inclusiveModule = inclusive(crossValue)
    // def cdeModule = rocketchip(crossValue).cde

    override def forkArgs = Seq("-Xmx60G", "-Xss16m")

    override def sources = T.sources {
      super.sources() ++ Seq(PathRef(millSourcePath / "src" / crossValue / "main" / "scala"))
    }

    object test extends SbtModuleTests with TestModule.ScalaTest {
      override def forkArgs = Seq("-Xmx20G", "-Xss256m")

      override def sources = T.sources {
        super.sources() ++ Seq(PathRef(this.millSourcePath / "src" / crossValue / "test" / "scala"))
      }

    override def ivyDeps = super.ivyDeps() ++ Agg(
      defaultVersions(crossValue)("chiseltest")
    )
  }
}



