import histos.*

object build extends HistosBuild:
  def graph =
    val scala3 = scala"3.10.0"
    val zio = dep"dev.zio::zio:2.1.27"
    val zioTest = dep"dev.zio::zio-test:2.1.27"
    val zioTestSbt = dep"dev.zio::zio-test-sbt:2.1.27"
    val api = module("api", Jvm)(scala3)
      .artifact(artifact"histos-api")
      .libraryDependencies(zio)
      .testDependencies(zioTest, zioTestSbt)
    val central = module("central", Jvm)(scala3)
      .dependsOn(api)
      .libraryDependencies(zio, dep"rocks.earlyeffect::heddle:0.13.0")
      .testDependencies(zioTest, zioTestSbt)
    val engine = module("engine", Jvm)(scala3)
      .dependsOn(central, api)
      .libraryDependencies(
        zio,
        dep"dev.zio::zio-streams:2.1.27",
        dep"org.scala-lang::scala3-compiler:3.10.0",
        dep"org.scala-sbt::zinc:2.0.4",
        // Zinc 2.0.4 still requests jars whose lazy vals call Unsafe.
        dep"org.scala-sbt::sbinary:0.6.0",
        dep"org.scala-lang.modules::scala-parser-combinators:2.5.0",
        dep"io.get-coursier::coursier:2.1.26",
        dep"org.ow2.asm:asm:9.11.1",
        dep"org.ow2.asm:asm-tree:9.11.1",
        dep"org.apache.commons:commons-compress:1.28.0",
        dep"org.tukaani:xz:1.12"
      )
      .testDependencies(zioTest, zioTestSbt)
    val app = module("app", Jvm)(scala3)
      .dependsOn(engine, central, api)
      .libraryDependencies(
        zio,
        dep"rocks.earlyeffect::heddle-mcp:0.13.0",
        dep"dev.zio::zio-cli:0.8.2",
        // zio-cli 0.8.2 still requests zio-process 0.7.1, whose lazy vals call Unsafe.
        dep"dev.zio::zio-process:0.8.1"
      )
      .testDependencies(
        zioTest,
        zioTestSbt,
        dep"org.ow2.asm:asm:9.11.1",
        dep"org.ow2.asm:asm-tree:9.11.1"
      )
    Projects
      .organization(organization"rocks.earlyeffect")
      .assembly(artifact"histos")(
        ships(manual("histos", manual"0.0.1-SNAPSHOT")(api)),
        api,
        central,
        engine,
        app
      )
