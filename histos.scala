import histos.*

object build extends HistosBuild:
  def graph =
    val scala3     = scala"3.10.0"
    val zio        = dep"dev.zio::zio:2.1.27"
    val zioTest    = dep"dev.zio::zio-test:2.1.27"
    val zioTestSbt = dep"dev.zio::zio-test-sbt:2.1.27"
    val api        = module("api", Jvm)(scala3)
      .artifact(artifact"histos-api")
      .libraryDependencies(zio)
      .testDependencies(zioTest, zioTestSbt)
    val central = module("central", Jvm)(scala3)
      .dependsOn(api)
      .libraryDependencies(zio, dep"rocks.earlyeffect::heddle:0.14.2")
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
        dep"org.bouncycastle:bcpg-jdk18on:1.86",
      )
      .testDependencies(zioTest, zioTestSbt)
    val app = module("app", Jvm)(scala3)
      .scalac(ScalacFlag.MaxInlines)
      .artifact(artifact"histos")
      .assembly(MainClass("histos.cli.Main"))
      .asset(asset"dist/histos.jar")
      .dependsOn(engine, central, api)
      .libraryDependencies(
        zio,
        dep"rocks.earlyeffect::heddle-mcp:0.14.2",
        dep"dev.zio::zio-cli:0.8.2",
        // zio-cli 0.8.2 still requests zio-process 0.7.1, whose lazy vals call Unsafe.
        dep"dev.zio::zio-process:0.8.1",
      )
      .testDependencies(
        zioTest,
        zioTestSbt,
        dep"org.ow2.asm:asm:9.11.1",
        dep"org.ow2.asm:asm-tree:9.11.1",
      )
    Projects
      .organization(organization"rocks.earlyeffect")
      .license(License(license"Apache-2.0", url"https://www.apache.org/licenses/LICENSE-2.0.txt"))
      .developer(Developer(developer"russwyte", pom"Russ White"))
      .scm(
        Scm(
          url"https://github.com/early-effect/histos",
          connection"scm:git@github.com:early-effect/histos.git",
        )
      )
      .description(pom"Histos is a Scala build tool.")
      .url(url"https://github.com/early-effect/histos")
      .runnerTools(
        RunnerTools(
          Some(NodeFloor(25)),
          Some(ToolRevision("21.1.8")),
          Some(ToolRevision("1.62.1")),
        )
      )(
        ships(manual("histos", manual"0.0.2")(api, app)),
        api,
        central,
        engine,
        app,
      )
