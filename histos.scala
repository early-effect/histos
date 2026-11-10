import histos.*
import histos.plugin.SecretName
import histos.supply.{PrAuthor, SupplyChain, UpdatePolicy}

object build extends HistosBuild:
  def graph =
    val scala3     = scala"3.10.0"
    val zio        = dep"dev.zio::zio:2.1.27"
    val zioTest    = dep"dev.zio::zio-test:2.1.27"
    val zioTestSbt = dep"dev.zio::zio-test-sbt:2.1.27"
    val api        = module("api", Jvm, Native)(scala3)
      .clang(ClangMajor(21))
      .artifact(artifact"histos-api")
      .libraryDependencies(zio)
      .testDependencies(zioTest, zioTestSbt)
    val zioJson = dep"dev.zio::zio-json:1.1.1"
    val wire    = module("wire", Jvm, Native)(scala3)
      .clang(ClangMajor(21))
      .dependsOn(api)
      .libraryDependencies(zio, zioJson)
      .testDependencies(zioTest, zioTestSbt)
    val central = module("central", Jvm)(scala3)
      .dependsOn(api, wire)
      .libraryDependencies(zio, dep"rocks.earlyeffect::heddle:0.14.2")
      .testDependencies(zioTest, zioTestSbt, dep"rocks.earlyeffect::slayer:0.2.0")
    val engine = module("engine", Jvm)(scala3)
      .dependsOn(central, api, wire)
      .libraryDependencies(
        zio,
        zioJson,
        dep"dev.zio::zio-streams:2.1.27",
        dep"org.scala-lang::scala3-compiler:3.10.0",
        dep"org.scala-lang::scaladoc:3.10.0",
        dep"org.scala-sbt::zinc:2.0.5",
        // Zinc still requests jars whose lazy vals call Unsafe.
        dep"org.scala-sbt::sbinary:0.6.0",
        dep"org.scala-lang.modules::scala-parser-combinators:2.5.0",
        dep"io.get-coursier::coursier:2.1.26",
        dep"org.ow2.asm:asm:9.11.1",
        dep"org.ow2.asm:asm-tree:9.11.1",
        // scala3-compiler 3.10.0 still requests the rest of ASM at 9.10.1.
        dep"org.ow2.asm:asm-commons:9.11.1",
        dep"org.ow2.asm:asm-util:9.11.1",
        dep"org.ow2.asm:asm-analysis:9.11.1",
        dep"org.bouncycastle:bcpg-jdk18on:1.86",
        // The stable Java API `histos fmt` loads scalafmt-dynamic-core through. The rest of scalafmt is a locked tool.
        dep"org.scalameta:scalafmt-interfaces:3.11.5",
        // heddle has no gRPC, and the remote-execution stubs are not published.
        dep"io.grpc:grpc-api:1.85.1",
        dep"io.grpc:grpc-netty-shaded:1.85.1",
        // scaladoc and flexmark still request Jackson and jsoup below the GHSA fixes.
        dep"com.fasterxml.jackson.core:jackson-annotations:2.22",
        dep"com.fasterxml.jackson.core:jackson-core:2.22.4",
        dep"com.fasterxml.jackson.core:jackson-databind:2.22.4",
        dep"com.fasterxml.jackson.datatype:jackson-datatype-jsr310:2.22.4",
        dep"tools.jackson.core:jackson-core:3.2.4",
        dep"tools.jackson.core:jackson-databind:3.2.4",
        dep"tools.jackson.dataformat:jackson-dataformat-yaml:3.2.4",
        dep"org.jsoup:jsoup:1.24.1",
        // gRPC 1.85.1 still requests guava 33.6.0-android, inside GHSA-xxph-c9ww-hj94.
        dep"com.google.guava:guava:33.8.0-jre",
      )
      .testDependencies(
        zioTest,
        zioTestSbt,
        dep"rocks.earlyeffect::slayer:0.2.0",
        dep"io.grpc:grpc-stub:1.85.1",
      )
    val client = module("client", Native)(scala3)
      .clang(ClangMajor(21))
      .dependsOn(wire)
      .libraryDependencies(zio, zioJson)
      .main(MainClass("histos.client.Main"))
    val app = module("app", Jvm)(scala3)
      .scalac(ScalacFlag.MaxInlines)
      .artifact(artifact"histos")
      .assembly(MainClass("histos.cli.Main"))
      .asset(asset"dist/histos.jar")
      .dependsOn(engine, central, api, wire)
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
        dep"rocks.earlyeffect::slayer:0.2.0",
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
      )
      .supplyChain(
        SupplyChain(updates =
          UpdatePolicy(author =
            // The published driver compiles this file. It has App, and it does not have PrAuthor.histosDeps.
            PrAuthor.App(SecretName("HISTOS_APP_ID"), SecretName("HISTOS_APP_PRIVATE_KEY"))
          )
        )
      )
      .coverageExcludes(CoverageExcludes(classlikes = List(".*Macro.*")))
      .scalafmt
      .docCheck(
        ships(Ship("histos").next(0, 2, 7)(api, app)),
        api,
        wire,
        client,
        central,
        engine,
        app,
      )
