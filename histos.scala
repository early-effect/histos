import dev.earlyeffect.specular.{Site, SiteCi, Specular}
import histos.*
import histos.plugin.{BuildServices, JobName, NoteKey, NoteText, PluginId, SecretName, Site as _, SitePublish, TaskName}
import histos.supply.{PrAuthor, SupplyChain, UpdatePolicy}
import zio.*

object build extends HistosBuild:
  def graph =
    val scala3        = scala"3.10.1"
    val nativeNightly = NativeVersion("0.5.13-20261122-fa4437d-SNAPSHOT")
    val zio           = dep"dev.zio::zio:2.1.27"
    val zioTest       = zio.module("zio-test")
    val zioTestSbt    = zio.module("zio-test-sbt")
    val zioStreams    = zio.module("zio-streams")
    val zioJson       = dep"dev.zio::zio-json:1.1.1"
    val api           = module("api", Jvm, Native)(scala3)
      .clang(ClangMajor(21))
      .nativeVersion(nativeNightly)
      .artifact(artifact"histos-api")
      .libraryDependencies(zio)
      .testDependencies(zioTest, zioTestSbt)
    val pluginTestkit = module("plugin-testkit", Jvm)(scala3)
      .dependsOn(api)
      .libraryDependencies(zio)
      .testDependencies(zioTest, zioTestSbt)
    val wire = module("wire", Jvm, Native)(scala3)
      .clang(ClangMajor(21))
      .nativeVersion(nativeNightly)
      .dependsOn(api)
      .libraryDependencies(zio, zioJson)
      .testDependencies(zioTest, zioTestSbt)
    val heddle    = dep"rocks.earlyeffect::heddle:0.15.0"
    val heddleMcp = heddle.module("heddle-mcp")
    val central   = module("central", Jvm)(scala3)
      .dependsOn(api, wire)
      .libraryDependencies(zio, heddle)
      .testDependencies(zioTest, zioTestSbt, dep"rocks.earlyeffect::slayer:0.3.0")
    val scalaCompiler   = dep"org.scala-lang::scala3-compiler:3.10.1"
    val scaladoc        = scalaCompiler.module("scaladoc")
    val asm             = dep"org.ow2.asm:asm:9.11.1"
    val asmTree         = asm.module("asm-tree")
    val grpc            = dep"io.grpc:grpc-api:1.85.1"
    val grpcNetty       = grpc.module("grpc-netty-shaded")
    val grpcStub        = grpc.module("grpc-stub")
    val jacksonCore     = dep"com.fasterxml.jackson.core:jackson-core:2.22.4"
    val jacksonDatabind = jacksonCore.module("jackson-databind")
    val jackson3        = dep"tools.jackson.core:jackson-core:3.2.4"
    val jackson3Bind    = jackson3.module("jackson-databind")
    val splice          = module("splice", Jvm)(scala3)
      .artifact(artifact"histos-splice")
      .libraryDependencies(
        zio,
        dep"io.get-coursier::coursier-cache:2.1.26",
        dep"org.apache.commons:commons-compress:1.28.0",
        dep"com.google.javascript:closure-compiler:v20260726",
        dep"org.scala-js::scalajs-ir:1.23.0",
        dep"org.scala-js::scalajs-linker-interface:1.23.0",
      )
      .testDependencies(
        zioTest,
        zioTestSbt,
        dep"org.graalvm.polyglot:polyglot:25.2.4",
        dep"org.graalvm.js:js-language:25.2.4",
        dep"org.graalvm.truffle:truffle-runtime:25.2.4",
      )
    val engine = module("engine", Jvm)(scala3)
      .dependsOn(central, api, wire, splice)
      .libraryDependencies(
        zio,
        zioJson,
        zioStreams,
        scalaCompiler,
        scaladoc,
        dep"org.scala-js::scalajs-linker:1.23.0",
        dep"org.scala-js::scalajs-sbt-test-adapter:1.23.0",
        dep"org.scala-js::scalajs-env-nodejs:1.7.0",
        dep"rocks.earlyeffect::chekhov-jsenv:0.1.3",
        dep"org.scala-sbt::zinc:2.0.5",
        dep"org.scala-sbt::sbinary:0.6.0",
        dep"org.scala-lang.modules::scala-parser-combinators:2.5.0",
        dep"io.get-coursier::coursier:2.1.26",
        asm,
        asmTree,
        dep"org.bouncycastle:bcpg-jdk18on:1.86",
        dep"org.scalameta:scalafmt-interfaces:3.11.5",
        grpc,
        grpcNetty,
        dep"com.fasterxml.jackson.core:jackson-annotations:2.22",
        jacksonCore,
        jacksonDatabind,
        dep"com.fasterxml.jackson.datatype:jackson-datatype-jsr310:2.22.4",
        jackson3,
        jackson3Bind,
        dep"tools.jackson.dataformat:jackson-dataformat-yaml:3.2.4",
        dep"org.jsoup:jsoup:1.24.1",
        dep"com.google.guava:guava:33.8.0-jre",
        dep"com.typesafe::mima-core:1.1.5",
      )
      .testDependencies(
        zioTest,
        zioTestSbt,
        dep"rocks.earlyeffect::slayer:0.3.0",
        grpcStub,
      )
    val client = module("client", Native)(scala3)
      .clang(ClangMajor(21))
      .nativeVersion(nativeNightly)
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
        heddleMcp,
        dep"dev.zio::zio-cli:0.8.2",
        dep"dev.zio::zio-process:0.8.1",
      )
      .testDependencies(
        zioTest,
        zioTestSbt,
        dep"rocks.earlyeffect::slayer:0.3.0",
        asm,
        asmTree,
      )
    val library =
      Ship("histos").next(0, 5, 0)(api, wire, splice, central, engine, app).toolchain(launcher = app, client = client)
    val docs = module("docs", Jvm, Js)(scala3)
      .dependsOn(Jvm)(api, app)
      .note(PluginId("dev.earlyeffect.specular"), NoteKey("library"), shipNote(library.number.render))
      .main(MainClass("histos.docs.ClientMain"))
      .libraryDependencies(
        dep"rocks.earlyeffect::specular-core:0.21.4",
        dep"rocks.earlyeffect::specular-site:0.21.4".on(Jvm),
        dep"rocks.earlyeffect::early-effect-docs-theme:0.21.4".on(Jvm),
        dep"rocks.earlyeffect::mermoid-ascent:0.2.0".on(Jvm),
        dep"org.scala-lang::scala3-tasty-inspector:3.10.1".on(Jvm),
        zio,
      )
      .testDependencies(
        dep"rocks.earlyeffect::specular-zio-test:0.21.4".on(Jvm),
        zioTest.on(Jvm),
        zioTestSbt.on(Jvm),
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
          UpdatePolicy(author = PrAuthor.App(SecretName("HISTOS_APP_ID"), SecretName("HISTOS_APP_PRIVATE_KEY")))
        )
      )
      .coverageExcludes(CoverageExcludes(classlikes = List(".*Macro.*")))
      .scalafmt
      .docCheck
      .verify(VerifyTask(JobName("client"), VerifyRun.BuiltIn(TaskName("host")), ClientLink.Native))
      .resolvers(
        Resolver.Maven(RepositoryUrl("https://central.sonatype.com/repository/maven-snapshots/")),
        Resolver.Central,
      )
      .publishLocalSuite(
        ships(library),
        api,
        wire,
        splice,
        client,
        central,
        engine,
        app,
        pluginTestkit,
        docs,
      )

  override def services =
    BuildServices.default
      .add[Specular](ZLayer.succeed(site) >>> Specular.layer)
      .tasks[Specular](ZLayer.succeed(site) >>> Specular.layer)

  private val site =
    Site
      .forProject("docs", "histos.docs.BuildSite")
      .docsUrl("https://early-effect.github.io/histos/")
      .name("histos")
      .organization("rocks.earlyeffect")
      .libraryShip
      .title("Histos")
      .description("A Scala build tool. The build is a program. The work is an effect.")
      .homepage("https://github.com/early-effect/histos")
      .linkClient
      .preview
      .ci(SiteCi.Publish(SitePublish.Automatic))

  /** A ship revision is one line. `not-a-ship` is the branch a revision cannot take. */
  private def shipNote(revision: String): NoteText =
    NoteText.from(revision) match
      case Right(text) => text
      case Left(_)     => NoteText("not-a-ship")
end build
