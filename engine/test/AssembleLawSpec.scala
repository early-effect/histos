package histos.assembly

import histos.cache.ContentStore
import histos.jdk.{JdkMajor, JdkRuntime, JdkVersion, Machine, Sha256Digest}
import histos.native.NativeOrigin
import histos.resolve.{ResolutionService, Resolve}
import histos.toolchain.RuntimeIdentity
import histos.version.{Catalog, CommitSha, Ship, ShipName, ShipNumber, ShipToolchain}
import histos.workspace.Workspace
import histos.zinc.{GraphCompile, Sources}
import histos.{
  CompilerClosure,
  MainClass,
  Module,
  Platform,
  Platforms,
  ProjectDep,
  ProjectGraph,
  ProjectName,
  Publication,
  Resolution,
  ScalaVersion,
  ScalaVersions,
  SnapshotId,
  SourcePos,
  Splits,
  ToolchainIdentity,
}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import slayer.stubbed

import zio.json.*
import zio.{Chunk, Scope, UIO, ZIO, ZLayer}
import zio.test.{TestAspect, ZIOSpecDefault, assertTrue, suite, test}

/** A launcher jar holds that module, the classes of the ship's library members, and the toolchain identity when the
  * checkout names a commit.
  */
object AssembleLawSpec extends ZIOSpecDefault:
  private val api  = ProjectName.trusted("api")
  private val app  = ProjectName.trusted("app")
  private val main = MainClass.trusted("histos.cli.Main")

  def spec = suite("assemble laws")(
    test("a missing module, a library, and a launcher whose library member is absent are refused before a compiler"):
      val jdk = JdkRuntime(
        Path.of("jdk"),
        JdkVersion.trusted("21", JdkMajor.trusted(21)),
        RuntimeIdentity.Local(Path.of("jdk"), Sha256Digest.of(Chunk.empty)),
      )
      ZIO.serviceWithZIO[CompilerClosure] { closure =>
        val missing =
          Assemble
            .run(Path.of("missing"), empty, jdk, NativeOrigin.Central, app, Path.of("out.jar"), _ => ZIO.unit)
            .either
        val library =
          Assemble
            .run(
              Path.of("missing"),
              appOnly(closure.scalaVersion, Publication.Library),
              jdk,
              NativeOrigin.Central,
              app,
              Path.of("out.jar"),
              _ => ZIO.unit,
            )
            .either
        val absent =
          Assemble
            .run(
              Path.of("missing"),
              launcherMissingApi(closure.scalaVersion),
              jdk,
              NativeOrigin.Central,
              app,
              Path.of("out.jar"),
              _ => ZIO.unit,
            )
            .either
        (for
          gone   <- missing
          bare   <- library
          member <- absent
        yield assertTrue(
          gone == Left(AssembleError.NoModule("app")),
          bare == Left(AssembleError.NotAssembly(app)),
          member == Left(AssembleError.NoModule("api")),
        )).provide(stubbed[Resolve] ++ stubbed[ContentStore] ++ stubbed[ResolutionService])
      }
    ,
    test("app and api compile into one jar, with the library jar and the toolchain identity"):
      ZIO.scoped {
        Workspace.work("histos-assemble").flatMap { repo =>
          Machine.load(repo).flatMap { machine =>
            GraphCompile.jdk(machine).flatMap { jdk =>
              ZIO.serviceWithZIO[CompilerClosure] { closure =>
                val scala3      = closure.scalaVersion
                val destination = repo.resolve("dist").resolve("histos.jar")
                val stamp       = Sources.unmanagedRoot(repo, "wire").resolve("histos").resolve("Toolchain.scala")
                val run         =
                  for
                    _ <- Workspace
                      .writeString(
                        repo.resolve("api").resolve("src").resolve("main").resolve("scala").resolve("Api.scala"),
                        "object ApiLib:\n  def ready: Int = 1\n",
                      )
                      .catchAll(error => ZIO.dieMessage(error.message))
                    _ <- Workspace
                      .writeString(
                        repo
                          .resolve("app")
                          .resolve("src")
                          .resolve("main")
                          .resolve("scala")
                          .resolve("histos")
                          .resolve("cli")
                          .resolve("Main.scala"),
                        "package histos.cli\nobject Main:\n  def main(args: Array[String]): Unit = ()\n",
                      )
                      .catchAll(error => ZIO.dieMessage(error.message))
                    _         <- Workspace.writeString(repo.resolve(".gitignore"), "target/\ndist/\n.histos/\nstore/\n")
                    _         <- Workspace.writeString(repo.resolve("histos.scala"), shipText)
                    _         <- git(repo, "init")
                    _         <- git(repo, "config", "user.email", "law@histos.test")
                    _         <- git(repo, "config", "user.name", "Law")
                    _         <- git(repo, "add", ".")
                    _         <- git(repo, "commit", "--no-gpg-sign", "-m", "first")
                    head      <- git(repo, "rev-parse", "HEAD")
                    assembled <- Assemble
                      .run(
                        repo,
                        graph(scala3, List(ProjectDep(api, None))),
                        jdk,
                        machine.native,
                        app,
                        destination,
                        _ => ZIO.unit,
                      )
                      .either
                    packed    <- ZIO.attemptBlocking(readJar(destination)).orDie
                    ident     <- ZIO.attemptBlocking(readEntry(destination, ToolchainIdentity.entry)).orDie
                    stampGone <- ZIO.attempt(Files.exists(stamp)).orDie
                  yield identityOf(head, ident) match
                    case None =>
                      assertTrue(false)
                    case Some(found) =>
                      assertTrue(
                        assembled.map(jar => jar.path == destination && jar.entries > 0) == Right(true),
                        packed.contains("ApiLib.class"),
                        packed.contains("histos/cli/Main.class"),
                        packed.contains(Assemble.ApiEntry),
                        packed.contains(ToolchainIdentity.entry),
                        found.coordinate == SnapshotId.render(ShipNumber.trusted(0, 3, 0), foundSha(head)),
                        found.commit == foundSha(head).value,
                        !found.dirty,
                        !stampGone,
                      )
                run.provideSomeLayer[Resolve](
                  ResolutionService.pinned(closure.pinning(api, app)) ++
                    ZLayer.succeed(ContentStore.in(repo.resolve("store")))
                )
              }
            }
          }
        }
      }
    ,
    test("an app over an older api packs the classes of api's own cell"):
      ZIO.scoped {
        Workspace.work("histos-assemble-older").flatMap { repo =>
          Machine.load(repo).flatMap { machine =>
            GraphCompile.jdk(machine).flatMap { jdk =>
              ZIO.serviceWithZIO[CompilerClosure] { closure =>
                val newest      = closure.scalaVersion
                val destination = repo.resolve("dist").resolve("histos.jar")
                val run         =
                  for
                    older <- closure.previous
                    _     <- Workspace.writeString(
                      repo.resolve("api").resolve("src").resolve("main").resolve("scala").resolve("Api.scala"),
                      "package histos.cli\nobject ApiLib:\n  def ready: Int = 1\n",
                    )
                    _ <- Workspace.writeString(
                      repo.resolve("app").resolve("src").resolve("main").resolve("scala").resolve("Main.scala"),
                      "package histos.cli\nobject Main:\n  def main(args: Array[String]): Unit = println(ApiLib.ready)\n",
                    )
                    assembled <- Assemble
                      .run(
                        repo,
                        graphAt(older, newest),
                        jdk,
                        machine.native,
                        app,
                        destination,
                        _ => ZIO.unit,
                      )
                      .either
                      .provideSomeLayer[Resolve](
                        ResolutionService.pinned(closure.pinningAt(api -> older, app -> newest)) ++
                          ZLayer.succeed(ContentStore.in(repo.resolve("store")))
                      )
                    packed <- ZIO.attemptBlocking(readJar(destination)).orDie
                  yield assertTrue(
                    assembled.map(_.path) == Right(destination),
                    packed.contains("histos/cli/ApiLib.class"),
                    packed.contains("histos/cli/Main.class"),
                    packed.contains(Assemble.ApiEntry),
                  )
                run
              }
            }
          }
        }
      },
  ).provideSomeLayerShared[zio.test.TestEnvironment & Scope](CompilerClosure.layer) @@ TestAspect.withLiveSystem

  private val shipText: String =
    """Ship("histos").next(0, 3, 0)(api, app).toolchain(launcher = app, client = client)
      |""".stripMargin

  /** `api` at `older`, and `app` at `newest` over it. */
  private def graphAt(older: ScalaVersion, newest: ScalaVersion): ProjectGraph =
    val apiModule =
      Module(api, Platforms(Platform.Jvm, Nil), ScalaVersions(older, Nil), Nil, Nil, SourcePos("api.scala", 1, 1))
    ProjectGraph.trusted(
      List(apiModule, appModule(newest, List(ProjectDep(api, None)))),
      Splits.aggregate,
      ship,
      None,
      Nil,
      resolution = Resolution.Lock,
    )

  private def empty: ProjectGraph =
    ProjectGraph.trusted(Nil, Splits.aggregate, Catalog.empty, None, Nil, resolution = Resolution.Lock)

  private def appOnly(scala3: ScalaVersion, publication: Publication): ProjectGraph =
    ProjectGraph.trusted(
      List(appModule(scala3, Nil, publication)),
      Splits.aggregate,
      Catalog.empty,
      None,
      Nil,
      resolution = Resolution.Lock,
    )

  private def launcherMissingApi(scala3: ScalaVersion): ProjectGraph =
    ProjectGraph.trusted(
      List(appModule(scala3, Nil)),
      Splits.aggregate,
      ship,
      None,
      Nil,
      resolution = Resolution.Lock,
    )

  private def appModule(
      scala3: ScalaVersion,
      dependsOn: List[ProjectDep],
      publication: Publication = Publication.Assembly(main),
  ): Module =
    Module(
      app,
      Platforms(Platform.Jvm, Nil),
      ScalaVersions(scala3, Nil),
      Nil,
      dependsOn,
      SourcePos("app.scala", 1, 1),
      publication = publication,
    )

  private def ship: Catalog =
    Catalog.Lines(
      ::(
        Ship.Line(
          ShipName.trusted("histos"),
          ShipNumber.trusted(0, 3, 0),
          ::(api, ::(app, Nil)),
          linked = Some(ShipToolchain(app, ProjectName.trusted("client"))),
        ),
        Nil,
      )
    )

  private def graph(scala3: ScalaVersion, dependsOn: List[ProjectDep]): ProjectGraph =
    val apiModule = Module(
      api,
      Platforms(Platform.Jvm, Nil),
      ScalaVersions(scala3, Nil),
      Nil,
      Nil,
      SourcePos("api.scala", 1, 1),
    )
    ProjectGraph.trusted(
      List(apiModule, appModule(scala3, dependsOn)),
      Splits.aggregate,
      ship,
      None,
      Nil,
      resolution = Resolution.Lock,
    )

  private def identityOf(head: String, text: Option[String]): Option[ToolchainIdentity] =
    text.flatMap: raw =>
      raw.fromJson[ToolchainIdentity] match
        case Right(found) => Some(found)
        case Left(_)      => None

  /** `head` is a commit this law just created. A parse failure is the empty sha, and the assertion fails. */
  private def foundSha(head: String): CommitSha =
    CommitSha.from(head) match
      case Right(sha) => sha
      case Left(_)    => CommitSha.trusted("0000000000000000000000000000000000000000")

  private def readJar(path: Path): Set[String] =
    if !Files.isRegularFile(path) then Set.empty
    else
      val zip = new java.util.zip.ZipFile(path.toFile)
      try zip.entries().asScala.map(_.getName).toSet
      finally zip.close()

  private def readEntry(path: Path, name: String): Option[String] =
    if !Files.isRegularFile(path) then None
    else
      val zip = new java.util.zip.ZipFile(path.toFile)
      try
        Option(zip.getEntry(name)).map: entry =>
          val stream = zip.getInputStream(entry)
          try String(stream.readAllBytes, StandardCharsets.UTF_8)
          finally stream.close()
      finally zip.close()

  private def git(repo: Path, args: String*): UIO[String] =
    ZIO
      .attemptBlocking:
        val command = List("git", "-C", repo.toString) ++ args.toList
        val process = new ProcessBuilder(command*)
          .redirectError(ProcessBuilder.Redirect.PIPE)
          .start()
        val out  = String(process.getInputStream.readAllBytes, StandardCharsets.UTF_8)
        val err  = String(process.getErrorStream.readAllBytes, StandardCharsets.UTF_8)
        val code = process.waitFor()
        (code, out.trim, err.trim)
      .orDie
      .flatMap:
        case (0, out, _)    => ZIO.succeed(out)
        case (code, _, err) => ZIO.dieMessage(s"git ${args.mkString(" ")} ($code): $err")
end AssembleLawSpec
