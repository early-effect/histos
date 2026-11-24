package histos.load

import histos.cache.ContentStore
import histos.contracts.ContractFixtures
import histos.plugin.{
  ActivationManifest,
  ApiDigest,
  ArtifactEntry,
  ArtifactPath,
  Contribution,
  DeclaredTask,
  Digests,
  FlagName,
  HistosPlugin,
  NamedText,
  Need,
  ParamName,
  PluginArgs,
  PluginId,
  PluginLinked,
  PluginLoadError,
  PluginOrigin,
  PluginOutput,
  PluginRunError,
  PluginTaskId,
  RegisteredPlugin,
  RunRequest,
  Seal,
  SealedArtifactManifest,
  ServiceKey,
  SourceId,
  TaskName,
  VerifiedPluginManifest,
}
import histos.resolve.{DependencyLock, ResolutionService, Resolve, Repository}
import histos.schedule.{ActionCache, CacheError, Digest, RemoteCache}
import histos.task.{TaskError, TaskId}
import histos.version.Catalog
import histos.workspace.Workspace
import histos.zinc.{CompilerStarts, Compilers}
import histos.{
  Dep,
  Module,
  Platform,
  Platforms,
  ProjectGraph,
  ProjectName,
  ScalaVersion,
  ScalaVersions,
  SourcePos,
  Splits,
}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import zio.test.*
import zio.{IO, Ref, Scope, UIO, ZIO, ZLayer, durationInt}

/** Task cache identity. The loader's verified closure, not an empty artifact list, decides the output.
  */
object PluginTaskIdentitySpec extends ZIOSpecDefault:
  private val launcher: ApiDigest  = ApiDigest.trusted("ab" * 32)
  private val otherHost: ApiDigest = ApiDigest.trusted("cd" * 32)

  def spec = suite("plugin task identity")(
    test("a verified loader closure changes the key and the output") {
      hosted { root =>
        for
          api    <- apiJar(root.resolve("api.jar"))
          cache  <- memory
          _      <- write(root.resolve("histos.plugins.scala"), catalog)
          _      <- write(root.resolve("echo/src/demo/Echo.scala"), echo("alpha"))
          _      <- write(root.resolve("histos.scala"), main)
          first  <- run(root, api, cache)
          _      <- write(root.resolve("echo/src/demo/Echo.scala"), echo("beta"))
          second <- run(root, api, cache)
        yield (first, second)
      }.map { ran =>
        val (first, second) = ran
        assertTrue(
          first.token == "alpha",
          second.token == "beta",
          first.sealedBytes.nonEmpty,
          second.sealedBytes.nonEmpty,
          first.sealedBytes != second.sealedBytes,
          first.entries.exists(_.endsWith("Echo.class")),
          second.entries.exists(_.endsWith("Echo.class")),
        )
      }
    },
    test("args, service identity, and host binding miss a stale output") {
      ZIO
        .scoped {
          for
            root    <- work("histos-p7-key")
            cache   <- memory
            kept    <- manifest("sealed-bytes", "echo", launcher)
            renamed <- manifest("sealed-bytes", "other", launcher)
            shifted <- manifest("sealed-bytes", "echo", otherHost)
            plain   = PluginArgs(List(NamedText(ParamName("label"), "one")), Set.empty)
            flagged = PluginArgs(plain.texts, Set(FlagName("loud")))
            binary  = PluginLinked.FromBinary(launcher)
            source  = PluginLinked.FromSource(otherHost, launcher)
            first   <- drive(root, LabelTask("body"), List(kept), binary, cache, plain)
            repeat  <- drive(root, LabelTask("body"), List(kept), binary, cache, plain)
            args    <- drive(root, LabelTask("body"), List(kept), binary, cache, flagged)
            service <- drive(root, LabelTask("body"), List(renamed), binary, cache, flagged)
            host    <- drive(root, LabelTask("body"), List(shifted), source, cache, flagged)
          yield assertTrue(
            first == "body[label=one][]",
            repeat == PluginRuntime.cacheHit(first),
            args == "body[label=one][loud]",
            service == "body[label=one][loud]",
            host == "body[label=one][loud]",
          )
        }
        .provideSomeLayer[Scope](driveLayer)
    },
    test("an unverified closure is not an empty artifact list") {
      ZIO
        .scoped {
          for
            root  <- work("histos-p7-empty")
            cache <- memory
            empty <- emptyClosure
            kept  <- manifest("real-bytes", "echo", launcher)
            binding = PluginLinked.FromBinary(launcher)
            filled  <- drive(root, LabelTask("sealed"), List(kept), binding, cache, PluginArgs.empty)
            vacant  <- drive(root, LabelTask("empty"), List(empty), binding, cache, PluginArgs.empty)
            refused <- drive(
              root,
              LabelTask("empty"),
              List(ProvenanceResult.Unverified(SourceId("echo"))),
              binding,
              cache,
              PluginArgs.empty,
            ).either
          yield assertTrue(
            filled == "sealed[][]",
            vacant == "empty[][]",
            refused == Left(PluginFault.Task(PluginRunError.Unverified(SourceId("echo")))),
          )
        }
        .provideSomeLayer[Scope](driveLayer)
    },
    test("mixed provenance does not return a stale task result") {
      ZIO
        .scoped {
          for
            root  <- work("histos-p7-mixed")
            cache <- memory
            kept  <- manifest("real-bytes", "echo", launcher)
            binding = PluginLinked.FromBinary(launcher)
            filled <- drive(root, LabelTask("sealed"), List(kept), binding, cache, PluginArgs.empty)
            mixed  <- drive(
              root,
              LabelTask("other"),
              List(kept, ProvenanceResult.Unverified(SourceId("echo"))),
              binding,
              cache,
              PluginArgs.empty,
            ).either
          yield assertTrue(
            filled == "sealed[][]",
            mixed == Left(PluginFault.Task(PluginRunError.Unverified(SourceId("echo")))),
          )
        }
        .provideSomeLayer[Scope](driveLayer)
    },
    test("compile and scheduler failures stay distinct through bind") {
      ZIO
        .scoped {
          for
            root      <- work("histos-p7-fault")
            _         <- ZIO.attemptBlocking(Files.createDirectories(root.resolve("store"))).orDie
            _         <- hostFacts
            compiled  <- compileFault(root)
            scheduled <- schedulerFault(root)
          yield assertTrue(compiled == "compiled", scheduled == "scheduled")
        }
        .provideSomeLayer[Scope](driveLayer)
    },
  ) @@ TestAspect.withLiveClock @@ TestAspect.timeout(10.minutes)

  private final case class Ran(token: String, sealedBytes: String, entries: List[String])

  private def run(
      root: Path,
      api: Path,
      cache: ActionCache,
  ): ZIO[BuildCache.Env & PluginProducts & Scope, LoadError, Ran] =
    for
      loaded <- BootstrapLoad.prepare(silent, BootstrapLoad.Prepare(root, api))
      _      <- hostFacts
      output <- loaded.plugins
        .run(PluginTaskId(PluginId("echo"), TaskName("show")), PluginArgs.empty)
        .provideSome[ContentStore & RemoteCache](
          ZLayer.succeed(silent),
          ResolutionService.pinned(DependencyLock(Nil)),
          ZLayer.succeed(cache),
        )
        .mapError(fault => LoadError.Plugin(PluginLoadError.Construction(fault.message)))
    yield Ran(output.text, sealedBytes(loaded), entries(loaded))

  private def sealedBytes(loaded: LoadedBuild): String =
    loaded.provenance match
      case ProvenanceResult.Verified(manifest) :: Nil =>
        manifest.artifacts.map(_.identity.digest.value).mkString(",")
      case _ => ""

  private def entries(loaded: LoadedBuild): List[String] =
    loaded.provenance match
      case ProvenanceResult.Verified(manifest) :: Nil =>
        manifest.artifacts.flatMap(_.entries.map(_.path.value))
      case _ => Nil

  private def drive(
      repo: Path,
      plugin: HistosPlugin,
      provenance: List[ProvenanceResult],
      binding: PluginLinked,
      cache: ActionCache,
      args: PluginArgs,
  ): ZIO[Base & Scope, PluginFault, String] =
    open(repo, plugin, graph(plugin), provenance, binding).flatMap: runtime =>
      runtime
        .run(PluginTaskId(plugin.id, TaskName("show")), args)
        .provideSomeLayer[Base](ZLayer.succeed(cache))
        .map(_.text)

  private def compileFault(repo: Path): ZIO[Base & Scope, Nothing, String] =
    val plugin = CompileTask()
    for
      runtime <- open(repo, plugin, compileGraph(plugin), Nil, PluginLinked.FromBinary(launcher))
      exit    <- fault(runtime, plugin, ActionCache.miss)
    yield exit

  private def schedulerFault(repo: Path): ZIO[Base & Scope, Nothing, String] =
    val plugin = LabelTask("body")
    for
      proven  <- manifest("body", "echo", launcher)
      runtime <- open(repo, plugin, graph(plugin), List(proven), PluginLinked.FromBinary(launcher))
      exit    <- fault(runtime, plugin, broken)
    yield exit

  private def fault(runtime: PluginRuntime, plugin: HistosPlugin, cache: ActionCache): ZIO[Base, Nothing, String] =
    val ran = for
      bound  <- runtime.bind.provideSomeLayer[Base](ZLayer.succeed(cache))
      id     <- ZIO.fromEither(TaskId.from(PluginTaskId(plugin.id, TaskName("show")).render))
      effect <- bound.get(id) match
        case Some(found) => ZIO.succeed(found)
        case None        => ZIO.fail(TaskError.NoRunner(id))
      exit <- effect.either
    yield exit
    ran.foldZIO(
      error => ZIO.succeed(kind(error)),
      exit =>
        ZIO.succeed:
          exit match
            case Left(error) => kind(error)
            case Right(_)    => "success",
    )

  private def emptyClosure: UIO[ProvenanceResult] =
    ContractFixtures.verifiedEmpty match
      case Right(result) => ZIO.succeed(result)
      case Left(error)   => ZIO.dieMessage(error.message)

  private def open(
      repo: Path,
      plugin: HistosPlugin,
      built: ProjectGraph,
      provenance: List[ProvenanceResult],
      binding: PluginLinked,
  ): ZIO[Base & Scope, Nothing, PluginRuntime] =
    for
      _       <- hostFacts
      leases  <- GenerationLeases.make
      threads <- PluginThreads.make(getClass.getClassLoader)
      runtime <- PluginRuntime.loaded(
        repo,
        built,
        getClass.getClassLoader,
        threads,
        List(PluginLink(plugin.id, binding, Nil)),
        launcher,
        leases,
        provenance,
      )
    yield runtime

  private def graph(plugin: HistosPlugin): ProjectGraph =
    ProjectGraph.trusted(
      Nil,
      Splits.aggregate,
      Catalog.empty,
      None,
      Nil,
      plugins = List(RegisteredPlugin(plugin.id, plugin.contribute)(plugin)),
    )

  private def compileGraph(plugin: HistosPlugin): ProjectGraph =
    val version = ScalaVersion.trusted("3.10.1")
    val module  = Module(
      ProjectName("app"),
      Platforms(Platform.Jvm, Nil),
      ScalaVersions(version, Nil),
      Nil,
      Nil,
      SourcePos("histos.scala", 1, 1),
    )
    ProjectGraph.trusted(
      List(module),
      Splits.aggregate,
      Catalog.empty,
      None,
      Nil,
      plugins = List(RegisteredPlugin(plugin.id, plugin.contribute)(plugin)),
    )

  private def manifest(bytes: String, service: String, host: ApiDigest): UIO[ProvenanceResult] =
    val raw   = bytes.getBytes(StandardCharsets.UTF_8)
    val hash  = Digests.content(raw)
    val built = SealedArtifactManifest.from(
      List(ArtifactEntry(ArtifactPath("demo/Echo.class"), raw.length.toLong, hash))
    )
    built match
      case Left(error)  => ZIO.dieMessage(error.message)
      case Right(found) =>
        ServiceKey.from(service) match
          case Left(error) => ZIO.dieMessage(error.message)
          case Right(key)  =>
            val identity = found.identity
            ZIO.succeed(
              ProvenanceResult.Verified(
                VerifiedPluginManifest(
                  ActivationManifest(
                    SourceId("echo"),
                    key,
                    PluginOrigin.Source(SourceId("echo"), hash),
                    identity,
                    List(identity),
                    host,
                    hash,
                  ),
                  List(found),
                )
              )
            )

  private def kind(error: TaskError): String =
    error match
      case TaskError.Fault(PluginFault.Compiled(_))  => "compiled"
      case TaskError.Fault(PluginFault.Scheduled(_)) => "scheduled"
      case TaskError.Fault(other)                    => s"fault ${other.message}"
      case TaskError.Plugin(found)                   => s"plugin ${found.message}"
      case other                                     => other.message

  private val broken: ActionCache = new ActionCache:
    def get(digest: Digest): IO[CacheError, Option[String]]      = ZIO.fail(CacheError.Corrupt(digest))
    def put(digest: Digest, value: String): IO[CacheError, Unit] = ZIO.unit

  private type Base = RemoteCache & Resolve & ContentStore & ResolutionService & PluginProducts

  private val driveLayer: ZLayer[Any, Nothing, Base] =
    ZLayer.make[Base](
      RemoteCache.off,
      PluginProducts.live,
      ZLayer.succeed(silent),
      ResolutionService.pinned(DependencyLock(Nil)),
      ZLayer.scoped(work("histos-p7-store").map(root => ContentStore.in(root))),
    )

  private val catalog: String =
    """|import histos.*
       |import histos.plugin.*
       |
       |object plugins extends HistosPlugins:
       |  val echo = module("echo", Jvm)(scala"3.10.1").in(ProjectPath("echo"))
       |  def sources = PluginCatalog(PluginSources.module(echo))
       |""".stripMargin

  private val main: String =
    """|import demo.Echo
       |import histos.*
       |import histos.plugin.*
       |import zio.ZLayer
       |
       |final class EchoTask extends HistosPlugin:
       |  val id = PluginId("echo")
       |  def contribute =
       |    Contribution(
       |      List(DeclaredTask(TaskName("show"), Seal.Hermetic, Set.empty[Need], Nil, Nil, Nil)),
       |      Nil,
       |      Nil,
       |    )
       |  def run(request: RunRequest) =
       |    zio.ZIO.succeed(PluginOutput(Echo.token, Nil))
       |
       |object EchoTask:
       |  val layer: ZLayer[Any, Nothing, EchoTask] = ZLayer.succeed(new EchoTask)
       |
       |object build extends HistosBuild:
       |  def graph =
       |    val app = module("app", Jvm)(scala"3.10.1")
       |    Projects(app)
       |
       |  override def services = BuildServices.default.tasks[EchoTask](EchoTask.layer)
       |""".stripMargin

  private def echo(token: String): String =
    s"""|package demo
        |
        |object Echo:
        |  def token: String = "$token"
        |""".stripMargin

  private final class LabelTask(label: String) extends HistosPlugin:
    val id: PluginId             = PluginId("echo")
    def contribute: Contribution =
      Contribution(
        List(DeclaredTask(TaskName("show"), Seal.Hermetic, Set.empty[Need], Nil, Nil, Nil)),
        Nil,
        Nil,
      )
    def run(request: RunRequest): zio.ZIO[zio.Scope & histos.plugin.PluginScopes, PluginRunError, PluginOutput] =
      val texts = request.args.texts.map(item => s"${item.name.value}=${item.value}").sorted.mkString(",")
      val flags = request.args.flags.toList.map(_.value).sorted.mkString(",")
      ZIO.succeed(PluginOutput(s"$label[$texts][$flags]", Nil))

  private final class CompileTask extends HistosPlugin:
    val id: PluginId             = PluginId("echo")
    def contribute: Contribution =
      Contribution(
        List(
          DeclaredTask(
            TaskName("show"),
            Seal.Hermetic,
            Set.empty[Need],
            Nil,
            Nil,
            List(histos.plugin.TaskDep.Compile(ProjectName("app"), None, None)),
          )
        ),
        Nil,
        Nil,
      )
    def run(request: RunRequest): zio.ZIO[zio.Scope & histos.plugin.PluginScopes, PluginRunError, PluginOutput] =
      ZIO.succeed(PluginOutput("compiled", Nil))

  private def hosted[A](
      body: Path => ZIO[BuildCache.Env & PluginProducts & Scope, LoadError, A]
  ): ZIO[Any, LoadError, A] =
    ZIO.scoped {
      for
        root  <- work("histos-p7-task")
        cas   <- ZIO.attemptBlocking(Files.createDirectories(root.resolve("cas"))).orDie
        value <- body(root).provideSomeLayer[Scope](
          ZLayer.succeed(ContentStore.in(cas)) ++
            CompilerStarts.live ++
            Compilers.live ++
            RemoteCache.off ++
            PluginFlights.live ++ PluginProducts.live
        )
      yield value
    }

  private def memory: UIO[ActionCache] =
    Ref.make(Map.empty[String, String]).map { kept =>
      new ActionCache:
        def get(digest: Digest): IO[CacheError, Option[String]] =
          kept.get.map(_.get(digest.value))
        def put(digest: Digest, value: String): IO[CacheError, Unit] =
          kept.update(_.updated(digest.value, value))
    }

  private def work(name: String): ZIO[Scope, Nothing, Path] =
    Workspace.work(name).foldZIO(failure => ZIO.dieMessage(failure.message), ZIO.succeed)

  private def write(path: Path, text: String): UIO[Path] =
    ZIO.attemptBlocking {
      Option(path.getParent).foreach(parent => Files.createDirectories(parent))
      Files.writeString(path, text, StandardCharsets.UTF_8)
      path
    }.orDie

  private def apiJar(destination: Path): UIO[Path] =
    ZIO.attemptBlocking {
      val located = Path.of(classOf[histos.HistosBuild].getProtectionDomain.getCodeSource.getLocation.toURI)
      if Files.isRegularFile(located) then located
      else
        Files.createDirectories(destination.getParent)
        val out  = new java.util.zip.ZipOutputStream(Files.newOutputStream(destination))
        val walk = Files.walk(located)
        try
          walk.iterator.asScala.foreach { path =>
            if Files.isRegularFile(path) then
              val name = located.relativize(path).toString.replace('\\', '/')
              out.putNextEntry(new java.util.zip.ZipEntry(name))
              out.write(Files.readAllBytes(path))
              out.closeEntry()
          }
        finally
          walk.close()
          out.close()
        destination
    }.orDie

  private val silent: Resolve =
    new Resolve:
      def resolveTool(request: histos.resolve.ToolRequest)                          = ZIO.dieMessage("resolveTool")
      def resolve(cell: histos.resolve.ResolveCell)                                 = ZIO.dieMessage("resolve")
      def fetch(artifact: histos.resolve.LockedArtifact)                            = ZIO.dieMessage("fetch")
      def resolvePlugin(dependency: Dep, repositories: histos.resolve.Repositories) =
        ZIO.dieMessage("resolvePlugin")
      def sources(organization: String, name: String, revision: String, repository: Repository) =
        ZIO.dieMessage("sources")

  private def hostFacts: UIO[Unit] =
    ZIO.foreachDiscard(List("user.home", "os.name", "os.arch", "java.home")): key =>
      Option(System.getProperty(key)) match
        case Some(value) if value.nonEmpty => TestSystem.putProperty(key, value)
        case _                             => ZIO.dieMessage(s"the test JVM has no $key")
end PluginTaskIdentitySpec
