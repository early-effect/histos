package histos.plugin

import histos.cache.ContentStore
import histos.central.{Artifact, Layout, Published, Publisher, Revision}
import histos.cli.ConsentDecision
import histos.client.CallerFacts
import histos.jdk.{Jdk, JdkError, Sha256Digest}
import histos.load.{BootstrapLoad, BuildCache, BuildClasspath, Loader, PluginFault, PluginRuntime}
import histos.mcp.McpConsent
import histos.publish.{EntryBytes, PluginBundle, PluginPublishFault, PluginPublishRequest, SourceBytes, StagedPlugin}
import histos.lock.{
  CallIdentity,
  ExecutionKind,
  InvocationEnvelope,
  LockDiff,
  LockOffer,
  LockReply,
  LockRevisionState,
  NecessaryScope,
}
import histos.pack.{AssemblyFile, ClassJar, PackageKey}
import histos.report.WireJson.given
import histos.resolve.{
  Classpath,
  ClasspathScope,
  DependencyLock,
  Repositories,
  ResolutionService,
  Resolve,
  ResolvedArtifact,
  ResolveError,
  ToolRequest,
}
import histos.schedule.{ActionCache, CacheError, Digest, RemoteCache, ScheduleError}
import histos.server.{Call, ClientFrames, InvocationDesk, RunBytes, RunInput}
import histos.task.{TaskError, TaskId}
import histos.toolchain.{HostCompilerError, HostCompilerInputs, HostCompilerManifest, HostCompilerSession, HostToolRole}
import histos.version.{ReleaseVersion, ShipNumber}
import histos.workspace.{IoFailure, IoFault, Workspace}
import histos.zinc.{
  CompilationCapture,
  CompilationSnapshot,
  CompileInput,
  CompileInputs,
  CompileRow,
  CompilerRequests,
  CompilerStarts,
  Compilers,
  ProductDigest,
  Sources,
}
import histos.{
  Dep,
  HistosBuild,
  Organization,
  Platform,
  PluginPublication,
  ProjectName,
  Resolution,
  ScalaVersion,
  ScalacFlag,
}

import histos.suite.HistosSpec

import java.net.URLClassLoader
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, StandardCopyOption}
import java.time.Instant
import java.util.zip.{ZipEntry, ZipFile, ZipOutputStream}

import scala.compiletime.testing.typeChecks
import scala.jdk.CollectionConverters.*

import org.objectweb.asm.Opcodes
import org.objectweb.asm.tree.ClassNode

import zio.json.*
import zio.test.*
import zio.{Chunk, IO, Ref, Scope, ZIO, ZLayer, durationInt}

/** Sentences the pinned host has to execute. Each test names the row it closes. */
object InstalledHostProofSpec extends HistosSpec:
  private val Scala3101 = ScalaVersion("3.10.1")
  private val utf8      = StandardCharsets.UTF_8

  def spec = suite("installed host proof")(
    test("CompileInputs.remember stamps the analysis with the test clock") {
      val when = Instant.parse("2024-06-01T00:00:00Z")
      ZIO.scoped {
        for
          root <- work("histos-m6-clock")
          _    <- ZIO.attemptBlocking(Files.createDirectories(root.resolve("cas"))).orDie
          store = ContentStore.in(root.resolve("cas"))
          _ <- TestClock.setTime(when)
          analysis = root.resolve("analysis")
          remembered <- CompileInputs
            .remember(store, analysis, List(CompileRow(CompileInput.Sources, "abc")))
            .either
          listed <- store.list.either
        yield
          val slot    = CompileInputs.slot(analysis)
          val stamped = listed match
            case Right(entries) =>
              entries.exists(entry => entry.digest == slot && entry.lastUsed == when)
            case Left(_) => false
          assertTrue(remembered == Right(()), stamped)
      }
    },
    test("stamp bytes change the package key and leave the product digest") {
      ZIO.scoped {
        for
          root <- work("histos-m6-stamp")
          bytes  = classBytes
          tasty  = Array[Byte](4, 4, 4)
          stampA = "stamp-a\n".getBytes(utf8)
          stampB = "stamp-b\n".getBytes(utf8)
          left   = root.resolve("left")
          right  = root.resolve("right")
          _ <- ZIO.attemptBlocking {
            writeTree(left, bytes, tasty, Some(stampA))
            writeTree(right, bytes, tasty, Some(stampB))
          }.orDie
          digestA <- ZIO.fromEither(ProductDigest.ofDirectory(left, root)).mapError(digestFailed)
          digestB <- ZIO.fromEither(ProductDigest.ofDirectory(right, root)).mapError(digestFailed)
          entriesA = entriesOf(bytes, tasty, Some(stampA))
          entriesB = entriesOf(bytes, tasty, Some(stampB))
          sealedA   <- ZIO.fromEither(sealKey(entriesA)).mapError(publishFailed)
          sealedB   <- ZIO.fromEither(sealKey(entriesB)).mapError(publishFailed)
          manifestA <- ZIO.fromEither(manifestKey(entriesA)).mapError(publishFailed)
          manifestB <- ZIO.fromEither(manifestKey(entriesB)).mapError(publishFailed)
        yield assertTrue(digestA == digestB, sealedA != sealedB, manifestA != manifestB)
      }
    },
    test("a generated replacement from Sources.prefer is the published rebuild entry") {
      ZIO.scoped {
        for
          repo <- work("histos-m6-prefer")
          committed = "package demo\n\nobject Mark:\n  def token = \"committed-mark\"\n"
          generated = "package demo\n\nobject Mark:\n  def token = \"generated-mark\"\n"
          source    = repo.resolve("plug/src/main/scala/demo/Mark.scala")
          unmanaged = Sources.unmanagedRoot(repo, "plug").resolve("main/scala/demo/Mark.scala")
          _        <- write(source, committed)
          _        <- write(unmanaged, generated)
          selected <- PluginBundle
            .preferred(repo, repo.resolve("plug/src"), Sources.unmanagedRoot(repo, "plug"), Scala3101, Platform.Jvm)
            .mapError(publishFailed)
          current <- ZIO.foreach(selected) { input =>
            readBytes(repo.resolve(input.path.value)).map(bytes => SourceBytes(input.path, Chunk.fromArray(bytes)))
          }
          request <- ZIO.fromEither(publishRequest(selected, Nil, current)).mapError(publishFailed)
          packed  <- upload(request).mapError(publishFailed)
        yield
          val payload = packed.entries.collect {
            case (name, bytes) if name.contains("META-INF/histos/rebuild/") && name.endsWith("Mark.scala") =>
              new String(bytes, utf8)
          }
          assertTrue(
            selected.exists(_.provenance == SourceProvenance.Generated),
            payload == List(generated),
            !payload.exists(_.contains("committed-mark")),
            packed.uploads == 1,
          )
      }
    },
    test("a compile-only input is in the recipe and absent from the consumer POM") {
      val only    = Dep("com.example:only:1.2.3").compileOnly
      val kept    = Dep("com.example:kept:1.2.3")
      val digest  = Digests.content("compile-only-member".getBytes(utf8))
      val closure = List(ClosureArtifact(ArtifactIdentity(ArtifactFormat.Jar, digest), ClosureRole.CompileOnly))
      for
        dependencies <- ZIO.fromEither(PluginBundle.dependencies(List(only, kept), Scala3101, Platform.Jvm))
        request      <- ZIO.fromEither(publishRequest(Nil, closure, Nil, dependencies))
        staged       <- ZIO.fromEither(PluginBundle.stage(request))
        packed       <- upload(request)
      yield
        val names  = dependencies.map(_.name.value)
        val frozen = staged.recipe.toList.flatMap(_.nodes).flatMap { item =>
          item.sources match
            case InputRecord.Frozen(body) => body.closure
            case InputRecord.Unrecorded   => Nil
        }
        val pom  = Layout.pom(artifactOf(staged, dependencies))
        val json = packed.text(PluginBundle.Descriptor)
        assertTrue(
          names.contains("kept"),
          !names.contains("only"),
          frozen.exists(item => item.role == ClosureRole.CompileOnly && item.artifact.digest == digest),
          json.exists(text => text.contains(digest.value) && text.contains("\"role\":\"compile-only\"")),
          pom.contains("<artifactId>kept</artifactId>"),
          !pom.contains("<artifactId>only</artifactId>"),
          packed.uploads == 1,
        )
    },
    test("registration is the capability interface and an intersection key is refused") {
      ZIO.scoped {
        for
          count <- Ref.make(0)
          layer = ZLayer.fromZIO(
            count.update(_ + 1) *> ZIO.succeed[InstalledReview](InstalledReviewLive("iface"))
          )
          built <- activate(
            BuildServices.default.add[InstalledReview](layer).tasks[InstalledSeen](InstalledSeen.layer).activate
          )
          seen <- count.get
        yield
          val text = built.tasks match
            case (found: InstalledSeen) :: Nil => found.text
            case _                             => ""
          assertTrue(
            typeChecks(oneCapability),
            !typeChecks(intersectionKey),
            typeChecks(intersectionTag),
            seen == 1,
            text == "iface",
          )
      }
    },
    test("native and MCP are two callers on one generation and repair is not env or stdin") {
      val poison = "HISTOS_LOCK_REPLY"
      val stdin  = RunInput.Stdin(RunBytes.encode(Chunk.fromArray("y".getBytes(utf8))))
      val reply = LockReply.Accept(ProposalId.trusted(Digests.sha256("reply".getBytes(utf8))), LockRevisionState.Absent)
      val decline =
        LockReply.Decline(ProposalId.trusted(Digests.sha256("decline".getBytes(utf8))))
      for
        _     <- TestSystem.putEnv("CI", "true")
        _     <- TestSystem.putEnv(poison, "accept")
        quiet <- CallerFacts.native(Call.Compile(Nil), Map.empty, prompts = false)
        loud  <- CallerFacts.native(
          Call.Compile(Nil),
          Map("CI" -> "true", poison -> "accept"),
          prompts = false,
        )
        mcp      <- CallerFacts.mcp(Call.Compile(Nil))
        desk     <- InvocationDesk.make
        gate     <- zio.Promise.make[Nothing, Unit]
        arrived  <- Ref.make(0)
        nativeId <- caller(desk, gate, arrived, quiet.context, "native-only").fork
        mcpId    <- caller(desk, gate, arrived, mcp.context, "mcp-only").fork
        left     <- nativeId.join
        right    <- mcpId.join
        program  <- zio.Queue.unbounded[RunInput]
        consent  <- zio.Queue.unbounded[LockReply]
        _        <- deliver(stdin, program, consent)
        _        <- deliver(reply, program, consent)
        _        <- deliver(decline, program, consent)
        programs <- program.takeAll
        replies  <- consent.takeAll
        offer = LockOffer(
          ProposalId.trusted(Digests.sha256("offer".getBytes(utf8))),
          CallerFacts.mcp,
          LockRevisionState.Absent,
          Digests.content("declarations".getBytes(utf8)),
          Digests.content("candidate".getBytes(utf8)),
          NecessaryScope(PluginDetail("none"), LockDiff(Nil)),
          CallIdentity.identity(Call.Compile(Nil)),
        )
        decision = McpConsent.unnegotiated(offer, Call.Compile(Nil))
      yield
        val diagnosis = decision match
          case ConsentDecision.Diagnosis(_)         => true
          case ConsentDecision.Prompt               => false
          case ConsentDecision.Structured           => false
          case ConsentDecision.Required(_, _, _, _) => false
        val nativeTexts = messages(left.events)
        val mcpTexts    = messages(right.events)
        assertTrue(
          quiet.context.client == CallerFacts.native,
          quiet.context.execution == ExecutionKind.Local,
          quiet.lock.isEmpty,
          loud.context.client == CallerFacts.native,
          loud.context.execution == ExecutionKind.Ci,
          loud.lock.isEmpty,
          mcp.context.client == CallerFacts.mcp,
          mcp.lock.isEmpty,
          !quiet.toString.contains(poison),
          !loud.toString.contains("accept"),
          !mcp.toString.contains(poison),
          left.built == 1,
          right.built == 1,
          nativeTexts == List("native-only"),
          mcpTexts == List("mcp-only"),
          programs.toList == List(stdin),
          replies.toList == List(reply, decline),
          diagnosis,
        )
    },
    suite("live host")(
      test("old plugin bytes reuse a stale task output and new bytes change the key and the output") {
        hosted { (root, _) =>
          for
            api   <- apiJar(root.resolve("api.jar"))
            cache <- memory
            _     <- write(root.resolve("histos.plugins.scala"), echoCatalog)
            _     <- write(root.resolve("echo/src/demo/Echo.scala"), echo("alpha"))
            _     <- write(root.resolve("histos.scala"), echoMain)
            _     <- hostFacts
            first <- runTask(root, api, cache, PluginId("echo"))
            again <- runTask(root, api, cache, PluginId("echo"))
            _     <- write(root.resolve("echo/src/demo/Echo.scala"), echo("beta"))
            third <- runTask(root, api, cache, PluginId("echo"))
          yield assertTrue(
            first.text == "alpha",
            again.text == PluginRuntime.cacheHit("alpha"),
            first.sealedBytes.nonEmpty,
            again.sealedBytes == first.sealedBytes,
            third.text == "beta",
            third.sealedBytes != first.sealedBytes,
            third.text != PluginRuntime.cacheHit("alpha"),
          )
        }
      },
      test("a host-API byte change misses the compile cache") {
        hosted { (root, starts) =>
          for
            session <- sessionFor(root)
            jar     <- apiBody(root.resolve("api.jar"), "alpha-api")
            first   <- compileMark(root, session, "api", List(jar))
            _       <- apiBody(root.resolve("api.jar"), "beta-api")
            second  <- compileMark(root, session, "api", List(jar))
            count   <- starts.get
          yield assertTrue(count == 2, first.key != second.key)
        }
      },
      test("a helper-only stamp mismatch is BindingDrift and the root stamp matches") {
        hosted { (root, _) =>
          for
            api      <- apiJar(root.resolve("api.jar"))
            launcher <- digestOf(api)
            otherHex = if launcher == "ab" * 32 then "cd" * 32 else "ab" * 32
            other   <- ZIO.fromEither(ApiDigest.from(otherHex)).mapError(_ => LoadError.MissingFile(root))
            rootJar <- pluginJar(root.resolve("direct.jar"), launcher, "com/example/Root.class")
            helper  <- pluginJar(
              root.resolve("helper.jar"),
              other.value,
              "com/example/Helper.class",
              Some("""{"version":1,"mode":"rebuildable"}"""),
            )
            _    <- write(root.resolve("histos.plugins.scala"), oneShip("reviewed"))
            _    <- write(root.resolve("histos.scala"), plainMain)
            exit <- prepare(root, api, shipJars("reviewed", rootJar, helper)).either
          yield assertTrue(
            exit == Left(
              LoadError.Plugin(PluginLoadError.BindingDrift(helper, other, ApiDigest.trusted(launcher)))
            )
          )
        }
      },
      test("a local plugin compile sees its external jar") {
        hosted { (root, _) =>
          for
            api    <- apiJar(root.resolve("api.jar"))
            ext    <- externalJar(root.resolve("ext.jar"))
            _      <- write(root.resolve("histos.plugins.scala"), moduleCatalog)
            _      <- write(root.resolve("helper/src/demo/Use.scala"), useSource)
            _      <- write(root.resolve("histos.scala"), plainMain)
            loaded <- ext.compiled match
              case false => ZIO.succeed(None)
              case true  => prepare(root, api, extResolve(ext.path)).map(found => Some(found))
            text <- loaded match
              case None        => ZIO.succeed("")
              case Some(built) => token(built.build, "demo.Use$", "token")
          yield assertTrue(ext.compiled, ext.notes.isEmpty, text == "ext")
        }
      },
      test("settings and resources reach the compile and a second identical load starts the compiler zero times") {
        hosted { (root, starts) =>
          for
            files   <- packaged
            payload <- payloadJar(root, files, None, corrupt = false)
            api     <- apiJar(root.resolve("api.jar"))
            _       <- write(root.resolve("histos.plugins.scala"), settingsCatalog)
            _       <- write(root.resolve("helper/src/demo/Use.scala"), settingsSource)
            _       <- write(root.resolve("helper/src/main/resources/demo/note.txt"), "from-resource")
            _       <- write(root.resolve("histos.scala"), plainMain)
            first   <- prepare(root, api, silent, Some(payload))
            mid     <- starts.get
            report  <- token(first.build, "demo.Use$", "report")
            _       <- prepare(root, api, silent, Some(payload))
            again   <- starts.get
            _       <- write(root.resolve("helper/src/main/resources/demo/note.txt"), "changed-resource")
            third   <- prepare(root, api, silent, Some(payload))
            end     <- starts.get
            changed <- token(third.build, "demo.Use$", "report")
          yield assertTrue(
            report == "7|from-resource",
            mid > 0,
            again == mid,
            end > again,
            changed == "7|changed-resource",
          )
        }
      },
      test("theme and step are exported and an engine package is refused") {
        hosted { (root, _) =>
          for
            api    <- apiJar(root.resolve("api.jar"))
            _      <- write(root.resolve("histos.plugins.scala"), moduleCatalogBare)
            _      <- write(root.resolve("helper/src/demo/Probe.scala"), probeSource)
            _      <- write(root.resolve("histos.scala"), plainMain)
            loaded <- prepare(root, api, silent)
            text   <- token(loaded.build, "demo.Probe$", "token")
          yield assertTrue(text == "true:histos.step.StepKind|refused|refused")
        }
      },
      test("a compile lock failure and a scheduler failure stay distinct through bind") {
        hosted { (root, _) =>
          for
            compiled  <- faultKind(root.resolve("compile"), compileMain, ActionCache.miss)
            scheduled <- faultKind(root.resolve("schedule"), scheduleMain, brokenCache)
          yield
            val expectedCompile = Left(
              TaskError.Fault(
                PluginFault.Compiled(
                  histos.zinc.GraphCompileError.Lock(
                    ResolveError.LockDrift(ProjectName("app"), Scala3101, Platform.Jvm, ClasspathScope.Compile, Nil)
                  )
                )
              )
            )
            val scheduleSeen = scheduled match
              case Left(TaskError.Fault(PluginFault.Scheduled(ScheduleError.Cache(CacheError.Corrupt(_))))) => true
              case _                                                                                        => false
            assertTrue(compiled == expectedCompile, scheduleSeen, compiled != scheduled)
        }
      },
      test("a comment containing trait HistosBuild does not skip compilation") {
        hosted { (root, _) =>
          for
            api    <- apiJar(root.resolve("api.jar"))
            _      <- write(root.resolve("histos.plugins.scala"), moduleCatalogBare)
            _      <- write(root.resolve("helper/src/demo/Helper.scala"), commentSource)
            _      <- write(root.resolve("histos.scala"), plainMain)
            loaded <- prepare(root, api, silent)
            text   <- token(loaded.build, "demo.Helper$", "token")
            present = classFile(loaded.build, "demo/Helper.class")
          yield assertTrue(text == "compiled", present)
        }
      },
      test("a catalog intent is what main inherits and a conflicting main resolution is ResolutionConflict") {
        hosted { (root, _) =>
          for
            api       <- apiJar(root.resolve("api.jar"))
            _         <- write(root.resolve("histos.plugins.scala"), lockCatalog)
            _         <- write(root.resolve("histos.scala"), plainMain)
            inherited <- prepare(root, api, silent)
            _         <- write(root.resolve("histos.plugins.scala"), emptyCatalog)
            _         <- write(root.resolve("histos.scala"), lockedMain)
            conflict  <- prepare(root, api, silent).either
          yield
            val refused = conflict match
              case Left(LoadError.Bootstrap(BootstrapError.ResolutionConflict(Resolution.Solve, Resolution.Lock))) =>
                true
              case _ => false
            assertTrue(
              inherited.graph.resolution == Resolution.Lock,
              inherited.graph.intent == ResolutionIntent.Inherit,
              refused,
            )
        }
      },
      test("histos.strategies.scala runs after the core seed and before the remaining solve") {
        hosted { (root, starts) =>
          for
            api    <- apiJar(root.resolve("api.jar"))
            digest <- digestOf(api)
            seed   <- pluginJar(root.resolve("seed.jar"), digest, "marker.txt")
            rest   <- pluginJar(root.resolve("rest.jar"), digest, "marker.txt")
            log = root.resolve("order.txt")
            _     <- write(root.resolve("histos.plugins.scala"), orderedCatalog)
            _     <- write(root.resolve("histos.strategies.scala"), orderedStrategies(log))
            _     <- write(root.resolve("histos.scala"), plainMain)
            _     <- prepare(root, api, jarsOf(log, Map("seedplug" -> seed, "restplug" -> rest)), notice = append(log))
            text  <- ZIO.attemptBlocking(Files.readString(log, utf8)).orDie
            count <- starts.get
          yield assertTrue(
            count > 0,
            text == "seed\nplugin:seedplug\nstrategy\ndecide\nplugin:restplug\n",
          )
        }
      },
      test("a TASTy entry-byte change changes the seal and the consumer sees it") {
        hosted { (root, starts) =>
          for
            session <- sessionFor(root)
            left    <- helper(root, session, "aa")
            right   <- helper(root, session, "bb")
            useA    <- consumer(root, session, left, "aa")
            useB    <- consumer(root, session, right, "bb")
            before  <- starts.get
            flipped <- flipTasty(root.resolve("flipped"), left.classes)
            digestA <- ZIO.fromEither(ProductDigest.ofDirectory(left.classes, root)).mapError(digestFailed)
            digestF <- ZIO.fromEither(ProductDigest.ofDirectory(flipped, root)).mapError(digestFailed)
            sealA   <- sealDirectory(left.classes)
            sealF   <- sealDirectory(flipped)
            after   <- starts.get
          yield assertTrue(
            left.product.value != right.product.value,
            digestA == digestF,
            sealA != sealF,
            before == after,
            useA == "aa",
            useB == "bb",
          )
        }
      },
      test("the host compiler comes from the packaged manifest and a missing bridge does not solve") {
        hosted { (root, starts) =>
          for
            files       <- packaged
            whole       <- payloadJar(root.resolve("whole"), files, None, corrupt = false)
            distributed <- BuildClasspath.distribution(whole)
            store       <- ZIO.service[ContentStore]
            home        <- jdkHome
            session     <- HostCompilerInputs.materialize(whole, home, store).mapError(LoadError.HostCompiler(_))
            bridge      <- bridgeEntry(session)
            actual      <- jarEntryDigest(whole, s"bootstrap/compiler/${bridge.entry.relative.value}")
            gap         <- payloadJar(root.resolve("gap"), files, Some(HostToolRole.Bridge), corrupt = false)
            before      <- fixture(root)
            seen        <- Ref.make(0)
            api         <- apiJar(root.resolve("cache"))
            _           <- write(root.resolve("histos.plugins.scala"), bridgeCatalog)
            _           <- write(root.resolve("bridge/src/demo/Marker.java"), markerJava)
            _           <- write(root.resolve("bridge/src/demo/Bridge.scala"), bridgeScala)
            _           <- write(root.resolve("histos.scala"), plainMain)
            exit        <- prepare(root, api, counting(seen), Some(gap)).either
            calls       <- seen.get
            after       <- readBytes(root.resolve("dependencies.lock"))
            count       <- starts.get
            checkout    <- checkoutLock
          yield
            val missing = exit match
              case Left(LoadError.HostCompiler(HostCompilerError.EntryAbsent(HostToolRole.Bridge))) => true
              case _                                                                                => false
            assertTrue(
              distributed,
              session.manifest.scalaVersion == Scala3101,
              session.fingerprint == session.manifest.fingerprint,
              bridge.entry.digest == actual,
              session.tools.exists(_.entry.role == HostToolRole.Compiler),
              session.tools.exists(_.entry.role == HostToolRole.Bridge),
              session.tools.exists(_.entry.role == HostToolRole.Interfaces),
              session.tools.exists(_.entry.role == HostToolRole.Runtime),
              calls == 0,
              before.sameElements(after),
              count == 0,
              !fixtureText.contains("tool compiler"),
              checkout.isEmpty || checkout.linesIterator.exists(_.trim == "tool compiler 3.10.1 jvm"),
              missing,
            )
        }
      },
    ).provideShared(Compilers.owned) @@ TestAspect.sequential @@ TestAspect.withLiveClock @@ TestAspect.timeout(
      20.minutes
    ),
  )

  private val fixtureText = "fixture\n"

  private val plainMain: String =
    """|import histos.*
       |
       |object build extends HistosBuild:
       |  def graph =
       |    val app = module("app", Jvm)(scala"3.10.1")
       |    Projects(app)
       |""".stripMargin

  private val lockedMain: String =
    """|import histos.*
       |
       |object build extends HistosBuild:
       |  def graph =
       |    val app = module("app", Jvm)(scala"3.10.1")
       |    Projects.locked(app)
       |""".stripMargin

  private val emptyCatalog: String =
    """|import histos.*
       |import histos.plugin.*
       |
       |object plugins extends HistosPlugins:
       |  def sources = PluginCatalog.empty
       |""".stripMargin

  private val lockCatalog: String =
    """|import histos.*
       |import histos.plugin.*
       |
       |object plugins extends HistosPlugins:
       |  override def resolution = Resolution.Lock
       |  def sources = PluginCatalog.empty
       |""".stripMargin

  private val echoCatalog: String =
    """|import histos.*
       |import histos.plugin.*
       |
       |object plugins extends HistosPlugins:
       |  val echo = module("echo", Jvm)(scala"3.10.1").in(ProjectPath("echo"))
       |  def sources = PluginCatalog(PluginSources.module(echo))
       |""".stripMargin

  private val echoMain: String =
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

  private val scheduleMain: String =
    """|import histos.*
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
       |    zio.ZIO.succeed(PluginOutput("body", Nil))
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

  private val compileMain: String =
    """|import histos.*
       |import histos.plugin.*
       |import zio.ZLayer
       |
       |final class CompileTask extends HistosPlugin:
       |  val id = PluginId("echo")
       |  def contribute =
       |    Contribution(
       |      List(
       |        DeclaredTask(
       |          TaskName("show"),
       |          Seal.Hermetic,
       |          Set.empty[Need],
       |          Nil,
       |          Nil,
       |          List(TaskDep.Compile(ProjectName("app"), None, None)),
       |        )
       |      ),
       |      Nil,
       |      Nil,
       |    )
       |  def run(request: RunRequest) =
       |    zio.ZIO.succeed(PluginOutput("ran", Nil))
       |
       |object CompileTask:
       |  val layer: ZLayer[Any, Nothing, CompileTask] = ZLayer.succeed(new CompileTask)
       |
       |object build extends HistosBuild:
       |  def graph =
       |    val app = module("app", Jvm)(scala"3.10.1")
       |    Projects(app)
       |
       |  override def services = BuildServices.default.tasks[CompileTask](CompileTask.layer)
       |""".stripMargin

  private def echo(token: String): String =
    s"""|package demo
        |
        |object Echo:
        |  def token: String = "$token"
        |""".stripMargin

  private val moduleCatalog: String =
    """|import histos.*
       |import histos.plugin.*
       |
       |object plugins extends HistosPlugins:
       |  val helper = module("helper", Jvm)(scala"3.10.1")
       |    .in(ProjectPath("helper"))
       |    .libraryDependencies(dep"demo:ext:1")
       |  def sources = PluginCatalog(PluginSources.module(helper))
       |""".stripMargin

  private val moduleCatalogBare: String =
    """|import histos.*
       |import histos.plugin.*
       |
       |object plugins extends HistosPlugins:
       |  val helper = module("helper", Jvm)(scala"3.10.1").in(ProjectPath("helper"))
       |  def sources = PluginCatalog(PluginSources.module(helper))
       |""".stripMargin

  private val useSource: String =
    """|package demo
       |
       |object Use:
       |  def token: String = demo.Ext.n()
       |""".stripMargin

  private val settingsCatalog: String =
    """|import histos.*
       |import histos.plugin.*
       |
       |object plugins extends HistosPlugins:
       |  val helper = module("helper", Jvm)(scala"3.10.1")
       |    .in(ProjectPath("helper"))
       |    .scalac(ScalacFlag.ImplicitConversions)
       |  def sources = PluginCatalog(PluginSources.module(helper))
       |""".stripMargin

  private val settingsSource: String =
    """|package demo
       |
       |object Use:
       |  given Conversion[Int, String] = _.toString
       |  def shown: String = 7
       |  def note: String =
       |    val stream = Option(getClass.getResourceAsStream("/demo/note.txt"))
       |    stream match
       |      case Some(in) =>
       |        try new String(in.readAllBytes(), java.nio.charset.StandardCharsets.UTF_8)
       |        finally in.close()
       |      case None => "missing"
       |  def report: String = s"$shown|$note"
       |""".stripMargin

  private val probeSource: String =
    """|package demo
       |
       |import histos.step.StepKind
       |import histos.theme.Theme
       |
       |object Probe:
       |  def token: String =
       |    val theme = Theme.standard.compiling.isDefined
       |    val step = classOf[StepKind].getName
       |    val schedule =
       |      try
       |        Class.forName("histos.schedule.Sendable", false, getClass.getClassLoader)
       |        "seen"
       |      catch
       |        case _: ClassNotFoundException => "refused"
       |    val loader =
       |      try
       |        Class.forName("histos.load.Loader", false, getClass.getClassLoader)
       |        "seen"
       |      catch
       |        case _: ClassNotFoundException => "refused"
       |    s"$theme:$step|$schedule|$loader"
       |""".stripMargin

  private val commentSource: String =
    """|package demo
       |
       |object Helper:
       |  // trait HistosBuild
       |  def token: String = "compiled"
       |""".stripMargin

  private val bridgeCatalog: String =
    """|import histos.*
       |import histos.plugin.*
       |
       |object plugins extends HistosPlugins:
       |  val bridge = module("bridge", Jvm)(scala"3.10.1").in(ProjectPath("bridge"))
       |  def sources = PluginCatalog(PluginSources.module(bridge))
       |""".stripMargin

  private val markerJava: String =
    """|package demo;
       |public final class Marker {
       |  private Marker() {}
       |  public static String token() { return "from-java"; }
       |}
       |""".stripMargin

  private val bridgeScala: String =
    """|package demo
       |object Bridge:
       |  def text: String = Marker.token()
       |""".stripMargin

  private val orderedCatalog: String =
    """|import histos.*
       |import histos.lock.*
       |import histos.plugin.*
       |
       |object plugins extends HistosPlugins:
       |  private val seed = PluginSources.ship(dep"com.example::seedplug:1.0.0")
       |  private val rest = PluginSources.ship(dep"com.example::restplug:1.0.0")
       |
       |  override def resolution = Resolution.Solve
       |  def sources = PluginCatalog(seed, rest)
       |  override def bootstrap =
       |    BootstrapSource.File(RepoPath("histos.strategies.scala"), required = PluginCatalog(seed))
       |""".stripMargin

  private def oneShip(name: String): String =
    s"""|import histos.*
        |import histos.plugin.*
        |
        |object plugins extends HistosPlugins:
        |  def sources = PluginCatalog(PluginSources.ship(dep"com.example::$name:1.0.0"))
        |""".stripMargin

  private def orderedStrategies(log: Path): String =
    s"""|import histos.lock.*
        |import histos.plugin.*
        |import zio.{ZIO, ZLayer}
        |
        |final class Custom extends LockStrategy:
        |  def decide(context: LockContext) =
        |    ZIO.attemptBlocking {
        |      val path = java.nio.file.Path.of("${log.toString}")
        |      val prior = if java.nio.file.Files.exists(path) then java.nio.file.Files.readString(path) else ""
        |      java.nio.file.Files.writeString(path, prior + "decide\\n")
        |      LockDecision.OfferRepair
        |    }.orDie
        |
        |object Custom:
        |  def layer: ZLayer[Any, Nothing, LockStrategy] =
        |    ZLayer.succeed[LockStrategy](new Custom)
        |
        |object strategies extends HistosStrategies:
        |  def services = StrategyServices.default.withLocks(Custom.layer)
        |""".stripMargin

  private inline val intersectionTag =
    """
    import zio.Tag
    trait ReviewLabel
    trait Marker
    given Tag[ReviewLabel & Marker] = Tag[ReviewLabel & Marker]
    """

  private inline val intersectionKey =
    """
    import histos.plugin.*
    import zio.{Tag, ZLayer}
    trait ReviewLabel
    trait Marker
    given Tag[ReviewLabel & Marker] = Tag[ReviewLabel & Marker]
    val layer: ZLayer[Any, Nothing, ReviewLabel & Marker] =
      ZLayer.succeed[ReviewLabel & Marker](new ReviewLabel with Marker {})
    val added = BuildServices.default.add[ReviewLabel & Marker](layer)
    """

  private inline val oneCapability =
    """
    import histos.plugin.*
    import zio.{Tag, ZLayer}
    trait ReviewLabel
    given Tag[ReviewLabel] = Tag[ReviewLabel]
    val layer: ZLayer[Any, Nothing, ReviewLabel] = ZLayer.succeed[ReviewLabel](new ReviewLabel {})
    val added = BuildServices.default.add[ReviewLabel](layer)
    """

  private final case class Ran(text: String, sealedBytes: String)

  private final case class ExtJar(path: Path, notes: List[String], compiled: Boolean)

  private final case class HelperOut(product: ContentHash, classes: Path)

  private final case class Heard(events: List[InvocationPluginEvent], built: Int)

  private final case class Packed(uploads: Int, entries: Map[String, Array[Byte]]):
    def text(name: String): Option[String] =
      entries.get(name).map(bytes => new String(bytes, utf8))

  private def hosted[A](
      body: (Path, Ref[Int]) => ZIO[BuildCache.Env & histos.load.PluginProducts & Scope, LoadError, A]
  ): ZIO[Compilers, LoadError, A] =
    ZIO.scoped {
      for
        root   <- work("histos-m6-host")
        cas    <- ZIO.attemptBlocking(Files.createDirectories(root.resolve("cas"))).orDie
        starts <- Ref.make(0)
        value  <- body(root, starts).provideSomeLayer[Compilers & Scope](
          ZLayer.succeed(ContentStore.in(cas)) ++
            ZLayer.succeed(CompilerStarts(starts)) ++
            RemoteCache.off ++
            histos.load.PluginFlights.live ++ histos.load.PluginProducts.live
        )
      yield value
    }

  private def prepare(
      root: Path,
      api: Path,
      resolve: Resolve,
      payload: Option[Path] = None,
      notice: String => zio.UIO[Unit] = _ => ZIO.unit,
  ): ZIO[BuildCache.Env & histos.load.PluginProducts & Scope, LoadError, histos.load.LoadedBuild] =
    BootstrapLoad.prepare(resolve, BootstrapLoad.Prepare(root, api, payload, notice))

  private def runTask(
      root: Path,
      api: Path,
      cache: ActionCache,
      id: PluginId,
  ): ZIO[BuildCache.Env & histos.load.PluginProducts & Scope, LoadError, Ran] =
    for
      loaded <- prepare(root, api, silent)
      output <- loaded.plugins
        .run(PluginTaskId(id, TaskName("show")), PluginArgs.empty)
        .provideSome[ContentStore & RemoteCache](
          ZLayer.succeed(silent),
          ResolutionService.pinned(DependencyLock(Nil)),
          ZLayer.succeed(cache),
        )
        .mapError(fault => LoadError.Plugin(PluginLoadError.Construction(fault.message)))
    yield Ran(output.text, sealedBytes(loaded))

  private def sealedBytes(loaded: histos.load.LoadedBuild): String =
    loaded.provenance match
      case histos.load.ProvenanceResult.Verified(manifest) :: Nil =>
        manifest.artifacts.map(_.identity.digest.value).mkString(",")
      case _ => ""

  private def faultKind(
      root: Path,
      main: String,
      cache: ActionCache,
  ): ZIO[BuildCache.Env & histos.load.PluginProducts & Scope, LoadError, Either[TaskError, histos.task.TaskResult]] =
    for
      _      <- ZIO.attemptBlocking(Files.createDirectories(root)).orDie
      api    <- apiJar(root.resolve("api.jar"))
      _      <- write(root.resolve("histos.plugins.scala"), emptyCatalog)
      _      <- write(root.resolve("histos.scala"), main)
      _      <- hostFacts
      loaded <- prepare(root, api, silent)
      store  <- ZIO.service[ContentStore]
      bound  <- loaded.plugins.bind
        .provide(
          ZLayer.succeed(cache),
          RemoteCache.off,
          ZLayer.succeed(silent),
          ZLayer.succeed(store),
          ResolutionService.pinned(DependencyLock(Nil)),
        )
        .mapError(error => LoadError.Plugin(PluginLoadError.Construction(error.message)))
      id <- ZIO
        .fromEither(TaskId.from(PluginTaskId(PluginId("echo"), TaskName("show")).render))
        .mapError(error => LoadError.Plugin(PluginLoadError.Construction(error.message)))
      effect <- bound.get(id) match
        case Some(found) => ZIO.succeed(found)
        case None        => ZIO.fail(LoadError.Plugin(PluginLoadError.Construction(id.value)))
      exit <- effect.either
    yield exit

  private def sessionFor(
      root: Path
  ): ZIO[BuildCache.Env & histos.load.PluginProducts & Scope, LoadError, HostCompilerSession] =
    for
      files   <- packaged
      payload <- payloadJar(root, files, None, corrupt = false)
      store   <- ZIO.service[ContentStore]
      home    <- jdkHome
      session <- HostCompilerInputs.materialize(payload, home, store).mapError(LoadError.HostCompiler(_))
    yield session

  private def compileMark(
      root: Path,
      session: HostCompilerSession,
      name: String,
      api: List[Path],
  ): ZIO[BuildCache.Env, LoadError, histos.load.PluginCompileResult] =
    for
      source <- write(
        root.resolve(s"$name/src/Mark.scala"),
        """|object Mark:
           |  def text: String = "one"
           |""".stripMargin,
      )
      result <- histos.load.PluginCompiler
        .compile(
          histos.load.PluginCompileJob(
            ProjectName("app"),
            session,
            root.resolve(name),
            root.resolve(name),
            List(source),
            apiJars = api,
            classes = root.resolve(s"$name/classes"),
            analysis = root.resolve(s"$name/analysis"),
          )
        )
        .mapError(pluginFailed)
    yield result

  private def helper(
      root: Path,
      session: HostCompilerSession,
      label: String,
  ): ZIO[BuildCache.Env, LoadError, HelperOut] =
    val base = root.resolve(s"helper-$label")
    for
      source <- write(
        base.resolve("src/helper/Helper.scala"),
        s"""|package helper
            |object Helper:
            |  type Label = "$label"
            |""".stripMargin,
      )
      result <- histos.load.PluginCompiler
        .compile(
          histos.load.PluginCompileJob(
            ProjectName("helper"),
            session,
            base,
            base,
            List(source),
            classes = base.resolve("classes"),
            analysis = base.resolve("analysis"),
          )
        )
        .mapError(pluginFailed)
    yield HelperOut(result.snapshot.outputs.identity.digest, base.resolve("classes"))

  private def consumer(
      root: Path,
      session: HostCompilerSession,
      helper: HelperOut,
      label: String,
  ): ZIO[BuildCache.Env & histos.load.PluginProducts & Scope, LoadError, String] =
    val base = root.resolve(s"use-$label")
    for
      source <- write(
        base.resolve("src/Use.scala"),
        """|object Use:
           |  inline def token: String = scala.compiletime.constValue[helper.Helper.Label]
           |  def text: String = token
           |""".stripMargin,
      )
      _ <- histos.load.PluginCompiler
        .compile(
          histos.load.PluginCompileJob(
            ProjectName("app"),
            session,
            base,
            base,
            List(source),
            classpath = List(helper.classes),
            classes = base.resolve("classes"),
            analysis = base.resolve("analysis"),
            helpers = List(ArtifactIdentity(ArtifactFormat.Jar, helper.product)),
          )
        )
        .mapError(pluginFailed)
      text <- invoke(base.resolve("classes"), runtimeJars(session) :+ helper.classes, "Use$", "text")
    yield text

  private def runtimeJars(session: HostCompilerSession): List[Path] =
    session.tools
      .filter(tool => tool.entry.role == HostToolRole.Runtime || tool.entry.role == HostToolRole.Support)
      .map(_.path)

  private def invoke(classes: Path, more: List[Path], binary: String, method: String): ZIO[Scope, LoadError, String] =
    ZIO
      .acquireRelease(
        ZIO
          .attemptBlocking(
            new URLClassLoader((classes :: more).map(_.toUri.toURL).toArray, ClassLoader.getPlatformClassLoader)
          )
          .mapError(_ => LoadError.Io(IoFailure(classes, IoFault.Read)))
      )(loader => ZIO.attemptBlocking(loader.close()).ignore)
      .flatMap { loader =>
        ZIO
          .attemptBlocking {
            val cls      = loader.loadClass(binary)
            val instance = cls.getField("MODULE$").get(cls)
            cls.getMethod(method).invoke(instance)
          }
          .mapError(_ => LoadError.Io(IoFailure(classes, IoFault.Read)))
          .flatMap {
            case text: String => ZIO.succeed(text)
            case _            => ZIO.fail(LoadError.Io(IoFailure(classes, IoFault.Read)))
          }
      }

  private def flipTasty(dest: Path, classes: Path): IO[LoadError, Path] =
    ZIO
      .attemptBlocking {
        val walk  = Files.walk(classes)
        val files =
          try walk.iterator.asScala.filter(path => Files.isRegularFile(path)).toList
          finally walk.close()
        files.foreach { path =>
          val relative = classes.relativize(path)
          val target   = dest.resolve(relative)
          Option(target.getParent).foreach(parent => Files.createDirectories(parent))
          val bytes   = Files.readAllBytes(path)
          val written =
            if path.getFileName.toString.endsWith(".tasty") && bytes.nonEmpty then flipped(bytes) else bytes
          Files.write(target, written)
        }
        dest
      }
      .mapError(_ => LoadError.Io(IoFailure(dest, IoFault.Write)))

  private def flipped(bytes: Array[Byte]): Array[Byte] =
    val copy = bytes.clone()
    val last = copy.length - 1
    copy(last) = (copy(last) ^ 0xff.toByte).toByte
    copy

  private def sealDirectory(root: Path): IO[LoadError, histos.jdk.Sha256] =
    treeEntries(root).flatMap(entries => ZIO.fromEither(sealKey(entries)).mapError(publishFailed))

  private def treeEntries(root: Path): IO[LoadError, List[EntryBytes]] =
    ZIO
      .attemptBlocking {
        val walk = Files.walk(root)
        try
          walk.iterator.asScala.filter(path => Files.isRegularFile(path)).toList.map { path =>
            val name = root.relativize(path).toString.replace('\\', '/')
            EntryBytes(name, Chunk.fromArray(Files.readAllBytes(path)))
          }
        finally walk.close()
      }
      .mapError(_ => LoadError.Io(IoFailure(root, IoFault.Read)))

  private def packaged: ZIO[Scope, LoadError, List[AssemblyFile]] =
    for
      jdk   <- runningJdk
      files <- ZIO
        .serviceWithZIO[Resolve] { resolve =>
          resolve
            .resolveTool(CompilerRequests.compiler(Scala3101, Repositories.central))
            .mapError(LoadError.Resolve(_))
            .flatMap(classpath =>
              histos.toolchain.HostCompilerPackage
                .files(Scala3101, jdk, classpath.artifacts)
                .mapError(LoadError.HostCompiler(_))
            )
        }
        .provideSome[Scope](Resolve.coursier)
    yield files

  private def payloadJar(
      root: Path,
      files: List[AssemblyFile],
      drop: Option[HostToolRole],
      corrupt: Boolean,
  ): IO[LoadError, Path] =
    val stage = root.resolve("stage")
    val jar   = root.resolve("host.jar")
    for
      manifest <- readManifest(files)
      dropped = drop match
        case None       => Set.empty[String]
        case Some(role) =>
          manifest.tools
            .filter(_.role == role)
            .map(entry => s"bootstrap/compiler/${entry.relative.value}")
            .toSet
      _ <- ZIO.foreachDiscard(files.filterNot(file => dropped.contains(file.entry))) { file =>
        copyFile(file.file, stage.resolve(file.entry))
      }
      _ <-
        if corrupt then
          manifest.tools.find(_.role == HostToolRole.Bridge) match
            case Some(entry) => flipFile(stage.resolve("bootstrap/compiler").resolve(entry.relative.value))
            case None        => ZIO.fail(LoadError.HostCompiler(HostCompilerError.EntryAbsent(HostToolRole.Bridge)))
        else ZIO.unit
      _ <- writeBytes(stage.resolve("histos/cli/Main.class"), Array[Byte](0xca.toByte, 0xfe.toByte))
      _ <- writeBytes(stage.resolve(BuildClasspath.Entry), Array[Byte](0x50, 0x4b))
      _ <- ClassJar.write(stage, jar).mapError(LoadError.Jar(_))
    yield jar

  private def readManifest(files: List[AssemblyFile]): IO[LoadError, HostCompilerManifest] =
    files.find(_.entry == HostCompilerInputs.ManifestPath) match
      case None       => ZIO.fail(LoadError.HostCompiler(HostCompilerError.ManifestAbsent))
      case Some(file) =>
        ZIO
          .attemptBlocking(Files.readString(file.file, utf8))
          .mapError(_ => LoadError.Io(IoFailure(file.file, IoFault.Read)))
          .flatMap { text =>
            ZIO
              .fromEither(text.fromJson[HostCompilerManifest])
              .mapError(_ => LoadError.HostCompiler(HostCompilerError.ManifestUnreadable))
          }

  private def bridgeEntry(session: HostCompilerSession): IO[LoadError, histos.toolchain.MaterializedTool] =
    session.tools.find(_.entry.role == HostToolRole.Bridge) match
      case Some(tool) => ZIO.succeed(tool)
      case None       => ZIO.fail(LoadError.HostCompiler(HostCompilerError.EntryAbsent(HostToolRole.Bridge)))

  private def jarEntryDigest(jar: Path, name: String): IO[LoadError, ContentHash] =
    ZIO
      .attemptBlocking {
        val zip = new ZipFile(jar.toFile)
        try
          Option(zip.getEntry(name)).map { entry =>
            val input = zip.getInputStream(entry)
            try input.readAllBytes()
            finally input.close()
          }
        finally zip.close()
      }
      .mapError(_ => LoadError.Io(IoFailure(jar, IoFault.Read)))
      .flatMap {
        case Some(bytes) => ZIO.succeed(ContentHash.trusted(Sha256Digest.of(Chunk.fromArray(bytes)).value))
        case None        => ZIO.fail(LoadError.HostCompiler(HostCompilerError.EntryAbsent(HostToolRole.Bridge)))
      }

  private def runningJdk: IO[LoadError, histos.jdk.JdkVersion] =
    Option(System.getProperty("java.home")).filter(_.nonEmpty) match
      case None      => ZIO.fail(LoadError.Io(IoFailure(Path.of("java.home"), IoFault.Stat)))
      case Some(dir) =>
        val home = Path.of(dir)
        Jdk
          .home(home)
          .mapError {
            case JdkError.Io(failure) => LoadError.Io(failure)
            case JdkError.BadMajor(_) => LoadError.Io(IoFailure(home, IoFault.Read))
          }
          .flatMap { found =>
            found.release match
              case Left(_)        => ZIO.fail(LoadError.Io(IoFailure(home.resolve("release"), IoFault.Read)))
              case Right(release) => ZIO.succeed(release.version)
          }

  private def jdkHome: IO[LoadError, Path] =
    Option(System.getProperty("java.home")).filter(_.nonEmpty) match
      case Some(dir) => ZIO.succeed(Path.of(dir))
      case None      => ZIO.fail(LoadError.Io(IoFailure(Path.of("java.home"), IoFault.Stat)))

  private def fixture(root: Path): IO[LoadError, Array[Byte]] =
    write(root.resolve("dependencies.lock"), fixtureText) *> readBytes(root.resolve("dependencies.lock"))

  private def checkoutLock: IO[LoadError, String] =
    val start = Option(System.getProperty("user.dir")).filter(_.nonEmpty).fold(Path.of("."))(Path.of(_))
    walkLock(start.toAbsolutePath.normalize, 8)

  private def walkLock(dir: Path, left: Int): IO[LoadError, String] =
    val lock = dir.resolve("dependencies.lock")
    ZIO
      .attemptBlocking(Files.isRegularFile(lock))
      .mapError(_ => LoadError.Io(IoFailure(lock, IoFault.Stat)))
      .flatMap {
        case true =>
          ZIO
            .attemptBlocking(Files.readString(lock, utf8))
            .mapError(_ => LoadError.Io(IoFailure(lock, IoFault.Read)))
        case false =>
          Option(dir.getParent) match
            case Some(parent) if left > 0 => walkLock(parent, left - 1)
            case _                        => ZIO.succeed("")
      }

  private def apiJar(destination: Path): IO[LoadError, Path] =
    val location = Option(classOf[HistosBuild].getProtectionDomain)
      .flatMap(domain => Option(domain.getCodeSource))
      .flatMap(source => Option(source.getLocation))
    location match
      case None      => ZIO.fail(LoadError.MissingFile(Path.of("histos-api.jar")))
      case Some(url) =>
        ZIO
          .attempt(Path.of(url.toURI))
          .mapError(_ => LoadError.Io(IoFailure(Path.of(url.toString), IoFault.Read)))
          .flatMap { path =>
            if Files.isRegularFile(path) then ZIO.succeed(path)
            else zipTree(path, destination)
          }

  private def zipTree(root: Path, destination: Path): IO[LoadError, Path] =
    ZIO
      .attemptBlocking {
        Option(destination.getParent).foreach(parent => Files.createDirectories(parent))
        val out  = new ZipOutputStream(Files.newOutputStream(destination))
        val walk = Files.walk(root)
        try
          walk.iterator.asScala.foreach { path =>
            if Files.isRegularFile(path) then
              val name = root.relativize(path).toString.replace('\\', '/')
              writeEntry(out, name, Files.readAllBytes(path))
          }
        finally
          walk.close()
          out.close()
        destination
      }
      .mapError(_ => LoadError.Io(IoFailure(destination, IoFault.Write)))

  private def apiBody(path: Path, body: String): IO[LoadError, Path] =
    ZIO
      .attemptBlocking {
        Option(path.getParent).foreach(parent => Files.createDirectories(parent))
        val out = new ZipOutputStream(Files.newOutputStream(path))
        try writeEntry(out, "marker.txt", body.getBytes(utf8))
        finally out.close()
        path
      }
      .mapError(_ => LoadError.Io(IoFailure(path, IoFault.Write)))

  private def pluginJar(
      path: Path,
      version: String,
      classEntry: String,
      descriptor: Option[String] = None,
  ): IO[LoadError, Path] =
    ZIO
      .attemptBlocking {
        Option(path.getParent).foreach(parent => Files.createDirectories(parent))
        val out = new ZipOutputStream(Files.newOutputStream(path))
        try
          writeEntry(out, ApiDigest.resource, version.getBytes(utf8))
          writeEntry(out, classEntry, classEntry.getBytes(utf8))
          descriptor match
            case Some(text) => writeEntry(out, "META-INF/histos/plugin.json", text.getBytes(utf8))
            case None       => ()
        finally out.close()
        path
      }
      .mapError(_ => LoadError.Io(IoFailure(path, IoFault.Write)))

  private def digestOf(path: Path): IO[LoadError, String] =
    ZIO
      .attemptBlocking(Sha256Digest.of(Chunk.fromArray(Files.readAllBytes(path))).value)
      .mapError(_ => LoadError.Io(IoFailure(path, IoFault.Read)))

  private def externalJar(path: Path): IO[LoadError, ExtJar] =
    val source  = path.getParent.resolve("Ext.java")
    val classes = path.getParent.resolve("ext-classes")
    Option(javax.tools.ToolProvider.getSystemJavaCompiler) match
      case None           => ZIO.succeed(ExtJar(path, List("no system Java compiler"), compiled = false))
      case Some(compiler) =>
        for
          _ <- write(
            source,
            """|package demo;
               |public final class Ext {
               |  private Ext() {}
               |  public static String n() { return "ext"; }
               |}
               |""".stripMargin,
          )
          _       <- ZIO.attemptBlocking(Files.createDirectories(classes)).orDie
          outcome <- ZIO
            .attemptBlocking {
              val notes    = new java.util.ArrayList[String]()
              val listener = new javax.tools.DiagnosticListener[javax.tools.JavaFileObject]:
                def report(diagnostic: javax.tools.Diagnostic[? <: javax.tools.JavaFileObject]): Unit =
                  notes.add(diagnostic.getMessage(java.util.Locale.ROOT))
              val manager = compiler.getStandardFileManager(listener, java.util.Locale.ROOT, utf8)
              try
                val units = manager.getJavaFileObjects(source.toFile)
                val task  = compiler.getTask(
                  new java.io.StringWriter,
                  manager,
                  listener,
                  List("-d", classes.toString).asJava,
                  java.util.List.of[String](),
                  units,
                )
                val ok = task.call().booleanValue()
                (ok, notes.asScala.toList)
              finally manager.close()
            }
            .mapError(_ => LoadError.Io(IoFailure(source, IoFault.Write)))
          (ok, notes) = outcome
          _ <-
            if ok then jarClasses(classes, path)
            else ZIO.unit
        yield ExtJar(path, notes, ok)

  private def jarClasses(classes: Path, jar: Path): IO[LoadError, Path] =
    ZIO
      .attemptBlocking {
        val out  = new ZipOutputStream(Files.newOutputStream(jar))
        val walk = Files.walk(classes)
        try
          walk.iterator.asScala.foreach { file =>
            if Files.isRegularFile(file) && file.getFileName.toString.endsWith(".class") then
              val name = classes.relativize(file).toString.replace('\\', '/')
              writeEntry(out, name, Files.readAllBytes(file))
          }
        finally
          walk.close()
          out.close()
        jar
      }
      .mapError(_ => LoadError.Io(IoFailure(jar, IoFault.Write)))

  private def writeEntry(zip: ZipOutputStream, name: String, bytes: Array[Byte]): Unit =
    zip.putNextEntry(new ZipEntry(name))
    zip.write(bytes)
    zip.closeEntry()

  private val silent: Resolve =
    new Resolve:
      def resolveTool(request: ToolRequest)                          = ZIO.dieMessage("resolveTool")
      def resolve(cell: histos.resolve.ResolveCell)                  = ZIO.dieMessage("resolve")
      def fetch(artifact: histos.resolve.LockedArtifact)             = ZIO.dieMessage("fetch")
      def resolvePlugin(dependency: Dep, repositories: Repositories) = ZIO.dieMessage("resolvePlugin")
      def sources(organization: String, name: String, revision: String, repository: histos.resolve.Repository) =
        ZIO.dieMessage("sources")

  private def counting(seen: Ref[Int]): Resolve =
    new Resolve:
      def resolveTool(request: ToolRequest) =
        seen.update(_ + 1) *> ZIO.dieMessage("resolveTool")
      def resolve(cell: histos.resolve.ResolveCell) =
        seen.update(_ + 1) *> ZIO.dieMessage("resolve")
      def fetch(artifact: histos.resolve.LockedArtifact) =
        seen.update(_ + 1) *> ZIO.dieMessage("fetch")
      def resolvePlugin(dependency: Dep, repositories: Repositories) =
        seen.update(_ + 1) *> ZIO.dieMessage("resolvePlugin")
      def sources(organization: String, name: String, revision: String, repository: histos.resolve.Repository) =
        ZIO.dieMessage("sources")

  private def shipJars(name: String, direct: Path, helper: Path): Resolve =
    new Resolve:
      def resolveTool(request: ToolRequest)                          = ZIO.dieMessage("resolveTool")
      def resolve(cell: histos.resolve.ResolveCell)                  = ZIO.dieMessage("resolve")
      def fetch(artifact: histos.resolve.LockedArtifact)             = ZIO.dieMessage("fetch")
      def resolvePlugin(dependency: Dep, repositories: Repositories) =
        if dependency.name == name then
          val own   = artifactOf(dependency, direct)
          val extra = own.copy(name = dependency.name + "-helper", path = helper)
          ZIO.succeed(Classpath(List(own, extra), Nil))
        else ZIO.dieMessage(dependency.name)
      def sources(organization: String, name: String, revision: String, repository: histos.resolve.Repository) =
        ZIO.dieMessage("sources")

  private def extResolve(path: Path): Resolve =
    new Resolve:
      def resolveTool(request: ToolRequest)         = ZIO.dieMessage("resolveTool")
      def resolve(cell: histos.resolve.ResolveCell) =
        cell.dependencies match
          case dependency :: _ => ZIO.succeed(Classpath(List(artifactOf(dependency, path)), Nil))
          case Nil             => ZIO.dieMessage("resolve")
      def fetch(artifact: histos.resolve.LockedArtifact)             = ZIO.dieMessage("fetch")
      def resolvePlugin(dependency: Dep, repositories: Repositories) = ZIO.dieMessage("resolvePlugin")
      def sources(organization: String, name: String, revision: String, repository: histos.resolve.Repository) =
        ZIO.dieMessage("sources")

  private def jarsOf(log: Path, jars: Map[String, Path]): Resolve =
    new Resolve:
      def resolveTool(request: ToolRequest)                          = ZIO.dieMessage("resolveTool")
      def resolve(cell: histos.resolve.ResolveCell)                  = ZIO.dieMessage("resolve")
      def fetch(artifact: histos.resolve.LockedArtifact)             = ZIO.dieMessage("fetch")
      def resolvePlugin(dependency: Dep, repositories: Repositories) =
        jars.get(dependency.name) match
          case None       => ZIO.dieMessage(dependency.name)
          case Some(path) =>
            append(log)(s"plugin:${dependency.name}") *> ZIO.succeed(Classpath(List(artifactOf(dependency, path)), Nil))
      def sources(organization: String, name: String, revision: String, repository: histos.resolve.Repository) =
        ZIO.dieMessage("sources")

  private def artifactOf(dependency: Dep, path: Path): ResolvedArtifact =
    ResolvedArtifact(
      dependency.organization,
      dependency.name,
      dependency.revision.value,
      path,
      Sha256Digest.of(Chunk.fromArray(path.toString.getBytes(utf8))),
      histos.resolve.Repository.Central,
    )

  private def memory: zio.UIO[ActionCache] =
    Ref.make(Map.empty[String, String]).map { kept =>
      new ActionCache:
        def get(digest: Digest): IO[CacheError, Option[String]] =
          kept.get.map(_.get(digest.value))
        def put(digest: Digest, value: String): IO[CacheError, Unit] =
          kept.update(_.updated(digest.value, value))
    }

  private val brokenCache: ActionCache = new ActionCache:
    def get(digest: Digest): IO[CacheError, Option[String]]      = ZIO.fail(CacheError.Corrupt(digest))
    def put(digest: Digest, value: String): IO[CacheError, Unit] = ZIO.unit

  private def token(build: HistosBuild, binary: String, method: String): ZIO[Any, LoadError, String] =
    ZIO
      .attemptBlocking {
        val cls      = build.getClass.getClassLoader.loadClass(binary)
        val instance = cls.getField("MODULE$").get(cls)
        cls.getMethod(method).invoke(instance)
      }
      .mapError(_ => LoadError.Io(IoFailure(Path.of(binary), IoFault.Read)))
      .flatMap {
        case text: String => ZIO.succeed(text)
        case _            => ZIO.fail(LoadError.Io(IoFailure(Path.of(binary), IoFault.Read)))
      }

  private def classFile(build: HistosBuild, relative: String): Boolean =
    build.getClass.getClassLoader match
      case loader: URLClassLoader =>
        loader.getURLs.toList
          .flatMap(url => scala.util.Try(Path.of(url.toURI)).toOption)
          .exists(path => Files.isRegularFile(path.resolve(relative)))
      case _ => false

  private def hostFacts: zio.UIO[Unit] =
    ZIO.foreachDiscard(List("user.home", "os.name", "os.arch", "java.home")) { key =>
      Option(System.getProperty(key)) match
        case Some(value) if value.nonEmpty => TestSystem.putProperty(key, value)
        case _                             => ZIO.dieMessage(s"the test JVM has no $key")
    }

  private def append(path: Path)(line: String): zio.UIO[Unit] =
    ZIO.attemptBlocking {
      val prior = if Files.exists(path) then Files.readString(path, utf8) else ""
      Files.writeString(path, prior + line + "\n", utf8)
      ()
    }.orDie

  private def caller(
      desk: InvocationDesk,
      gate: zio.Promise[Nothing, Unit],
      arrived: Ref[Int],
      who: InvocationContext,
      text: String,
  ): ZIO[Scope, PluginEventError, Heard] =
    ZIO.scoped {
      desk.open(who).flatMap { lease =>
        for
          seen  <- arrived.updateAndGet(_ + 1)
          _     <- ZIO.when(seen == 2)(gate.succeed(())).unit
          _     <- gate.await
          built <- desk.constructions
          _     <- lease.invocation.emit(InvocationPluginEvent.Message(PluginDetail.trusted(text)))
          heard <- lease.heard
        yield Heard(heard, built)
      }
    }

  private def messages(events: List[InvocationPluginEvent]): List[String] =
    events.collect { case InvocationPluginEvent.Message(detail) => detail.value }

  private def deliver(
      frame: RunInput | LockReply,
      program: zio.Queue[RunInput],
      consent: zio.Queue[LockReply],
  ): zio.UIO[Unit] =
    val payload = frame match
      case input: RunInput  => input.toJson
      case reply: LockReply => reply.toJson
    ZIO
      .succeed(
        ClientFrames.deliver(
          ClientFrames.classify(payload),
          programOpen = true,
          (found: RunInput) => program.offer(found).unit,
          (found: LockReply) => consent.offer(found).unit,
          ZIO.unit,
        )
      )
      .flatten

  private def activate[E, A](effect: ZIO[GenerationAmbient & CoreServices & Scope, E, A]): ZIO[Scope, E, A] =
    ZIO.serviceWithZIO[Scope] { scope =>
      effect.provide(
        ZLayer.succeed[BuildContext](Ambient.context),
        ZLayer.succeed[PluginEvents.Generation](Ambient.events),
        ZLayer.succeed[histos.lock.LockStrategy](Ambient.locks),
        PluginLoading.exactOrRebuild,
        ZLayer.succeed(scope),
      )
    }

  private object Ambient:
    private inline val digest = "0000000000000000000000000000000000000000000000000000000000000000"

    val context: BuildContext = new BuildContext:
      def generation: GenerationId  = GenerationId(digest)
      def repository: String        = "repo"
      def declarations: ContentHash = ContentHash(digest)
      def host: ApiDigest           = ApiDigest(digest)

    val events: PluginEvents.Generation = new PluginEvents.Generation:
      def emit(event: GenerationPluginEvent): zio.IO[PluginEventError, Unit] = ZIO.unit

    val locks: histos.lock.LockStrategy = new histos.lock.LockStrategy:
      def decide(context: histos.lock.LockContext): zio.IO[histos.lock.LockStrategyError, histos.lock.LockDecision] =
        ZIO.succeed(histos.lock.LockDecision.UsePinned)
  end Ambient

  private def classBytes: Array[Byte] =
    val node = new ClassNode()
    node.version = Opcodes.V1_8
    node.access = Opcodes.ACC_PUBLIC | Opcodes.ACC_SUPER
    node.name = "pkg/A"
    node.superName = "java/lang/Object"
    node.interfaces = new java.util.ArrayList[String]()
    val writer = new org.objectweb.asm.ClassWriter(0)
    node.accept(writer)
    writer.toByteArray

  private def writeTree(root: Path, classFile: Array[Byte], tasty: Array[Byte], stamp: Option[Array[Byte]]): Unit =
    val pkg = root.resolve("pkg")
    Files.createDirectories(pkg)
    Files.write(pkg.resolve("A.class"), classFile)
    Files.write(pkg.resolve("A.tasty"), tasty)
    stamp match
      case None        => ()
      case Some(bytes) =>
        val meta = root.resolve("META-INF/histos")
        Files.createDirectories(meta)
        Files.write(meta.resolve("version"), bytes)

  private def entriesOf(classFile: Array[Byte], tasty: Array[Byte], stamp: Option[Array[Byte]]): List[EntryBytes] =
    val products = List(
      EntryBytes("pkg/A.class", Chunk.fromArray(classFile)),
      EntryBytes("pkg/A.tasty", Chunk.fromArray(tasty)),
    )
    stamp match
      case None        => products
      case Some(bytes) => products :+ EntryBytes("META-INF/histos/version", Chunk.fromArray(bytes))

  private def sealKey(listed: List[EntryBytes]): Either[PluginPublishFault, histos.jdk.Sha256] =
    PluginBundle
      .manifest(listed)
      .map(found => PackageKey.sealedOf(ArtifactFormat.Jar, PluginPublication.Rebuildable, found))

  private def manifestKey(listed: List[EntryBytes]): Either[PluginPublishFault, histos.jdk.Sha256] =
    PluginBundle.manifest(listed).map(found => PackageKey.manifestOf(ArtifactFormat.Jar, found))

  private val revision = Revision.Release(ReleaseVersion(ShipNumber("0.1.0")))
  private val node     = RecipeNodeId("plug")
  private val blob     = "histos-contract".getBytes(utf8)

  private def publishRequest(
      sources: List[SelectedInput],
      closure: List[ClosureArtifact],
      current: List[SourceBytes],
      dependencies: List[histos.central.MavenDependency] = Nil,
  ): Either[PluginPublishFault, PluginPublishRequest] =
    for
      artifact <- histos.central.ArtifactId
        .from("plug")
        .left
        .map(_ => PluginPublishFault.Dependency("com.example", "plug", "0.1.0"))
      jdk     <- histos.jdk.JdkVersion.from("25").left.map(_ => PluginPublishFault.Dependency("jdk", "25", "25"))
      outputs <- PluginBundle.manifest(List(EntryBytes("pkg/A.class", Chunk.fromArray(blob)))).left.map(identity)
      snapshot = CompilationSnapshot(
        ProjectName("plug"),
        Scala3101,
        Platform.Jvm,
        sources,
        Nil,
        Nil,
        Nil,
        closure,
        Nil,
        Nil,
        Nil,
        Nil,
        jdk,
        outputs,
        List(EntryName("scala.quoted.Quotes")),
      )
    yield PluginPublishRequest(
      PluginPublication.Rebuildable,
      node,
      CompilationCapture.Recorded(snapshot),
      current,
      List(EntryBytes("pkg/A.class", Chunk.fromArray(blob))),
      None,
      dependencies,
      Organization("com.example"),
      artifact,
      revision,
    )

  private def artifactOf(staged: StagedPlugin, dependencies: List[histos.central.MavenDependency]): Artifact =
    Artifact(
      Organization("com.example"),
      histos.central.ArtifactId.trusted("plug"),
      revision,
      staged.bytes,
      dependencies,
    )

  private def upload(request: PluginPublishRequest): ZIO[Any, PluginPublishFault, Packed] =
    ZIO.scoped {
      for
        root <- Workspace.work("histos-m6-upload").mapError(PluginPublishFault.Io(_))
        hits <- Ref.make(0)
        layer = Publisher.local(root) >>> ZLayer.fromFunction((inner: Publisher) => Counting(inner, hits): Publisher)
        result <- PluginBundle.publish(request).either.provide(layer)
        count  <- hits.get
        packed <- result match
          case Left(fault)                             => ZIO.fail(fault)
          case Right(Published.Snapshot(_, _))         => ZIO.succeed(Packed(count, Map.empty))
          case Right(Published.Local(jar, _, _, _, _)) =>
            ZIO
              .attemptBlocking(Packed(count, unzip(jar)))
              .mapError(_ => PluginPublishFault.Io(IoFailure(jar, IoFault.Read)))
      yield packed
    }

  private final class Counting(inner: Publisher, hits: Ref[Int]) extends Publisher:
    def publish(artifact: Artifact): zio.IO[histos.central.PublishError, Published] =
      hits.update(_ + 1) *> inner.publish(artifact)

  private def unzip(path: Path): Map[String, Array[Byte]] =
    val zip = new ZipFile(path.toFile)
    try
      zip.entries.asScala
        .filterNot(_.isDirectory)
        .map { entry =>
          val stream = zip.getInputStream(entry)
          try entry.getName -> stream.readAllBytes()
          finally stream.close()
        }
        .toMap
    finally zip.close()

  private def copyFile(from: Path, dest: Path): IO[LoadError, Unit] =
    ZIO
      .attemptBlocking {
        Option(dest.getParent).foreach(parent => Files.createDirectories(parent))
        Files.copy(from, dest, StandardCopyOption.REPLACE_EXISTING)
        ()
      }
      .mapError(_ => LoadError.Io(IoFailure(dest, IoFault.Write)))

  private def flipFile(path: Path): IO[LoadError, Unit] =
    ZIO
      .attemptBlocking(Files.readAllBytes(path))
      .mapError(_ => LoadError.Io(IoFailure(path, IoFault.Read)))
      .flatMap { bytes =>
        if bytes.isEmpty then ZIO.fail(LoadError.Io(IoFailure(path, IoFault.Read)))
        else writeBytes(path, flipped(bytes))
      }

  private def writeBytes(path: Path, bytes: Array[Byte]): IO[LoadError, Unit] =
    ZIO
      .attemptBlocking {
        Option(path.getParent).foreach(parent => Files.createDirectories(parent))
        Files.write(path, bytes)
        ()
      }
      .mapError(_ => LoadError.Io(IoFailure(path, IoFault.Write)))

  private def write(path: Path, text: String): IO[LoadError, Path] =
    ZIO
      .attemptBlocking {
        Option(path.getParent).foreach(parent => Files.createDirectories(parent))
        Files.writeString(path, text, utf8)
        path
      }
      .mapError(_ => LoadError.Io(IoFailure(path, IoFault.Write)))

  private def readBytes(path: Path): IO[LoadError, Array[Byte]] =
    ZIO.attemptBlocking(Files.readAllBytes(path)).mapError(_ => LoadError.Io(IoFailure(path, IoFault.Read)))

  private def work(name: String): ZIO[Scope, LoadError, Path] =
    Workspace.work(name).mapError(LoadError.Io(_))

  private def digestFailed(error: histos.zinc.CompileError): LoadError =
    LoadError.CompileFailed(List(histos.load.CompilerMessage.fromCompiler(error.message)))

  private def pluginFailed(error: histos.load.PluginCompileError): LoadError =
    LoadError.CompileFailed(List(histos.load.CompilerMessage.fromCompiler(error.message)))

  private def publishFailed(error: PluginPublishFault): LoadError =
    LoadError.Plugin(PluginLoadError.Construction(error.toString))

  private type LoadError = histos.load.LoadError
  private val LoadError = histos.load.LoadError
end InstalledHostProofSpec

trait InstalledReview:
  def token: String

final class InstalledReviewLive(val token: String) extends InstalledReview

final class InstalledSeen(label: InstalledReview) extends HistosPlugin:
  val id = PluginId("installed-seen")

  def contribute: Contribution = Contribution(Nil, Nil, Nil)

  def run(request: RunRequest): zio.ZIO[zio.Scope & PluginScopes, PluginRunError, PluginOutput] =
    zio.ZIO.succeed(PluginOutput(label.token, Nil))

  def text: String = label.token
end InstalledSeen

object InstalledSeen:
  val layer: zio.ZLayer[InstalledReview, Nothing, InstalledSeen] =
    zio.ZLayer.fromFunction(InstalledSeen(_))
