package histos.load

import histos.cache.ContentStore
import histos.jdk.Sha256Digest
import histos.lock.ExecutionKind
import histos.plugin.{
  ApiDigest,
  BootstrapError,
  PluginArgs,
  PluginId,
  PluginLoadError,
  PluginRefusal,
  PluginTaskId,
  ResolutionIntent,
  TaskName,
}
import histos.resolve.{Classpath, DependencyLock, Repository, ResolutionService, Resolve, ResolvedArtifact}
import histos.schedule.{ActionCache, RemoteCache}
import histos.toolchain.HostToolRole
import histos.workspace.{IoFailure, IoFault, Workspace}
import histos.zinc.{CompilerStarts, Compilers}
import histos.{Dep, HistosBuild, Resolution}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.util.zip.{ZipEntry, ZipOutputStream}

import scala.jdk.CollectionConverters.*

import zio.test.*
import zio.{Chunk, Ref, Scope, UIO, ZIO, ZLayer, durationInt}

/** First-load protocol: one loader, core seed, then strategies, then the remaining closure, then main. */
object PluginBootstrapSpec extends ZIOSpecDefault:
  def spec = suite("plugin bootstrap")(
    test("an empty cache compiles once and a second load does not compile again") {
      hosted { (root, starts) =>
        for
          api    <- apiJar(root.resolve("api.jar"))
          _      <- write(root.resolve("histos.plugins.scala"), emptyCatalog)
          _      <- write(root.resolve("histos.scala"), plainMain)
          _      <- prepare(root, api, silent)
          first  <- starts.get
          again  <- prepare(root, api, silent)
          second <- starts.get
        yield (first, second, again)
      }.map { ran =>
        val (first, second, loaded) = ran.value
        val sameLoader              = loaded.catalog match
          case Some(plugins) =>
            plugins.getClass.getClassLoader eq loaded.build.getClass.getClassLoader
          case None => false
        val emptyClosure = loaded.provenance match
          case ProvenanceResult.Verified(manifest) :: Nil => manifest.artifacts.isEmpty
          case _                                          => false
        assertTrue(first > 0, second == first, sameLoader, emptyClosure, loaded.graph.resolution == Resolution.Solve)
      }
    },
    test("core seed runs before custom policy, and that policy runs before the remaining solve") {
      hosted { (root, _) =>
        for
          api    <- apiJar(root.resolve("api.jar"))
          digest <- digestOf(api)
          seed   <- pluginJar(root.resolve("seed.jar"), digest, Nil)
          rest   <- pluginJar(root.resolve("rest.jar"), digest, Nil)
          log = root.resolve("order.txt")
          _    <- write(root.resolve("histos.plugins.scala"), orderedCatalog)
          _    <- write(root.resolve("histos.strategies.scala"), orderedStrategies(log))
          _    <- write(root.resolve("histos.scala"), plainMain)
          _    <- prepare(root, api, jarsOf(log, Map("seedplug" -> seed, "restplug" -> rest)), notice = append(log))
          text <- ZIO.attemptBlocking(Files.readString(log)).orDie
        yield text
      }.map(ran => assertTrue(ran.value == "seed\nplugin:seedplug\nstrategy\ndecide\nplugin:restplug\n"))
    },
    test("a frozen miss does not solve") {
      hosted { (root, _) =>
        for
          api    <- apiJar(root.resolve("api.jar"))
          seen   <- Ref.make(List.empty[String])
          _      <- write(root.resolve("histos.plugins.scala"), missingShip)
          _      <- write(root.resolve("histos.scala"), plainMain)
          exit   <- prepare(root, api, counting(seen), execution = ExecutionKind.Ci).either
          events <- seen.get
        yield (exit, events)
      }.map { ran =>
        val (exit, events) = ran.value
        assertTrue(
          exit.rejects {
            case LoadError.Plugin(PluginLoadError.Unlocked(_)) => true
            case _                                             => false
          },
          events.isEmpty,
        )
      }
    },
    test("a repository without a catalog compiles histos.scala and inherits Solve") {
      ZIO.scoped {
        for
          root     <- work("histos-p6-inherit")
          api      <- apiJar(root.resolve("api.jar"))
          boundary <- Boundary.jars(api)
          _        <- write(root.resolve("histos.scala"), plainMain)
          _        <- write(root.resolve("dependencies.lock"), "present\n")
          loaded   <- ZIO
            .serviceWithZIO[Loader](_.load(root, api))
            .provideSome[Scope](ZLayer.succeed(silent) >>> Loader.live)
        yield assertTrue(
          loaded.catalog.isEmpty,
          loaded.graph.resolution == Resolution.Solve,
          loaded.graph.intent == ResolutionIntent.Inherit,
          loaded.buildFileClasspath == boundary,
        )
      }
    },
    test("an explicit main resolution that conflicts with the catalog is refused") {
      hosted { (root, _) =>
        for
          api  <- apiJar(root.resolve("api.jar"))
          _    <- write(root.resolve("histos.plugins.scala"), emptyCatalog)
          _    <- write(root.resolve("histos.scala"), lockedMain)
          exit <- prepare(root, api, silent).either
        yield exit
      }.map { ran =>
        assertTrue(
          ran.value.rejects {
            case LoadError.Bootstrap(BootstrapError.ResolutionConflict(Resolution.Solve, Resolution.Lock)) => true
            case _                                                                                         => false
          }
        )
      }
    },
    test("a catalog resolver reaches a local plugin dependency") {
      hosted { (root, _) =>
        for
          api      <- apiJar(root.resolve("api.jar"))
          boundary <- Boundary.jars(api)
          jar      <- externalJar(root.resolve("ext.jar"), boundary.filterNot(_ == api))
          seen     <- Ref.make(List.empty[String])
          _        <- write(root.resolve("histos.plugins.scala"), resolverCatalog)
          _        <- write(root.resolve("helper/src/Noted.scala"), "object Noted\n")
          _        <- write(root.resolve("histos.scala"), plainMain)
          _        <- prepare(root, api, repoResolve(jar, seen))
          urls     <- seen.get
        yield urls
      }.map(ran => assertTrue(ran.value.contains("maven:https://snapshots.example.test/maven")))
    },
    test("a resolved standard library is not a second copy beside the assembly") {
      hosted { (root, _) =>
        for
          api      <- apiJar(root.resolve("api.jar"))
          boundary <- Boundary.jars(api)
          jar      <- externalJar(root.resolve("ext.jar"), boundary.filterNot(_ == api))
          stdlib   <- stdlibJar(root.resolve("scala3-library_3-3.3.8.jar"))
          _        <- write(root.resolve("histos.plugins.scala"), resolverCatalog)
          _        <- write(root.resolve("helper/src/Noted.scala"), "object Noted\n")
          _        <- write(root.resolve("histos.scala"), plainMain)
          loaded   <- prepare(root, api, extraResolve(jar, stdlib))
          found = urls(loaded.build).map(path => Option(path.getFileName).map(_.toString).getOrElse(""))
        yield found
      }.map(ran =>
        assertTrue(ran.value.exists(_.startsWith("ext")), !ran.value.exists(_.startsWith("scala3-library_")))
      )
    },
    test("a local module and an external jar load") {
      hosted { (root, _) =>
        for
          api      <- apiJar(root.resolve("api.jar"))
          boundary <- Boundary.jars(api)
          jar      <- externalJar(root.resolve("ext.jar"), boundary.filterNot(_ == api))
          _        <- write(root.resolve("histos.plugins.scala"), moduleCatalog)
          _        <- write(root.resolve("helper/src/demo/Use.scala"), useSource)
          _        <- write(root.resolve("histos.scala"), plainMain)
          loaded   <- prepare(root, api, extResolve(jar))
          text     <- token(loaded.build, "demo.Use$")
        yield text
      }.map(ran => assertTrue(ran.value == "ext"))
    },
    test("the build file classpath keeps the plugin jars it compiled against") {
      hosted { (root, _) =>
        for
          api      <- apiJar(root.resolve("api.jar"))
          boundary <- Boundary.jars(api)
          jar      <- externalJar(root.resolve("ext.jar"), boundary.filterNot(_ == api))
          digest   <- digestOf(api)
          _        <- ZIO.attemptBlocking {
            addEntry(jar, ApiDigest.resource, digest.getBytes(StandardCharsets.UTF_8))
          }.orDie
          _      <- write(root.resolve("histos.plugins.scala"), oneShip("ext"))
          _      <- write(root.resolve("histos.scala"), extMain)
          loaded <- prepare(root, api, jarsOf(root.resolve("plugins.txt"), Map("ext" -> jar)))
          notes  <- ZIO
            .serviceWithZIO[Loader](loader =>
              loader.reportBuild(root, loaded.buildFileClasspath).zip(loader.reportBuild(root, boundary))
            )
            .provideSomeLayer[Scope](ZLayer.succeed(silent) >>> Loader.live)
        yield (loaded.buildFileClasspath, boundary, jar, notes)
      }.map { ran =>
        val (classpath, boundary, jar, (seen, missed)) = ran.value
        val seenText                                   = seen.problems.map(_.message)
        val missedText                                 = missed.problems.map(_.message)
        assertTrue(
          classpath.startsWith(boundary),
          classpath.contains(jar),
          seenText == Nil,
          missed.failed,
          missedText.exists(_.contains("Not found: demo")),
        )
      }
    },
    test("ordinary binary bytes stay pinned") {
      hosted { (root, starts) =>
        for
          api    <- apiJar(root.resolve("api.jar"))
          digest <- digestOf(api)
          direct <- pluginJar(root.resolve("direct.jar"), digest, List("direct/Pin.class" -> Array[Byte](1, 2)))
          helper <- pluginJar(root.resolve("helper.jar"), "", List("helper/Pin.class" -> Array[Byte](3, 4)))
          _      <- write(root.resolve("histos.plugins.scala"), oneShip("pinned"))
          _      <- write(root.resolve("histos.scala"), plainMain)
          loaded <- prepare(root, api, shipJars("pinned", direct, helper))
          count  <- starts.get
        yield (count, urls(loaded.build).exists(_ == helper))
      }.map { ran =>
        val (count, pinned) = ran.value
        assertTrue(count == 2, pinned)
      }
    },
    test("helper drift rebuilds when the publication carries sources") {
      hosted { (root, _) =>
        for
          api    <- apiJar(root.resolve("api.jar"))
          digest <- digestOf(api)
          other  <- digestOf(root.resolve("api.jar")).map(value =>
            if value.endsWith("00") then value.dropRight(2) + "ff" else value.dropRight(2) + "00"
          )
          direct <- pluginJar(root.resolve("direct.jar"), digest, Nil)
          helper <- pluginJar(
            root.resolve("helper.jar"),
            other,
            List(
              "META-INF/histos/plugin.json" -> """{"mode":"rebuildable"}""".getBytes(StandardCharsets.UTF_8),
              "META-INF/histos/rebuild/demo/Rebuilt.scala" -> rebuiltSource.getBytes(StandardCharsets.UTF_8),
            ),
          )
          _      <- write(root.resolve("histos.plugins.scala"), oneShip("drifted"))
          _      <- write(root.resolve("histos.scala"), plainMain)
          loaded <- prepare(root, api, shipJars("drifted", direct, helper))
          text   <- token(loaded.build, "demo.Rebuilt$")
        yield (text, urls(loaded.build).exists(_ == helper))
      }.map { ran =>
        val (text, helperKept) = ran.value
        assertTrue(text == "rebuilt", !helperKept)
      }
    },
    test("a host compiler rebuilds a drifted publication from its recipe") {
      hosted { (root, _) =>
        for
          api    <- apiJar(root.resolve("api.jar"))
          digest <- digestOf(api)
          other  <- digestOf(root.resolve("api.jar")).map(value =>
            if value.endsWith("00") then value.dropRight(2) + "ff" else value.dropRight(2) + "00"
          )
          direct <- pluginJar(root.resolve("direct.jar"), digest, Nil)
          helper <- pluginJar(
            root.resolve("helper.jar"),
            other,
            List(
              "META-INF/histos/plugin.json" -> """{"mode":"rebuildable"}""".getBytes(StandardCharsets.UTF_8),
              "META-INF/histos/rebuild/demo/Rebuilt.scala" -> rebuiltSource.getBytes(StandardCharsets.UTF_8),
            ),
          )
          payload <- PluginCompileSupport
            .payload(root.resolve("payload"), "3.10.1", PluginCompileSupport.Scala3101)
            .mapError(LoadError.HostCompiler(_))
          _      <- write(root.resolve("histos.plugins.scala"), oneShip("drifted"))
          _      <- write(root.resolve("histos.scala"), plainMain)
          loaded <- prepare(root, api, shipJars("drifted", direct, helper), payload = Some(payload))
          text   <- token(loaded.build, "demo.Rebuilt$")
        yield (text, urls(loaded.build).exists(_ == helper))
      }.map { ran =>
        val (text, helperKept) = ran.value
        assertTrue(text == "rebuilt", !helperKept)
      }
    },
    test("binary-only helper drift is refused") {
      hosted { (root, _) =>
        for
          api    <- apiJar(root.resolve("api.jar"))
          digest <- digestOf(api)
          other  <- ZIO.succeed("0123456789abcdef" * 4)
          direct <- pluginJar(root.resolve("direct.jar"), digest, Nil)
          helper <- pluginJar(
            root.resolve("helper.jar"),
            other,
            List("META-INF/histos/plugin.json" -> """{"mode":"binary-only"}""".getBytes(StandardCharsets.UTF_8)),
          )
          _    <- write(root.resolve("histos.plugins.scala"), oneShip("binary"))
          _    <- write(root.resolve("histos.scala"), plainMain)
          exit <- prepare(root, api, shipJars("binary", direct, helper)).either
        yield (exit, helper)
      }.map { ran =>
        val (exit, helper) = ran.value
        assertTrue(
          exit.rejects {
            case LoadError.Plugin(PluginLoadError.Refused(path, PluginRefusal.NotExact)) => path == helper
            case _                                                                       => false
          }
        )
      }
    },
    test("a classpath conflict names both origins") {
      hosted { (root, _) =>
        for
          api      <- apiJar(root.resolve("api.jar"))
          boundary <- Boundary.jars(api)
          jar      <- externalJar(root.resolve("ext.jar"), boundary.filterNot(_ == api))
          _        <- ZIO.attemptBlocking(addEntry(jar, "demo/Use.class", Array[Byte](9, 9, 9))).orDie
          _        <- write(root.resolve("histos.plugins.scala"), moduleCatalog)
          _        <- write(root.resolve("helper/src/demo/Use.scala"), useSource)
          _        <- write(root.resolve("histos.scala"), plainMain)
          exit     <- prepare(root, api, extResolve(jar)).either
        yield (exit, jar)
      }.map { ran =>
        val (exit, jar) = ran.value
        assertTrue(
          exit.rejects {
            case LoadError.Plugin(PluginLoadError.ClasspathConflict(binary, left, right, _, _)) =>
              binary == "demo/Use.class" && (left == jar || right == jar) && left != right
            case _ => false
          }
        )
      }
    },
    test("a duplicate export fails") {
      hosted { (root, _) =>
        for
          api      <- apiJar(root.resolve("api.jar"))
          boundary <- Boundary.jars(api)
          name     <- exportedEntry(api)
          jar      <- externalJar(root.resolve("ext.jar"), boundary.filterNot(_ == api))
          _        <- ZIO.attemptBlocking(addEntry(jar, name, Array[Byte](9, 9, 9))).orDie
          _        <- write(root.resolve("histos.plugins.scala"), moduleCatalog)
          _        <- write(root.resolve("helper/src/demo/Use.scala"), useSource)
          _        <- write(root.resolve("histos.scala"), plainMain)
          exit     <- prepare(root, api, extResolve(jar)).either
        yield (exit, name, api, jar)
      }.map { ran =>
        val (exit, name, api, jar) = ran.value
        assertTrue(
          exit.rejects {
            case LoadError.Plugin(PluginLoadError.ClasspathConflict(binary, left, right, _, _)) =>
              binary == name && ((left == api && right == jar) || (left == jar && right == api))
            case _ => false
          }
        )
      }
    },
    test("a late main withPluginLoading is refused") {
      hosted { (root, _) =>
        for
          api  <- apiJar(root.resolve("api.jar"))
          _    <- write(root.resolve("histos.plugins.scala"), emptyCatalog)
          _    <- write(root.resolve("histos.scala"), lateMain)
          exit <- prepare(root, api, silent).either
        yield exit
      }.map { ran =>
        assertTrue(ran.value == Left(LoadError.Bootstrap(BootstrapError.LateLoading)))
      }
    },
    test("a discarded candidate closes its services") {
      hosted { (root, _) =>
        for
          api <- apiJar(root.resolve("api.jar"))
          marker = root.resolve("marker.txt")
          _    <- write(root.resolve("histos.plugins.scala"), strategyCatalog)
          _    <- write(root.resolve("histos.strategies.scala"), closingStrategies(marker))
          _    <- write(root.resolve("histos.scala"), lockedMain)
          exit <- prepare(root, api, silent).either
          text <- ZIO.attemptBlocking(Files.readString(marker)).orDie
        yield (exit, text)
      }.map { ran =>
        val (exit, text) = ran.value
        assertTrue(
          text == "closed",
          exit.rejects {
            case LoadError.Bootstrap(BootstrapError.ResolutionConflict(_, _)) => true
            case _                                                            => false
          },
        )
      }
    },
    test("a generation service is alive when its task runs and closed after the last lease") {
      hosted { (root, _) =>
        for
          api <- apiJar(root.resolve("api.jar"))
          marker = root.resolve("marker.txt")
          _    <- write(root.resolve("histos.plugins.scala"), emptyCatalog)
          _    <- write(root.resolve("histos.scala"), markerMain(marker))
          seen <- ZIO.scoped {
            for
              loaded <- prepare(root, api, silent)
              _      <- hostFacts
              output <- loaded.plugins
                .run(PluginTaskId(PluginId("marker"), TaskName("show")), PluginArgs.empty)
                .provideSome[ContentStore & RemoteCache](
                  ZLayer.succeed(silent),
                  ResolutionService.pinned(DependencyLock(Nil)),
                  ZLayer.succeed(ActionCache.miss),
                )
                .mapError(fault => LoadError.Plugin(PluginLoadError.Construction(fault.message)))
            yield (output.text, loaded.build.getClass.getClassLoader)
          }
          (during, loader) = seen
          closed <- loaderClosed(loader)
          after  <- ZIO.attemptBlocking(Files.readString(marker)).orDie
        yield (during, closed, after)
      }.map { ran =>
        val (during, closed, after) = ran.value
        assertTrue(during == "open:true", closed, after == "closed")
      }
    },
    test("the spec author fixtures compile") {
      ZIO.scoped {
        for
          root     <- work("histos-p6-fixtures")
          api      <- apiJar(root.resolve("api.jar"))
          boundary <- Boundary.jars(api)
          produced <- compileSources(
            root.resolve("producer"),
            List(
              "ReviewedLocks.scala"      -> Author.locks,
              "ReviewTasks.scala"        -> Author.tasks,
              "CheckedReviewLabel.scala" -> Author.checked,
            ),
            boundary,
          )
          _ <- ZIO.foreachDiscard(Author.catalogs): (name, text) =>
            compileSources(root.resolve("catalogs").resolve(name), List(name -> text), boundary)
          _         <- compileSources(root.resolve("wired"), Author.wired, boundary :+ produced)
          directory <- ZIO.attemptBlocking(Files.isDirectory(produced)).orDie
        yield assertTrue(directory)
      }
    },
    test("a missing bridge does not solve") {
      hosted { (root, starts) =>
        for
          before <- lockBytes
          seen   <- Ref.make(0)
          api    <- apiJar(root.resolve("api.jar"))
          _      <- write(root.resolve("histos.plugins.scala"), bridgeCatalog)
          _      <- write(root.resolve("bridge/src/demo/Marker.java"), Author.marker)
          _      <- write(root.resolve("bridge/src/demo/Bridge.scala"), Author.bridge)
          _      <- write(root.resolve("histos.scala"), plainMain)
          built  <- PluginCompileSupport
            .payload(
              root.resolve("gap"),
              "3.10.1",
              PluginCompileSupport.Scala3101.filterNot(_.role == HostToolRole.Bridge),
            )
            .mapError(LoadError.HostCompiler(_))
          exit  <- prepare(root, api, countingCalls(seen), payload = Some(built)).either
          calls <- seen.get
          after <- lockBytes
          count <- starts.get
        yield (exit, calls, before.sameElements(after), count)
      }.map { ran =>
        val (exit, calls, same, count) = ran.value
        assertTrue(
          calls == 0,
          same,
          count == 0,
          exit.rejects {
            case LoadError.HostCompiler(histos.toolchain.HostCompilerError.EntryAbsent(HostToolRole.Bridge)) => true
            case _                                                                                           => false
          },
        )
      }
    },
    test("compiler B rebuilds a mixed module when the project lock has no B cell") {
      hosted { (root, _) =>
        for
          before   <- lockBytes
          text     <- lockText
          seen     <- Ref.make(0)
          api      <- apiJar(root.resolve("api.jar"))
          _        <- write(root.resolve("histos.plugins.scala"), bridgeCatalog)
          _        <- write(root.resolve("bridge/src/demo/Marker.java"), Author.marker)
          _        <- write(root.resolve("bridge/src/demo/Bridge.scala"), Author.bridge)
          _        <- write(root.resolve("histos.scala"), plainMain)
          store    <- ZIO.service[ContentStore]
          payloadA <- PluginCompileSupport
            .payload(root.resolve("a-payload"), "3.10.1", PluginCompileSupport.Scala3101)
            .mapError(LoadError.HostCompiler(_))
          sessionA <- PluginCompileSupport.session(payloadA, store).mapError(LoadError.HostCompiler(_))
          loadedA  <- prepare(root, api, countingCalls(seen), payload = Some(payloadA))
          textA    <- bridgeText(loadedA.build, sessionA)
          payloadB <- PluginCompileSupport
            .payload(root.resolve("b-payload"), "3.8.4", PluginCompileSupport.Scala384)
            .mapError(LoadError.HostCompiler(_))
          sessionB <- PluginCompileSupport.session(payloadB, store).mapError(LoadError.HostCompiler(_))
          loadedB  <- prepare(root, api, countingCalls(seen), payload = Some(payloadB))
          textB    <- bridgeText(loadedB.build, sessionB)
          calls    <- seen.get
          after    <- lockBytes
        yield (text, before.sameElements(after), calls, textA, textB)
      }.map { ran =>
        val (text, same, calls, textA, textB) = ran.value
        assertTrue(
          same,
          calls == 0,
          PluginCompileSupport.compilerCell(text, "3.10.1"),
          !PluginCompileSupport.compilerCell(text, "3.8.4"),
          !text.contains("scala3-compiler_3 3.8.4"),
          textA == "from-java",
          textB == "from-java",
        )
      }
    },
    test("the reviewed locks spec compiles") {
      ZIO.scoped {
        for
          root     <- work("histos-reviewed-spec")
          api      <- apiJar(root.resolve("api.jar"))
          boundary <- Boundary.jars(api)
          zioTest  <- ZIO
            .fromOption(Boundary.codeSource(classOf[zio.test.ZIOSpecDefault]))
            .orElseFail(LoadError.Io(IoFailure(Path.of("zio-test"), IoFault.Read)))
          locks    <- repoText("examples/reviewed-locks/src/com/example/reviewed/ReviewedLocks.scala")
          spec     <- repoText("examples/reviewed-locks/test/com/example/reviewed/ReviewedLocksSpec.scala")
          harness  <- repoText("plugin-testkit/src/histos/plugin/testkit/LockHarness.scala")
          repair   <- repoText("examples/expected-lock-repair.txt")
          produced <- compileSources(
            root.resolve("spec"),
            List(
              "ReviewedLocks.scala"     -> locks,
              "LockHarness.scala"       -> harness,
              "ReviewedLocksSpec.scala" -> spec,
            ),
            boundary :+ zioTest,
          )
          directory <- ZIO.attemptBlocking(Files.isDirectory(produced)).orDie
        yield assertTrue(
          directory,
          repair.startsWith("Lock repair required:"),
          repair.contains("Accept this proposal? [y/N]"),
          repair.endsWith("Resuming: histos compile app\n"),
        )
      }
    },
  ) @@ TestAspect.withLiveClock @@ TestAspect.timeout(10.minutes)

  private val emptyCatalog: String =
    """|import histos.*
       |import histos.plugin.*
       |
       |object plugins extends HistosPlugins:
       |  def sources = PluginCatalog.empty
       |""".stripMargin

  private val extMain: String =
    """|import histos.*
       |import demo.Ext
       |
       |object build extends HistosBuild:
       |  def graph =
       |    val app = module("app", Jvm)(scala"3.10.1")
       |    Ext.n match
       |      case _ => Projects(app)
       |""".stripMargin

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

  private val lateMain: String =
    """|import histos.*
       |import histos.plugin.*
       |
       |object build extends HistosBuild:
       |  def graph =
       |    val app = module("app", Jvm)(scala"3.10.1")
       |    Projects(app)
       |
       |  override def services =
       |    BuildServices.default.withPluginLoading(PluginLoading.exactOrRebuild)
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

  private val missingShip: String =
    """|import histos.*
       |import histos.plugin.*
       |
       |object plugins extends HistosPlugins:
       |  def sources = PluginCatalog(PluginSources.ship(dep"com.example::missing:1.0.0"))
       |""".stripMargin

  private val resolverCatalog: String =
    """|import histos.*
       |import histos.plugin.*
       |
       |object plugins extends HistosPlugins:
       |  val helper = module("helper", Jvm)(scala"3.10.1")
       |    .in(ProjectPath("helper"))
       |    .libraryDependencies(dep"demo:ext:1")
       |
       |  override def resolvers =
       |    PluginResolvers.Listed(
       |      List(
       |        Resolver.Maven(RepositoryUrl("https://snapshots.example.test/maven")),
       |        Resolver.Central,
       |      )
       |    )
       |
       |  def sources = PluginCatalog(PluginSources.module(helper))
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

  private val useSource: String =
    """|package demo
       |
       |object Use:
       |  def token: String = Ext.n
       |""".stripMargin

  private val rebuiltSource: String =
    """|package demo
       |
       |object Rebuilt:
       |  def token: String = "rebuilt"
       |""".stripMargin

  private def oneShip(name: String): String =
    s"""|import histos.*
        |import histos.plugin.*
        |
        |object plugins extends HistosPlugins:
        |  def sources = PluginCatalog(PluginSources.ship(dep"com.example::$name:1.0.0"))
        |""".stripMargin

  private val strategyCatalog: String =
    """|import histos.*
       |import histos.plugin.*
       |
       |object plugins extends HistosPlugins:
       |  override def resolution = Resolution.Solve
       |  def sources = PluginCatalog.empty
       |  override def bootstrap =
       |    BootstrapSource.File(RepoPath("histos.strategies.scala"), required = PluginCatalog.empty)
       |""".stripMargin

  private def closingStrategies(marker: Path): String =
    s"""|import histos.lock.*
        |import histos.plugin.*
        |import zio.{ZIO, ZLayer}
        |
        |final class Hold extends LockStrategy:
        |  def decide(context: LockContext) = ZIO.succeed(LockDecision.OfferRepair)
        |
        |object Hold:
        |  def layer: ZLayer[Any, Nothing, LockStrategy] =
        |    ZLayer.scoped(
        |      ZIO.acquireRelease(
        |        ZIO.attemptBlocking {
        |          java.nio.file.Files.writeString(java.nio.file.Path.of("${marker.toString}"), "open")
        |          new Hold
        |        }.orDie
        |      )(_ =>
        |        ZIO.attemptBlocking {
        |          java.nio.file.Files.writeString(java.nio.file.Path.of("${marker.toString}"), "closed")
        |          ()
        |        }.orDie
        |      )
        |    )
        |
        |object strategies extends HistosStrategies:
        |  def services = StrategyServices.default.withLocks(Hold.layer)
        |""".stripMargin

  private val bridgeCatalog: String =
    """|import histos.*
       |import histos.plugin.*
       |
       |object plugins extends HistosPlugins:
       |  val bridge = module("bridge", Jvm)(scala"3.10.1").in(ProjectPath("bridge"))
       |  def sources = PluginCatalog(PluginSources.module(bridge))
       |""".stripMargin

  private def markerMain(marker: Path): String =
    s"""|import histos.*
        |import histos.plugin.*
        |import zio.{ZIO, ZLayer}
        |
        |final class MarkerTask extends HistosPlugin:
        |  val id = PluginId("marker")
        |  def contribute =
        |    Contribution(
        |      List(DeclaredTask(TaskName("show"), Seal.Open, Set.empty[Need], Nil, Nil, Nil)),
        |      Nil,
        |      Nil,
        |    )
        |  def run(request: RunRequest) =
        |    ZIO
        |      .attemptBlocking {
        |        val marker = java.nio.file.Files.readString(java.nio.file.Path.of("${marker.toString}"))
        |        val home = Option(getClass.getProtectionDomain.getCodeSource).map(source =>
        |          java.nio.file.Path.of(source.getLocation.toURI)
        |        )
        |        val live = home.exists(path => java.nio.file.Files.isRegularFile(path.resolve("MarkerTask.class")))
        |        s"$$marker:$$live"
        |      }
        |      .mapBoth(_ => PluginRunError.Defect("marker"), text => PluginOutput(text, Nil))
        |
        |object MarkerTask:
        |  val layer: ZLayer[Any, Nothing, MarkerTask] =
        |    ZLayer.scoped(
        |      ZIO.acquireRelease(
        |        ZIO.attemptBlocking {
        |          java.nio.file.Files.writeString(java.nio.file.Path.of("${marker.toString}"), "open")
        |          new MarkerTask
        |        }.orDie
        |      )(_ =>
        |        ZIO.attemptBlocking {
        |          java.nio.file.Files.writeString(java.nio.file.Path.of("${marker.toString}"), "closed")
        |          ()
        |        }.orDie
        |      )
        |    )
        |
        |object build extends HistosBuild:
        |  def graph =
        |    val app = module("app", Jvm)(scala"3.10.1")
        |    Projects(app)
        |
        |  override def services = BuildServices.default.tasks[MarkerTask](MarkerTask.layer)
        |""".stripMargin

  extension [A](exit: Either[LoadError, A])
    private def rejects(accept: LoadError => Boolean): Boolean =
      exit match
        case Left(error) => accept(error)
        case Right(_)    => false

  private def hosted[A](
      body: (Path, Ref[Int]) => ZIO[BuildCache.Env & Scope, LoadError, A]
  ): ZIO[Any, LoadError, Ran[A]] =
    ZIO.scoped {
      for
        root   <- work("histos-p6-boot")
        cas    <- ZIO.attemptBlocking(Files.createDirectories(root.resolve("cas"))).orDie
        starts <- Ref.make(0)
        value  <- body(root, starts).provideSomeLayer[Scope](
          ZLayer.succeed(ContentStore.in(cas)) ++
            ZLayer.succeed(CompilerStarts(starts)) ++
            Compilers.live ++
            RemoteCache.off ++
            PluginFlights.live
        )
        count <- starts.get
      yield Ran(count, value)
    }

  private final case class Ran[A](starts: Int, value: A)

  private def prepare(
      root: Path,
      api: Path,
      resolve: Resolve,
      notice: String => UIO[Unit] = _ => ZIO.unit,
      execution: ExecutionKind = ExecutionKind.Local,
      payload: Option[Path] = None,
  ): ZIO[BuildCache.Env & Scope, LoadError, LoadedBuild] =
    BootstrapLoad.prepare(
      resolve,
      BootstrapLoad.Prepare(root, api, payload, notice, execution),
    )

  private def work(name: String): ZIO[Scope, Nothing, Path] =
    Workspace.work(name).foldZIO(failure => ZIO.dieMessage(failure.message), ZIO.succeed)

  private def write(path: Path, text: String): UIO[Path] =
    ZIO.attemptBlocking {
      Option(path.getParent).foreach(parent => Files.createDirectories(parent))
      Files.writeString(path, text, StandardCharsets.UTF_8)
      path
    }.orDie

  private def append(path: Path)(line: String): UIO[Unit] =
    ZIO.attemptBlocking {
      val prior = if Files.exists(path) then Files.readString(path) else ""
      Files.writeString(path, prior + line + "\n", StandardCharsets.UTF_8)
      ()
    }.orDie

  private def apiJar(destination: Path): UIO[Path] =
    ZIO.attemptBlocking {
      val located = Path.of(classOf[histos.HistosBuild].getProtectionDomain.getCodeSource.getLocation.toURI)
      if Files.isRegularFile(located) then located
      else
        Files.createDirectories(destination.getParent)
        val out  = new ZipOutputStream(Files.newOutputStream(destination))
        val walk = Files.walk(located)
        try
          walk.iterator.asScala.foreach { path =>
            if Files.isRegularFile(path) then
              val name = located.relativize(path).toString.replace('\\', '/')
              out.putNextEntry(new ZipEntry(name))
              out.write(Files.readAllBytes(path))
              out.closeEntry()
          }
        finally
          walk.close()
          out.close()
        destination
    }.orDie

  private def digestOf(path: Path): UIO[String] =
    ZIO.attemptBlocking(Sha256Digest.of(Chunk.fromArray(Files.readAllBytes(path))).value).orDie

  private def pluginJar(path: Path, version: String, extra: List[(String, Array[Byte])]): UIO[Path] =
    ZIO.attemptBlocking {
      Option(path.getParent).foreach(parent => Files.createDirectories(parent))
      val out = new ZipOutputStream(Files.newOutputStream(path))
      try
        if version.nonEmpty then writeEntry(out, ApiDigest.resource, version.getBytes(StandardCharsets.UTF_8))
        extra.foreach { (name, bytes) => writeEntry(out, name, bytes) }
        if extra.isEmpty then writeEntry(out, "marker.txt", Array[Byte](1))
      finally out.close()
      path
    }.orDie

  private def writeEntry(zip: ZipOutputStream, name: String, bytes: Array[Byte]): Unit =
    zip.putNextEntry(new ZipEntry(name))
    zip.write(bytes)
    zip.closeEntry()

  private def addEntry(path: Path, name: String, bytes: Array[Byte]): Unit =
    val existing = new java.util.zip.ZipFile(path.toFile)
    val entries  =
      try
        existing.entries.asScala.toList.map { entry =>
          val stream = existing.getInputStream(entry)
          try entry.getName -> stream.readAllBytes()
          finally stream.close()
        }
      finally existing.close()
    val out = new ZipOutputStream(Files.newOutputStream(path))
    try
      entries.foreach { (entry, body) => writeEntry(out, entry, body) }
      writeEntry(out, name, bytes)
    finally out.close()

  private def externalJar(path: Path, classpath: List[Path]): ZIO[Scope, LoadError, Path] =
    for
      source <- write(
        path.getParent.resolve("Ext.scala"),
        """|package demo
           |
           |object Ext:
           |  def n: String = "ext"
           |""".stripMargin,
      )
      classes = path.getParent.resolve("ext-classes")
      _   <- ZIO.attemptBlocking(Files.createDirectories(classes)).orDie
      _   <- BuildFile.compileSources(List(source), classpath, classes)
      jar <- ZIO.attemptBlocking {
        val zip = new ZipOutputStream(Files.newOutputStream(path))
        try
          val walk = Files.walk(classes)
          try
            walk.iterator.asScala.foreach: file =>
              if Files.isRegularFile(file) then
                val name = classes.relativize(file).toString.replace('\\', '/')
                if name.endsWith(".class") || name.endsWith(".tasty") then
                  writeEntry(zip, name, Files.readAllBytes(file))
          finally walk.close()
        finally zip.close()
        path
      }.orDie
    yield jar

  private val silent: Resolve =
    new Resolve:
      def resolveTool(request: histos.resolve.ToolRequest)                          = ZIO.dieMessage("resolveTool")
      def resolve(cell: histos.resolve.ResolveCell)                                 = ZIO.dieMessage("resolve")
      def fetch(artifact: histos.resolve.LockedArtifact)                            = ZIO.dieMessage("fetch")
      def resolvePlugin(dependency: Dep, repositories: histos.resolve.Repositories) =
        ZIO.dieMessage("resolvePlugin")
      def sources(organization: String, name: String, revision: String, repository: Repository) =
        ZIO.dieMessage("sources")

  private def counting(events: Ref[List[String]]): Resolve =
    new Resolve:
      def resolveTool(request: histos.resolve.ToolRequest) =
        events.update(_ :+ "resolveTool") *> ZIO.dieMessage("resolveTool")
      def resolve(cell: histos.resolve.ResolveCell) =
        events.update(_ :+ "resolve") *> ZIO.dieMessage("resolve")
      def fetch(artifact: histos.resolve.LockedArtifact) =
        events.update(_ :+ "fetch") *> ZIO.dieMessage("fetch")
      def resolvePlugin(dependency: Dep, repositories: histos.resolve.Repositories) =
        events.update(_ :+ "resolvePlugin") *> ZIO.dieMessage("resolvePlugin")
      def sources(organization: String, name: String, revision: String, repository: Repository) =
        events.update(_ :+ "sources") *> ZIO.dieMessage("sources")

  private def countingCalls(events: Ref[Int]): Resolve =
    new Resolve:
      private def tick(label: String)                      = events.update(_ + 1) *> ZIO.dieMessage(label)
      def resolveTool(request: histos.resolve.ToolRequest) = tick("resolveTool")
      def resolve(cell: histos.resolve.ResolveCell)        = tick("resolve")
      def fetch(artifact: histos.resolve.LockedArtifact)   = tick("fetch")
      def resolvePlugin(dependency: Dep, repositories: histos.resolve.Repositories)             = tick("resolvePlugin")
      def sources(organization: String, name: String, revision: String, repository: Repository) = tick("sources")

  private def jarsOf(log: Path, jars: Map[String, Path]): Resolve =
    new Resolve:
      def resolveTool(request: histos.resolve.ToolRequest)                          = ZIO.dieMessage("resolveTool")
      def resolve(cell: histos.resolve.ResolveCell)                                 = ZIO.dieMessage("resolve")
      def fetch(artifact: histos.resolve.LockedArtifact)                            = ZIO.dieMessage("fetch")
      def resolvePlugin(dependency: Dep, repositories: histos.resolve.Repositories) =
        jars.get(dependency.name) match
          case None       => ZIO.dieMessage(dependency.name)
          case Some(path) =>
            append(log)(s"plugin:${dependency.name}") *>
              ZIO.succeed(Classpath(List(artifact(dependency, path)), Nil))
      def sources(organization: String, name: String, revision: String, repository: Repository) =
        ZIO.dieMessage("sources")

  private def shipJars(name: String, direct: Path, helper: Path): Resolve =
    new Resolve:
      def resolveTool(request: histos.resolve.ToolRequest)                          = ZIO.dieMessage("resolveTool")
      def resolve(cell: histos.resolve.ResolveCell)                                 = ZIO.dieMessage("resolve")
      def fetch(artifact: histos.resolve.LockedArtifact)                            = ZIO.dieMessage("fetch")
      def resolvePlugin(dependency: Dep, repositories: histos.resolve.Repositories) =
        if dependency.name == name then
          val own   = artifact(dependency, direct)
          val extra = own.copy(name = dependency.name + "-helper", path = helper)
          ZIO.succeed(Classpath(List(own, extra), Nil))
        else ZIO.dieMessage(dependency.name)
      def sources(organization: String, name: String, revision: String, repository: Repository) =
        ZIO.dieMessage("sources")

  private def repoResolve(path: Path, seen: Ref[List[String]]): Resolve =
    new Resolve:
      def resolveTool(request: histos.resolve.ToolRequest) = ZIO.dieMessage("resolveTool")
      def resolve(cell: histos.resolve.ResolveCell)        =
        val urls = cell.repositories.all.map(Repository.token)
        seen.update(_ ++ urls) *>
          (cell.dependencies match
            case dependency :: _ => ZIO.succeed(Classpath(List(artifact(dependency, path)), Nil))
            case Nil             => ZIO.dieMessage("resolve"))
      def fetch(artifact: histos.resolve.LockedArtifact)                            = ZIO.dieMessage("fetch")
      def resolvePlugin(dependency: Dep, repositories: histos.resolve.Repositories) =
        ZIO.dieMessage("resolvePlugin")
      def sources(organization: String, name: String, revision: String, repository: Repository) =
        ZIO.dieMessage("sources")

  private def extraResolve(path: Path, extra: Path): Resolve =
    new Resolve:
      def resolveTool(request: histos.resolve.ToolRequest) = ZIO.dieMessage("resolveTool")
      def resolve(cell: histos.resolve.ResolveCell)        =
        cell.dependencies match
          case dependency :: _ =>
            ZIO.succeed(Classpath(List(artifact(dependency, path), artifact(dependency, extra)), Nil))
          case Nil =>
            ZIO.dieMessage("resolve")
      def fetch(artifact: histos.resolve.LockedArtifact)                            = ZIO.dieMessage("fetch")
      def resolvePlugin(dependency: Dep, repositories: histos.resolve.Repositories) =
        ZIO.dieMessage("resolvePlugin")
      def sources(organization: String, name: String, revision: String, repository: Repository) =
        ZIO.dieMessage("sources")

  private def exportedEntry(api: Path): UIO[String] =
    ZIO
      .attemptBlocking {
        val zip = new java.util.zip.ZipFile(api.toFile)
        try
          zip.entries.asScala
            .map(_.getName)
            .find(name => name.startsWith("histos/") && name.endsWith(".class"))
        finally zip.close()
      }
      .orDie
      .flatMap {
        case Some(name) => ZIO.succeed(name)
        case None       => ZIO.dieMessage("api jar exports no histos class")
      }

  private def stdlibJar(path: Path): UIO[Path] =
    ZIO.attemptBlocking {
      Option(path.getParent).foreach(parent => Files.createDirectories(parent))
      val out = new ZipOutputStream(Files.newOutputStream(path))
      try writeEntry(out, "histos/HistosBuild.class", Array[Byte](9, 9, 9))
      finally out.close()
      path
    }.orDie

  private def extResolve(path: Path): Resolve =
    new Resolve:
      def resolveTool(request: histos.resolve.ToolRequest) = ZIO.dieMessage("resolveTool")
      def resolve(cell: histos.resolve.ResolveCell)        =
        cell.dependencies match
          case dependency :: _ =>
            ZIO.succeed(Classpath(List(artifact(dependency, path)), Nil))
          case Nil =>
            ZIO.dieMessage("resolve")
      def fetch(artifact: histos.resolve.LockedArtifact)                            = ZIO.dieMessage("fetch")
      def resolvePlugin(dependency: Dep, repositories: histos.resolve.Repositories) =
        ZIO.dieMessage("resolvePlugin")
      def sources(organization: String, name: String, revision: String, repository: Repository) =
        ZIO.dieMessage("sources")

  private def artifact(dependency: Dep, path: Path): ResolvedArtifact =
    ResolvedArtifact(
      dependency.organization,
      dependency.name,
      dependency.revision.value,
      path,
      Sha256Digest.of(Chunk.fromArray(path.toString.getBytes(StandardCharsets.UTF_8))),
      Repository.Central,
    )

  private def token(build: HistosBuild, binary: String): ZIO[Any, LoadError, String] =
    ZIO
      .attemptBlocking {
        val cls      = build.getClass.getClassLoader.loadClass(binary)
        val instance = cls.getField("MODULE$").get(cls)
        cls.getMethod("token").invoke(instance) match
          case text: String => text
          case _            => ""
      }
      .mapError(_ => LoadError.Io(IoFailure(Path.of(binary), IoFault.Read)))

  private def urls(build: HistosBuild): List[Path] =
    build.getClass.getClassLoader match
      case loader: BuildLoader =>
        loader.getURLs.toList.flatMap(url => scala.util.Try(Path.of(url.toURI)).toOption)
      case _ => Nil

  private def bridgeText(
      build: HistosBuild,
      session: histos.toolchain.HostCompilerSession,
  ): ZIO[Scope, LoadError, String] =
    urls(build).find(path => Files.isRegularFile(path.resolve("demo/Bridge.class"))) match
      case None =>
        ZIO.fail(LoadError.Io(IoFailure(Path.of("demo/Bridge.class"), IoFault.Stat)))
      case Some(classes) =>
        PluginCompileSupport
          .invoke(classes, PluginCompileSupport.runtimeJars(session), "demo.Bridge$", "text")
          .mapError(error => LoadError.CompileFailed(List(CompilerMessage.fromCompiler(error.message))))

  private def hostFacts: UIO[Unit] =
    ZIO.foreachDiscard(List("user.home", "os.name", "os.arch", "java.home")): key =>
      Option(System.getProperty(key)) match
        case Some(value) if value.nonEmpty => TestSystem.putProperty(key, value)
        case _                             => ZIO.dieMessage(s"the test JVM has no $key")

  private def loaderClosed(loader: ClassLoader): UIO[Boolean] =
    ZIO.succeed:
      loader match
        case build: BuildLoader => build.isClosed
        case _                  => false

  /** A suite's work directory is empty. `HISTOS_REPO` is the checkout. */
  private def repoText(relative: String): ZIO[Any, LoadError, String] =
    ZIO
      .attemptBlocking(Option(System.getenv("HISTOS_REPO")))
      .orDie
      .flatMap:
        case Some(root) if root.nonEmpty =>
          val path = Path.of(root).resolve(relative)
          ZIO.attemptBlocking(Files.readString(path)).mapError(_ => LoadError.Io(IoFailure(path, IoFault.Read)))
        case _ =>
          ZIO.fail(LoadError.Io(IoFailure(Path.of(relative), IoFault.Read)))

  private def compileSources(
      root: Path,
      files: List[(String, String)],
      classpath: List[Path],
  ): ZIO[Scope, LoadError, Path] =
    for
      sources <- ZIO.foreach(files) { (name, text) => write(root.resolve(name), text) }
      out     <- BuildFile.tempDir("histos-fixture")
      _       <- BuildFile.compileSources(sources, classpath, out)
    yield out

  private def lockBytes: ZIO[Any, LoadError, Array[Byte]] =
    lockPath.flatMap: path =>
      ZIO.attemptBlocking(Files.readAllBytes(path)).mapError(_ => LoadError.Io(IoFailure(path, IoFault.Read)))

  private def lockText: ZIO[Any, LoadError, String] =
    lockPath.flatMap: path =>
      ZIO.attemptBlocking(Files.readString(path)).mapError(_ => LoadError.Io(IoFailure(path, IoFault.Read)))

  private def lockPath: ZIO[Any, LoadError, Path] =
    PluginCompileSupport.lockFile match
      case Some(path) => ZIO.succeed(path)
      case None       => ZIO.fail(LoadError.Io(IoFailure(Path.of("dependencies.lock"), IoFault.Stat)))

  private object Author:
    val marker: String =
      """|package demo;
         |public final class Marker {
         |  private Marker() {}
         |  public static String token() { return "from-java"; }
         |}
         |""".stripMargin

    val bridge: String =
      """|package demo
         |object Bridge:
         |  def text: String = Marker.token()
         |""".stripMargin

    val locks: String =
      """|package com.example.reviewed
         |
         |import histos.*
         |import histos.lock.*
         |import histos.plugin.*
         |import zio.{IO, ZIO, ZLayer}
         |
         |enum ReviewMode:
         |  case Ask, Update, Local, Frozen
         |
         |final case class ReviewConfig(
         |    local: ReviewMode = ReviewMode.Ask,
         |    ci: ReviewMode = ReviewMode.Frozen,
         |)
         |
         |final class ReviewedLocks(config: ReviewConfig) extends LockStrategy:
         |  def decide(context: LockContext): IO[LockStrategyError, LockDecision] =
         |    val selected = context.execution match
         |      case ExecutionKind.Local => config.local
         |      case ExecutionKind.Ci    => config.ci
         |    ZIO.succeed:
         |      selected match
         |        case ReviewMode.Ask    => LockDecision.OfferRepair
         |        case ReviewMode.Update => LockDecision.Update
         |        case ReviewMode.Local  => LockDecision.LocalSolve
         |        case ReviewMode.Frozen => LockDecision.Frozen
         |
         |object ReviewedLocks:
         |  def layer(config: ReviewConfig): ZLayer[Any, Nothing, LockStrategy] =
         |    ZLayer.succeed[LockStrategy](ReviewedLocks(config))
         |
         |trait ReviewLabel:
         |  def value: String
         |
         |final class LiveReviewLabel(val value: String) extends ReviewLabel
         |
         |object ReviewLabel:
         |  def layer(value: String): ZLayer[Any, Nothing, ReviewLabel] =
         |    ZLayer.succeed[ReviewLabel](LiveReviewLabel(value))
         |
         |extension [P](module: ModuleDef[P])
         |  def reviewed: ModuleDef[P] =
         |    module.note(PluginId("reviewed-locks"), NoteKey("review"), NoteText("required"))
         |""".stripMargin

    val tasks: String =
      """|package com.example.reviewed
         |
         |import histos.plugin.*
         |import zio.{Scope, ZIO, ZLayer}
         |
         |final class ReviewTasks(label: ReviewLabel) extends HistosPlugin:
         |  val id = PluginId("reviewed-locks")
         |  private val describe = TaskName("describe")
         |
         |  def contribute =
         |    Contribution(
         |      tasks = List(
         |        DeclaredTask(
         |          name = describe,
         |          seal = Seal.Hermetic,
         |          needs = Set.empty,
         |          inputs = List(DeclaredInput.Text(InputKey("label"), label.value)),
         |          outputs = Nil,
         |          dependsOn = Nil,
         |        )
         |      ),
         |      commands = Nil,
         |      jobs = Nil,
         |    )
         |
         |  def run(request: RunRequest): ZIO[Scope & PluginScopes, PluginRunError, PluginOutput] =
         |    if request.id == PluginTaskId(id, describe) then
         |      ZIO.succeed(PluginOutput(label.value, Nil))
         |    else
         |      ZIO.fail(PluginRunError.Unknown(request.id))
         |
         |object ReviewTasks:
         |  val layer: ZLayer[ReviewLabel, Nothing, ReviewTasks] =
         |    ZLayer.fromFunction(ReviewTasks(_))
         |""".stripMargin

    val checked: String =
      """|package com.example.reviewed
         |
         |import histos.doctor.Noted
         |import histos.plugin.*
         |import zio.{ZIO, ZLayer}
         |
         |enum LabelSetupError extends ServiceSetupError:
         |  case Empty
         |
         |  def diagnosis: Noted = this match
         |    case Empty =>
         |      Noted.error(
         |        "review.config",
         |        "Review label is empty",
         |        "A nonempty label is required.",
         |        Noted.steps("Set the label in histos.scala."),
         |      )
         |
         |object CheckedReviewLabel:
         |  def layer(value: String): ZLayer[Any, LabelSetupError, ReviewLabel] =
         |    ZLayer.fromZIO:
         |      if value.isBlank then ZIO.fail(LabelSetupError.Empty)
         |      else ZIO.succeed[ReviewLabel](LiveReviewLabel(value))
         |""".stripMargin

    val catalogs: List[(String, String)] = List(
      "locked.scala" ->
        """|import histos.*
           |import histos.plugin.*
           |
           |object plugins extends HistosPlugins:
           |  override def resolution = Resolution.Lock
           |
           |  def sources =
           |    PluginCatalog(
           |      PluginSources.ship(dep"com.example::reviewed-locks:1.0.0")
           |    )
           |""".stripMargin,
      "advanced.scala" ->
        """|import histos.*
           |import histos.lock.*
           |import histos.plugin.*
           |
           |object plugins extends HistosPlugins:
           |  private val reviewed =
           |    PluginSources.ship(dep"com.example::reviewed-locks:1.0.0")
           |
           |  override def resolution = Resolution.Lock
           |  override def bootstrapLocks =
           |    LockDefaults(local = LockMode.Offer, ci = LockMode.Frozen)
           |
           |  def sources =
           |    PluginCatalog(reviewed, PluginSources.ship(dep"com.example::module-tools:1.0.0"))
           |
           |  override def bootstrap =
           |    BootstrapSource.File(
           |      RepoPath("histos.strategies.scala"),
           |      required = PluginCatalog(reviewed),
           |    )
           |""".stripMargin,
      "local.scala" ->
        """|import histos.*
           |import histos.plugin.*
           |
           |object plugins extends HistosPlugins:
           |  val helper = module("review-core", Jvm)(scala"3.10.1")
           |    .in(ProjectPath("build-support/review-core"))
           |  val locks = module("reviewed-locks", Jvm)(scala"3.10.1")
           |    .in(ProjectPath("build-support/reviewed-locks"))
           |    .dependsOn(helper)
           |    .libraryDependencies(dep"org.scala-lang.modules::scala-parser-combinators:2.5.0")
           |
           |  override def resolution = Resolution.Lock
           |  def sources = PluginCatalog(PluginSources.module(locks))
           |  override def bootstrap =
           |    BootstrapSource.File(
           |      RepoPath("histos.strategies.scala"),
           |      required = PluginCatalog(PluginSources.module(locks)),
           |    )
           |""".stripMargin,
      "producer.scala" ->
        """|import histos.*
           |
           |object build extends HistosBuild:
           |  def graph =
           |    val locks = module("reviewed-locks", Jvm)(scala"3.10.1")
           |      .artifact(artifact"reviewed-locks")
           |      .pluginLibrary
           |
           |    Projects.organization(organization"com.example")(
           |      ships(Ship("reviewed-locks").next(1, 0, 0)(locks)),
           |      locks,
           |    )
           |""".stripMargin,
    )

    val wired: List[(String, String)] = List(
      "strategies.scala" ->
        """|import histos.lock.*
           |import histos.plugin.*
           |import com.example.reviewed.*
           |
           |object strategies extends HistosStrategies:
           |  def services =
           |    StrategyServices.default
           |      .withLocks(ReviewedLocks.layer(ReviewConfig(local = ReviewMode.Ask)))
           |      .withPluginLoading(PluginLoading.exactOrRebuild)
           |""".stripMargin,
      "consumer.scala" ->
        """|import histos.*
           |import histos.plugin.*
           |import com.example.reviewed.*
           |
           |object build extends HistosBuild:
           |  def graph =
           |    val app = module("app", Jvm)(scala"3.10.1").reviewed
           |    Projects(app)
           |
           |  override def services =
           |    BuildServices.default
           |      .withLocks(ReviewedLocks.layer(ReviewConfig(local = ReviewMode.Ask)))
           |      .add[ReviewLabel](ReviewLabel.layer("reviewed"))
           |      .tasks[ReviewTasks](ReviewTasks.layer)
           |""".stripMargin,
    )
  end Author

end PluginBootstrapSpec
