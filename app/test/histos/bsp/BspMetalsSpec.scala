package histos.bsp

import histos.*
import histos.cache.ContentStore
import histos.jdk.{Machine, Toolchain}
import histos.load.BuildNotes
import histos.resolve.{ClasspathScope, DependencyLock, LockedCell, ResolutionService, Resolve}
import histos.schedule.RemoteCache
import histos.testrun.TestCompile
import histos.toolchain.Runtimes
import histos.workspace.{IoFailure, IoFault, Workspace}
import histos.zinc.{CompileMemo, Compilers, GraphCompile}

import java.nio.file.{Files, Path}
import java.util.zip.{ZipEntry, ZipOutputStream}

import zio.json.*
import zio.json.ast.Json
import zio.{Chunk, Ref, Scope, ZIO, ZLayer, durationInt}
import zio.test.*

object BspMetalsSpec extends ZIOSpecDefault:
  private val scala3 = ScalaVersion("3.10.1")
  private val kept   = Set("PATH", "HOME", "XDG_CACHE_HOME")

  private val decisions = suite("decisions")(
    test("editor env keeps a server variable suiteEnv drops, and an existing HISTOS_REPO wins") {
      val repo             = Path.of("/repo")
      val root             = repo.toAbsolutePath.normalize.toString
      val (directory, env) = Bsp.editorEnv(repo, Map("LANG" -> "C", "TERRET_POLICY" -> "strict"))
      val (_, keptRepo)    = Bsp.editorEnv(repo, Map("HISTOS_REPO" -> "/other"))
      val dropped          = TestCompile.suiteEnv(
        repo,
        Map(
          "TERRET_POLICY"  -> "strict",
          "PATH"           -> "/usr/bin",
          "HISTOS_JAR"     -> "/tool/histos.jar",
          "CI"             -> "true",
          "GITHUB_ACTIONS" -> "true",
        ),
      )
      val emptyCi = TestCompile.suiteEnv(repo, Map("CI" -> ""))
      assertTrue(
        directory == root,
        env.get("TERRET_POLICY").contains("strict"),
        env.get("LANG").contains("C"),
        env.get("HISTOS_REPO").contains(root),
        keptRepo.get("HISTOS_REPO").contains("/other"),
        !dropped.contains("TERRET_POLICY"),
        dropped.get("HISTOS_JAR").contains("/tool/histos.jar"),
        dropped.get("PATH").contains("/usr/bin"),
        dropped.get("CI").contains("true"),
        dropped.get("GITHUB_ACTIONS").contains("true"),
        !emptyCi.contains("CI"),
      )
    },
    test("a library cannot run and a module that names a main can") {
      val library = module("library", Jvm)(scala"3.10.1")
      val program = module("program", Jvm)(scala"3.10.1").main(MainClass("program.App"))
      (only(Projects(library)), only(Projects(program))) match
        case (Some(libraryModule), Some(programModule)) =>
          assertTrue(!Bsp.canRun(libraryModule), Bsp.canRun(programModule))
        case _ => assertTrue(false)
    },
    test("a module named twice is one compile cell") {
      val name   = ProjectName("grok")
      val first  = ScalaVersion("3.10.1")
      val second = ScalaVersion("3.10.0")
      assertTrue(
        Bsp.once(List(name -> first, name -> first)) == List(name -> first),
        Bsp.once(List(name -> first, name -> second, name -> first)) == List(name -> first, name -> second),
      )
    },
  )

  private val session = suite("session")(
    test("inverseSources maps main, test, catalog, strategies, and plugin sources") {
      ZIO.scoped {
        Workspace.work("bsp-metals").flatMap { repo =>
          val keptModule = module("kept", Jvm)(scala"3.10.1")
          val demo       = module("demo", Jvm)(scala"3.10.1")
          val plug       = module("plug", Jvm)(scala"3.10.1")
          open(repo, Projects.locked.plugins(PluginSources.module(plug))(keptModule, demo, plug), DependencyLock(Nil)) {
            (bsp, _, _) =>
              for
                _       <- write(repo.resolve("histos.scala"), "// build\n")
                _       <- write(repo.resolve("histos.plugins.scala"), "object plugins\n")
                _       <- write(repo.resolve("histos.strategies.scala"), "object strategies\n")
                _       <- write(repo.resolve("histos.wiring.scala"), "object HistosWiring\n")
                keptSrc <- write(repo.resolve("kept/src/main/scala/Kept.scala"), "class Kept\n")
                testSrc <- write(repo.resolve("demo/test/scala/DemoTest.scala"), "class DemoTest\n")
                plugSrc <- write(repo.resolve("plug/src/main/scala/Plug.scala"), "class Plug\n")
                ready   <- call(bsp, "build/initialize", Json.Obj(), 1)
                main    <- call(bsp, "buildTarget/inverseSources", document(Bsp.fileUri(keptSrc)), 2)
                tests   <- call(bsp, "buildTarget/inverseSources", document(Bsp.fileUri(testSrc)), 3)
                catalog <- call(
                  bsp,
                  "buildTarget/inverseSources",
                  document(Bsp.fileUri(repo.resolve("histos.plugins.scala"))),
                  4,
                )
                strategy <- call(
                  bsp,
                  "buildTarget/inverseSources",
                  document(Bsp.fileUri(repo.resolve("histos.strategies.scala"))),
                  5,
                )
                wiring <- call(
                  bsp,
                  "buildTarget/inverseSources",
                  document(Bsp.fileUri(repo.resolve("histos.wiring.scala"))),
                  6,
                )
                plugin  <- call(bsp, "buildTarget/inverseSources", document(Bsp.fileUri(plugSrc)), 7)
                missing <- call(
                  bsp,
                  "buildTarget/inverseSources",
                  document(Bsp.fileUri(repo.resolve("missing.scala"))),
                  8,
                )
              yield
                val capability  = parsed[Rpc.InitializeResult](ready).map(_.capabilities.inverseSourcesProvider)
                val mainIds     = parsed[Rpc.InverseSourcesResult](main).map(_.targets.map(_.uri))
                val testIds     = parsed[Rpc.InverseSourcesResult](tests).map(_.targets.map(_.uri))
                val catalogIds  = parsed[Rpc.InverseSourcesResult](catalog).map(_.targets.map(_.uri))
                val strategyIds = parsed[Rpc.InverseSourcesResult](strategy).map(_.targets.map(_.uri))
                val wiringIds   = parsed[Rpc.InverseSourcesResult](wiring).map(_.targets)
                val pluginIds   = parsed[Rpc.InverseSourcesResult](plugin).map(_.targets.map(_.uri).toSet)
                val missingIds  = parsed[Rpc.InverseSourcesResult](missing).map(_.targets)
                val plugMain    = Bsp.mainUri(repo, "plug", scala3, Platform.Jvm)
                val buildUri    = Bsp.buildUri(repo)
                assertTrue(
                  capability == Right(true),
                  mainIds == Right(List(Bsp.mainUri(repo, "kept", scala3, Platform.Jvm))),
                  testIds == Right(List(Bsp.testUri(repo, "demo", scala3, Platform.Jvm))),
                  catalogIds == Right(List(buildUri)),
                  strategyIds == Right(List(buildUri)),
                  wiringIds == Right(Nil),
                  pluginIds == Right(Set(plugMain, buildUri)),
                  missingIds == Right(Nil),
                )
          }
        }
      }
    },
    test("a comment that names HistosBuild stays a build source") {
      ZIO.scoped {
        Workspace.work("bsp-metals-host").flatMap { repo =>
          val plug = module("plug", Jvm)(scala"3.10.1")
          for
            host   <- hostJar(repo.resolve("host.jar"))
            result <- open(
              repo,
              Projects.locked.plugins(PluginSources.module(plug))(plug),
              DependencyLock(Nil),
              List(host),
            ) { (bsp, _, _) =>
              for
                _       <- write(repo.resolve("histos.scala"), "// build\n")
                _       <- write(repo.resolve("histos.plugins.scala"), "object plugins\n")
                plugSrc <- write(
                  repo.resolve("plug/src/main/scala/Plug.scala"),
                  "class Plug\n// trait HistosBuild\n",
                )
                _      <- call(bsp, "build/initialize", Json.Obj(), 1)
                plugin <- call(bsp, "buildTarget/inverseSources", document(Bsp.fileUri(plugSrc)), 2)
              yield
                val pluginIds = parsed[Rpc.InverseSourcesResult](plugin).map(_.targets.map(_.uri).toSet)
                val plugMain  = Bsp.mainUri(repo, "plug", scala3, Platform.Jvm)
                val buildUri  = Bsp.buildUri(repo)
                assertTrue(pluginIds == Right(Set(plugMain, buildUri)))
            }
          yield result
        }
      }
    },
    test("a missing classes directory still returns the build file main") {
      ZIO.scoped {
        Workspace.work("bsp-metals").flatMap { repo =>
          val library = module("library", Jvm)(scala"3.10.1")
          val program = module("program", Jvm)(scala"3.10.1").main(MainClass("program.App"))
          open(repo, Projects(library, program), DependencyLock(Nil)) { (bsp, _, _) =>
            for
              _    <- call(bsp, "build/initialize", Json.Obj(), 1)
              line <- call(
                bsp,
                "buildTarget/scalaMainClasses",
                targets(List(mainOf(repo, "library"), mainOf(repo, "program"))),
                2,
              )
              refused <- call(bsp, "buildTarget/jvmRunEnvironment", targets(List(Bsp.buildUri(repo))), 3)
              notTest <- call(bsp, "buildTarget/jvmTestEnvironment", targets(List(mainOf(repo, "program"))), 4)
            yield
              val items = parsed[Rpc.ScalaMainClassesResult](line)
                .map(_.items.map(item => item.target.uri -> item.classes.map(_.className)))
              assertTrue(
                items == Right(
                  List(
                    mainOf(repo, "library") -> Nil,
                    mainOf(repo, "program") -> List("program.App"),
                  )
                ),
                refused.contains("jvmRunEnvironment needs a main target"),
                notTest.contains("jvmTestEnvironment needs a test target"),
              )
          }
        }
      }
    },
    test("the build target reports the build file classpath") {
      ZIO.scoped {
        Workspace.work("bsp-metals").flatMap { repo =>
          val plugin = repo.resolve("plugins").resolve("specular.jar")
          val app    = module("app", Jvm)(scala"3.10.1")
          open(repo, Projects(app), DependencyLock(Nil), List(plugin)) { (bsp, _, _) =>
            for
              _    <- call(bsp, "build/initialize", Json.Obj(), 1)
              line <- call(bsp, "buildTarget/scalacOptions", targets(List(Bsp.buildUri(repo))), 2)
            yield
              val classpath = parsed[Rpc.ScalacOptionsResult](line).map(_.items.flatMap(_.classpath))
              assertTrue(classpath == Right(List(Bsp.fileUri(plugin))))
          }
        }
      }
    },
    test(
      "cold scoped Test import includes fixture Main/Test paths and scoped or emitted-only targets without compilation"
    ) {
      ZIO.scoped {
        Workspace.work("bsp-scoped-import").flatMap { repo =>
          import histos.plugin.*
          val fixture  = module("fixture", Jvm)(scala3)
          val consumer = module("consumer", Jvm)(scala3).testDependsOn(fixture)
          val scoped   = module("scoped", Jvm)(scala3).roots(
            ScopedRoot(RepoPath("scoped-exact"), SourceSet.Test, PlatformSelection.only(Jvm), RootKind.Sources)
          )
          val emitted  = module("emitted", Jvm)(scala3)
          val producer = new HistosPlugin:
            val id         = PluginId("bsp-emitted")
            def contribute = Contribution(
              List(
                DeclaredTask(
                  TaskName("generate"),
                  Seal.Hermetic,
                  Set.empty,
                  Nil,
                  List(DeclaredOutput.File(OutputName("source"), RepoPath("generated/Fixture.scala"))),
                  List(
                    TaskDep.EmitScoped(
                      ProjectName("emitted"),
                      SourceSet.Test,
                      PlatformSelection.only(Jvm),
                      EmittedContent.SourceFile(ArtifactPath("Fixture.scala")),
                      OutputName("source"),
                    )
                  ),
                )
              ),
              Nil,
              Nil,
            )
            def run(request: RunRequest): ZIO[Scope & PluginScopes, PluginRunError, PluginOutput] =
              ZIO.fail(PluginRunError.Failed(PluginDetail("metadata must not invoke producers")))
          val built = Projects(fixture, consumer, scoped, emitted).map(
            _.withPlugins(
              List(RegisteredPlugin(producer.id, producer.contribute)(producer))
            )
          )
          compilerLock(built).flatMap { lock =>
            open(repo, built, lock) { (bsp, _, _) =>
              for
                _       <- write(repo.resolve("fixture/src/FixtureMain.scala"), "class FixtureMain\n")
                _       <- write(repo.resolve("fixture/test/Fixture.scala"), "class Fixture extends FixtureMain\n")
                _       <- write(repo.resolve("consumer/test/Consumer.scala"), "class Consumer extends Fixture\n")
                _       <- write(repo.resolve("scoped-exact/Only.scala"), "class Only\n")
                _       <- call(bsp, "build/initialize", Json.Obj(), 1)
                options <- call(bsp, "buildTarget/scalacOptions", targetsOf(testOf(repo, "consumer")), 2)
                listed  <- call(bsp, "workspace/buildTargets", Json.Obj(), 3)
                sources <- call(bsp, "buildTarget/sources", targetsOf(testOf(repo, "scoped")), 4)
                cold    <- ZIO
                  .attemptBlocking(
                    Files.notExists(repo.resolve(".histos/classes")) &&
                      Files.notExists(repo.resolve(".histos/test-classes")) && Files
                        .notExists(repo.resolve("generated"))
                  )
                  .mapError(_ => IoFailure(repo, IoFault.Stat))
                expected = List(
                  GraphCompile.classesDir(repo, ProjectName("consumer"), scala3, Jvm),
                  GraphCompile.classesDir(repo, ProjectName("fixture"), scala3, Jvm),
                  TestCompile.testClasses(repo, ProjectName("fixture"), scala3, Jvm),
                ).map(Bsp.fileUri)
                compiled        <- call(bsp, "buildTarget/compile", targetsOf(testOf(repo, "consumer")), 5)
                fixtureProducts <- ZIO
                  .attemptBlocking(
                    Files.exists(
                      GraphCompile.classesDir(repo, ProjectName("fixture"), scala3, Jvm).resolve("FixtureMain.class")
                    ) &&
                      Files.exists(
                        TestCompile.testClasses(repo, ProjectName("fixture"), scala3, Jvm).resolve("Fixture.class")
                      ) &&
                      Files.exists(
                        TestCompile.testClasses(repo, ProjectName("consumer"), scala3, Jvm).resolve("Consumer.class")
                      )
                  )
                  .mapError(_ => IoFailure(repo, IoFault.Stat))
              yield
                val paths   = parsed[Rpc.ScalacOptionsResult](options).map(_.items.flatMap(_.classpath))
                val targets = parsed[Rpc.BuildTargetsResult](listed).map(_.targets)
                assertTrue(
                  paths.exists(found => expected.forall(found.contains)),
                  targets.exists(_.exists(_.id.uri == testOf(repo, "scoped"))),
                  targets.exists(_.exists(_.id.uri == testOf(repo, "emitted"))),
                  targets.exists(
                    _.find(_.id.uri == testOf(repo, "consumer")).exists(target =>
                      target.dependencies.map(_.uri).toSet == Set(mainOf(repo, "consumer"), testOf(repo, "fixture"))
                    )
                  ),
                  sources.contains(Bsp.fileUri(repo.resolve("scoped-exact/Only.scala"))),
                  cold,
                  parsed[Rpc.CompileResult](compiled).map(_.statusCode) == Right(1),
                  fixtureProducts,
                )
            }
          }
        }
      }
    },
    test("one drifted scalacOptions target does not drop the other") {
      ZIO.scoped {
        Workspace.work("bsp-metals").flatMap { repo =>
          val drift = module("drift", Jvm)(scala"3.10.1")
          open(repo, Projects.locked(drift), DependencyLock(Nil)) { (bsp, _, notes) =>
            for
              _    <- write(repo.resolve("histos.scala"), "// build\n")
              _    <- call(bsp, "build/initialize", Json.Obj(), 1)
              line <- call(
                bsp,
                "buildTarget/scalacOptions",
                targets(List(Bsp.buildUri(repo), mainOf(repo, "drift"))),
                2,
              )
              seen <- notes.get
            yield
              val items          = parsed[Rpc.ScalacOptionsResult](line)
              val ids            = items.map(_.items.map(_.target.uri))
              val driftClasspath = items.map: result =>
                result.items.find(_.target.uri == mainOf(repo, "drift")) match
                  case Some(item) => item.classpath
                  case None       => Nil
              val text = seen.mkString("\n")
              assertTrue(
                ids.map(_.contains(Bsp.buildUri(repo))) == Right(true),
                driftClasspath == Right(Nil),
                text.contains("histos resolve --update"),
                text.contains("drift"),
                text.contains(Bsp.fileUri(repo.resolve("histos.scala"))),
              )
          }
        }
      }
    },
    test("one drifted test target returns no suites and the other target still does") {
      ZIO.scoped {
        Workspace.work("bsp-metals").flatMap { repo =>
          val named = module("named", Jvm)(scala"3.10.1").test(TestMain("named.Suite"))
          val drift = module("drift", Jvm)(scala"3.10.1").testDependencies(dep"dev.zio::zio-test-sbt:2.1.22")
          open(repo, Projects.locked(named, drift), DependencyLock(Nil)) { (bsp, _, notes) =>
            for
              _    <- write(repo.resolve("histos.scala"), "// build\n")
              _    <- call(bsp, "build/initialize", Json.Obj(), 1)
              line <- call(
                bsp,
                "buildTarget/scalaTestClasses",
                targets(List(testOf(repo, "named"), testOf(repo, "drift"))),
                2,
              )
              seen <- notes.get
            yield
              val items = parsed[Rpc.ScalaTestClassesResult](line).map: result =>
                result.items.map(item => item.target.uri -> item.classes)
              val text = seen.mkString("\n")
              assertTrue(
                items == Right(
                  List(
                    testOf(repo, "named") -> List("named.Suite"),
                    testOf(repo, "drift") -> Nil,
                  )
                ),
                text.contains("histos resolve --update"),
                text.contains("drift"),
              )
          }
        }
      }
    },
    test("run and test environments use the server map and the repo root, and a test compile sees main") {
      ZIO.scoped {
        Workspace.work("bsp-metals").flatMap { repo =>
          val library = module("library", Jvm)(scala"3.10.1")
          val demo    = module("demo", Jvm)(scala"3.10.1").main(MainClass("demo.App")).test(TestMain("demo.UseMarker"))
          val built   = Projects.locked(library, demo)
          compilerLock(built).flatMap { lock =>
            open(repo, built, lock) { (bsp, runtimes, notes) =>
              val libraryMain = mainOf(repo, "library")
              val demoMain    = mainOf(repo, "demo")
              val demoTest    = testOf(repo, "demo")
              for
                _ <- write(repo.resolve("build.properties"), "histos.version=0.0.1\n")
                _ <- write(repo.resolve("demo/src/main/scala/demo/Marker.scala"), "package demo\n\nclass Marker\n")
                _ <- write(
                  repo.resolve("demo/src/test/scala/demo/UseMarker.scala"),
                  "package demo\n\nclass UseMarker:\n  def take(marker: Marker): Marker = marker\n",
                )
                _        <- call(bsp, "build/initialize", Json.Obj(), 1)
                targets  <- call(bsp, "workspace/buildTargets", Json.Obj(), 2)
                runLine  <- call(bsp, "buildTarget/jvmRunEnvironment", targetsOf(libraryMain), 3)
                testLine <- call(bsp, "buildTarget/jvmTestEnvironment", targetsOf(demoTest), 4)
                compiled <- call(bsp, "buildTarget/compile", targetsOf(demoTest), 5)
                repeated <- call(bsp, "buildTarget/compile", targetsOf(demoMain, demoMain), 6)
                seen     <- notes.get
                marker   <- ZIO.attemptBlocking(Files.isRegularFile(markerClass(repo))).orDie
                use      <- ZIO.attemptBlocking(Files.isRegularFile(useClass(repo))).orDie
              yield
                val (directory, variables) = Bsp.editorEnv(repo, runtimes.env)
                val classes                = GraphCompile.classesDir(repo, ProjectName("library"), scala3, Platform.Jvm)
                val testClasses            = TestCompile.testClasses(repo, ProjectName("demo"), scala3, Platform.Jvm)
                val runItem                = parsed[Rpc.JvmEnvironmentResult](runLine).flatMap(_.items match
                  case item :: Nil => Right(item)
                  case other       => Left(s"run items: $other"))
                val testItem = parsed[Rpc.JvmEnvironmentResult](testLine).flatMap(_.items match
                  case item :: Nil => Right(item)
                  case other       => Left(s"test items: $other"))
                val caps = parsed[Rpc.BuildTargetsResult](targets).map: result =>
                  result.targets.map(item => item.displayName -> (item.capabilities.canRun, item.capabilities.canTest))
                val extra = runtimes.env.keySet -- kept
                val text  = seen.mkString("\n")
                assertTrue(
                  caps.map(_.contains("library" -> (false, false))) == Right(true),
                  caps.map(_.contains("demo" -> (true, false))) == Right(true),
                  caps.map(_.contains("demo-test" -> (false, true))) == Right(true),
                  runItem.map(_.workingDirectory) == Right(directory),
                  runItem.map(_.environmentVariables) == Right(variables),
                  runItem.map(_.jvmOptions.isEmpty) == Right(true),
                  runItem.map(_.jvmOptions.exists(flag => flag.contains("jdwp") || flag.contains("agentlib"))) == Right(
                    false
                  ),
                  runItem.map(_.classpath).map(startsWith(classes)) == Right(true),
                  testItem.map(_.workingDirectory) == Right(directory),
                  testItem.map(_.environmentVariables) == Right(variables),
                  testItem.map(_.jvmOptions.isEmpty) == Right(true),
                  testItem.map(_.classpath).map(startsWith(testClasses)) == Right(true),
                  extra.forall(key => variables.get(key) == runtimes.env.get(key)),
                  !directory.contains(".histos"),
                  !directory.startsWith("file:"),
                  parsed[Rpc.CompileResult](compiled).map(_.statusCode) == Right(1),
                  parsed[Rpc.CompileResult](repeated).map(_.statusCode) == Right(1),
                  !text.contains("listed twice"),
                  !compiled.contains("listed twice"),
                  !repeated.contains("listed twice"),
                  marker,
                  use,
                )
            }
          }
        }
      }
    },
  ) @@ TestAspect.withLiveClock @@ TestAspect.timeout(5.minutes)

  def spec = suite("bsp metals")(decisions, session)

  private def markerClass(repo: Path): Path =
    GraphCompile
      .classesDir(repo, ProjectName("demo"), scala3, Platform.Jvm)
      .resolve("demo")
      .resolve("Marker.class")

  private def useClass(repo: Path): Path =
    TestCompile
      .testClasses(repo, ProjectName("demo"), scala3, Platform.Jvm)
      .resolve("demo")
      .resolve("UseMarker.class")

  private def startsWith(path: Path)(classpath: List[String]): Boolean =
    classpath match
      case first :: _ =>
        _root_.scala.util
          .Try(Path.of(java.net.URI.create(first)).toAbsolutePath.normalize)
          .toOption
          .contains(path.toAbsolutePath.normalize)
      case Nil => false

  private def only(built: Either[BuildError, ProjectGraph]): Option[Module] =
    built.toOption.flatMap: graph =>
      graph.projects.collect { case item: Module => item } match
        case item :: Nil => Some(item)
        case _           => None

  private def mainOf(repo: Path, name: String): String =
    Bsp.mainUri(repo, name, scala3, Platform.Jvm)

  private def testOf(repo: Path, name: String): String =
    Bsp.testUri(repo, name, scala3, Platform.Jvm)

  private def targets(uris: List[String]): Json =
    Json.Obj("targets" -> Json.Arr(uris.map(uri => Json.Obj("uri" -> Json.Str(uri)))*))

  private def targetsOf(uris: String*): Json =
    targets(uris.toList)

  private def document(uri: String): Json =
    Json.Obj("textDocument" -> Json.Obj("uri" -> Json.Str(uri)))

  private def open[A](
      repo: Path,
      built: Either[BuildError, ProjectGraph],
      lock: DependencyLock,
      classpath: List[Path] = Nil,
  )(
      use: (Bsp.Session, Runtimes, Ref[Chunk[String]]) => ZIO[
        Scope & Resolve & ContentStore & CompileMemo & RemoteCache & Compilers & ResolutionService,
        Any,
        A,
      ]
  ): ZIO[Scope, Any, A] =
    ZIO.fromEither(built).mapError(_.message).flatMap { graph =>
      for
        _        <- hostFacts
        _        <- write(repo.resolve("build.properties"), "histos.version=0.0.1\n")
        notes    <- Ref.make(Chunk.empty[String])
        machine  <- Machine.load(repo)
        jdk      <- Toolchain.resolve(machine)
        runtimes <- Runtimes.read(machine, None)
        bsp      <- Bsp.open(
          repo,
          graph,
          jdk,
          runtimes,
          classpath,
          line => notes.update(_ :+ line),
          _ => ZIO.succeed(BuildNotes(false, Nil)),
          gate => gate,
          None,
        )
        result <- use(bsp, runtimes, notes).provideSome[Scope](services(repo, lock))
      yield result
    }

  private def services(repo: Path, lock: DependencyLock) =
    val base =
      Resolve.offline(coursier.cache.CacheDefaults.location.toPath) ++
        ZLayer.succeed(ContentStore.in(repo.resolve("store"))) ++
        CompileMemo.live ++
        RemoteCache.off ++
        Compilers.live
    base >+> ResolutionService.pinned(lock)

  private def call(
      bsp: Bsp.Session,
      method: String,
      params: Json,
      id: Int,
  ): ZIO[Resolve & ContentStore & CompileMemo & RemoteCache & Compilers & ResolutionService, String, String] =
    val payload = Json
      .Obj(
        "jsonrpc" -> Json.Str("2.0"),
        "id"      -> Json.Num(id),
        "method"  -> Json.Str(method),
        "params"  -> params,
      )
      .toJson
    bsp
      .answer(payload)
      .flatMap:
        case Bsp.Answer.Reply(line) => ZIO.succeed(line)
        case Bsp.Answer.Quiet       => ZIO.fail("bsp stayed quiet")
        case Bsp.Answer.Stop(code)  => ZIO.fail(s"bsp stopped $code")

  private def parsed[A: JsonCodec](line: String): Either[String, A] =
    line.fromJson[Rpc.RpcOk[A]].map(_.result).left.map(error => s"$error :: $line")

  private def hostJar(path: Path): ZIO[Any, Nothing, Path] =
    ZIO.attemptBlocking {
      Files.createDirectories(path.getParent)
      val out = new ZipOutputStream(Files.newOutputStream(path))
      try
        out.putNextEntry(new ZipEntry("histos/HistosBuild.class"))
        out.write(Array[Byte](0, 1, 2))
        out.closeEntry()
      finally out.close()
      path
    }.orDie

  private def write(path: Path, text: String): ZIO[Any, IoFailure, Path] =
    val stored = Option(path.getParent) match
      case None =>
        ZIO.fail(IoFailure(path, IoFault.Create))
      case Some(parent) =>
        Workspace.createDirectories(parent) *>
          ZIO.attemptBlocking(Files.writeString(path, text)).mapError(_ => IoFailure(path, IoFault.Write))
    stored.as(path)

  private def hostFacts: ZIO[Any, String, Unit] =
    val keys = List("user.home", "os.name", "os.arch", "java.home")
    ZIO.foreachDiscard(keys) { key =>
      Option(System.getProperty(key)) match
        case Some(value) if value.nonEmpty => TestSystem.putProperty(key, value)
        case _                             => ZIO.fail(s"the test JVM has no $key")
    } *> TestSystem.putEnv("TERRET_POLICY", "strict") *>
      TestSystem.putEnv("PATH", Option(System.getenv("PATH")).getOrElse("/usr/bin"))

  private def checkout: ZIO[Any, String, Path] =
    ZIO.attemptBlocking(Option(System.getenv("HISTOS_REPO"))).orDie.flatMap {
      case Some(value) if value.nonEmpty => ZIO.succeed(Path.of(value))
      case _                             => ZIO.fail("HISTOS_REPO is not set")
    }

  /** The compiler tool record. A solved checkout keeps it in `.histos/resolution`. A hermetic checkout keeps it in
    * `dependencies.lock`. A solved checkout does not need the lock.
    */
  private def compilerLock(graph: Either[BuildError, ProjectGraph]): ZIO[Any, String, DependencyLock] =
    checkout.flatMap { root =>
      val lock   = root.resolve("dependencies.lock")
      val report = histos.resolve.ResolutionService.reportPath(root)
      ZIO
        .attemptBlocking(if Files.isRegularFile(lock) then lock else report)
        .orDie
        .flatMap(readLock(_, graph))
    }

  private def readLock(path: Path, graph: Either[BuildError, ProjectGraph]): ZIO[Any, String, DependencyLock] =
    ZIO
      .attemptBlocking(Files.readString(path))
      .mapError(_ => s"no compiler record at $path")
      .flatMap: text =>
        val lines = text.linesIterator.toList
        val start = lines.indexWhere(_ == "tool compiler 3.10.1 jvm")
        if start < 0 then ZIO.fail("tool compiler 3.10.1 jvm is not in the lock")
        else
          val rest = lines.drop(start + 1)
          val next = rest.indexWhere(_.startsWith("tool "))
          val tool = (if next < 0 then lines.drop(start) else lines.slice(start, start + 1 + next)).mkString("\n")
          ZIO.fromEither(graph.left.map(_.message)).flatMap { built =>
            val locked = built.projects.collect:
              case module: Module =>
                LockedCell(
                  module.name,
                  module.versions.first,
                  Platform.Jvm,
                  Nil,
                  Nil,
                  Nil,
                  ClasspathScope.Compile,
                )
            val head = locked.map(DependencyLock.renderCell).mkString("\n")
            val raw  = if head.isEmpty then tool else s"$head\n$tool"
            ZIO.fromEither(DependencyLock.parse(raw).left.map(_.message))
          }
end BspMetalsSpec
