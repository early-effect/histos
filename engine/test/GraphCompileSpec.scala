package histos.zinc

import histos.jdk.{JdkMajor, JdkVersion}
import histos.native.NativeOrigin
import histos.resolve.{Resolve, ResolveError}
import histos.workflow.CiError
import histos.{Js, Jvm, Module, ProjectPath, Projects, ScalaVersion, module}

import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import zio.{Chunk, Promise, Ref, Scope, ZIO, durationInt}
import zio.test.*

object GraphCompileSpec extends ZIOSpecDefault:
  private val jdk: ResolvedJdk =
    val home = Path.of(java.lang.System.getProperty("java.home"))
    val text = if Files.isRegularFile(home.resolve("release")) then
      Files.readString(home.resolve("release"))
    else ""
    JdkVersion.fromRelease(text) match
      case Right(version) => ResolvedJdk(home, version)
      case Left(_)        =>
        ResolvedJdk(home, JdkVersion.trusted("21.0.2", JdkMajor.trusted(21)))

  private val compile = suite("cells")(
    test("one module compiles src at the repository root"):
      ZIO.scoped:
        for
          root <- temp
          _ <- ZIO.attempt {
            write(root, "histos.scala", oneModule)
            write(
              root,
              "src/main/scala/demo/Hello.scala",
              "package demo\n\nobject Hello:\n  def greet: String = \"hi\"\n"
            )
            write(root, ".histos/dependencies.lock", "cell core 3.9.0 jvm\n")
          }
          built <- ZIO
            .fromEither(Projects(module("core", Jvm)(ScalaVersion("3.9.0"))))
          cells <- GraphCompile
            .run(root, built, Nil, jdk, NativeOrigin.Central)
            .provide(Resolve.live(root.resolve("coursier")))
          found <- ZIO.attempt(
            cells.map(cell =>
              (
                cell.module.value,
                cell.version.value,
                cell.classFiles,
                entries(cell.classes)
              )
            )
          )
        yield assertTrue:
          found == List(
            (
              "core",
              "3.9.0",
              2,
              List(
                "META-INF/semanticdb/src/main/scala/demo/Hello.scala.semanticdb",
                "demo/Hello$.class",
                "demo/Hello.class",
                "demo/Hello.tasty"
              )
            )
          )
    ,
    test("a scala 2.13 cell writes semanticdb"):
      ZIO.scoped:
        for
          root <- temp
          _ <- ZIO.attempt {
            write(
              root,
              "core/src/Hello.scala",
              "object Hello {\n  def greet: String = \"hi\"\n}\n"
            )
            write(root, ".histos/dependencies.lock", "cell core 2.13.16 jvm\n")
          }
          built <- ZIO
            .fromEither(Projects(module("core", Jvm)(ScalaVersion("2.13.16"))))
          cells <- GraphCompile
            .run(root, built, Nil, jdk, NativeOrigin.Central)
            .provide(Resolve.live(root.resolve("coursier")))
          found <- ZIO.attempt(cells.flatMap(cell => entries(cell.classes)))
        yield assertTrue(
          found.exists(_.endsWith("Hello.scala.semanticdb")),
          found.exists(_.endsWith("Hello.class"))
        )
    ,
    test("a named module compiles its upstream, from each module directory"):
      ZIO.scoped:
        for
          root <- temp
          built <- ZIO
            .attempt {
              val core = module("core", Jvm)(ScalaVersion("3.9.0"))
              val app =
                module("app", Jvm)(ScalaVersion("3.9.0")).dependsOn(core)
              val extra =
                module("extra", Jvm)(ScalaVersion("3.9.0")).dependsOn(app)
              write(
                root,
                "core/src/main/scala/Core.scala",
                "object Core:\n  def n: Int = 1\n"
              )
              write(
                root,
                "app/src/main/scala/App.scala",
                "object App:\n  def x: Int = Core.n\n"
              )
              write(
                root,
                "extra/src/main/scala/Extra.scala",
                "object Extra:\n  def bad = notDefined\n"
              )
              write(
                root,
                "src/main/scala/Ignored.scala",
                "object Ignored:\n  def bad = notDefined\n"
              )
              write(
                root,
                ".histos/dependencies.lock",
                "cell core 3.9.0 jvm\ncell app 3.9.0 jvm\n"
              )
              Projects(core, app, extra)
            }
            .flatMap(ZIO.fromEither(_))
          cells <- GraphCompile
            .run(root, built, List("app"), jdk, NativeOrigin.Central)
            .provide(Resolve.live(root.resolve("coursier")))
          found <- ZIO.attempt(
            cells.map(cell => (cell.module.value, entries(cell.classes)))
          )
        yield assertTrue:
          found == List(
            (
              "app",
              List(
                "App$.class",
                "App.class",
                "App.tasty",
                "META-INF/semanticdb/app/src/main/scala/App.scala.semanticdb"
              )
            ),
            (
              "core",
              List(
                "Core$.class",
                "Core.class",
                "Core.tasty",
                "META-INF/semanticdb/core/src/main/scala/Core.scala.semanticdb"
              )
            )
          )
    ,
    test(
      "the console finishes the upstream cell before the module that depends on it"
    ):
      ZIO.scoped:
        for
          root <- temp
          built <- ZIO
            .attempt {
              val core = module("core", Jvm)(ScalaVersion("3.9.0"))
              val app =
                module("app", Jvm)(ScalaVersion("3.9.0")).dependsOn(core)
              write(
                root,
                "core/src/main/scala/Core.scala",
                "object Core:\n  def n: Int = 1\n"
              )
              write(
                root,
                "app/src/main/scala/App.scala",
                "object App:\n  def x: Int = Core.n\n"
              )
              write(
                root,
                ".histos/dependencies.lock",
                "cell core 3.9.0 jvm\ncell app 3.9.0 jvm\n"
              )
              Projects(core, app)
            }
            .flatMap(ZIO.fromEither(_))
          names <- GraphCompile
            .console(root, built, List("app"), jdk, NativeOrigin.Central)
            .collect {
              case CompileFeed.Began(module, _, _) =>
                s"compiling ${module.value}"
              case CompileFeed.Done(cell) => cell.module.value
            }
            .runCollect
            .provide(Resolve.live(root.resolve("coursier")))
        yield assertTrue(
          names.toList == List("compiling core", "core", "compiling app", "app")
        )
    ,
    test("a cell starts while an unrelated upstream is still compiling"):
      ZIO.scoped:
        for
          root <- temp
          release <- Promise.make[Nothing, Unit]
          begun <- Promise.make[Nothing, Unit]
          built <- ZIO
            .attempt {
              val slow = module("slow", Jvm)(ScalaVersion("3.9.0"))
              val fast = module("fast", Jvm)(ScalaVersion("3.9.0"))
              val after = module("after", Jvm)(ScalaVersion("3.9.0")).dependsOn(fast)
              write(root, "slow/src/main/scala/Slow.scala", "object Slow:\n  def n: Int = 1\n")
              write(root, "fast/src/main/scala/Fast.scala", "object Fast:\n  def n: Int = 1\n")
              write(root, "after/src/main/scala/After.scala", "object After:\n  def n: Int = Fast.n\n")
              write(
                root,
                ".histos/dependencies.lock",
                "cell slow 3.9.0 jvm\ncell fast 3.9.0 jvm\ncell after 3.9.0 jvm\n",
              )
              Projects(slow, fast, after)
            }
            .flatMap(ZIO.fromEither(_))
          fiber <- GraphCompile
            .gated(
              root,
              built,
              Nil,
              jdk,
              NativeOrigin.Central,
              name => if name.value == "slow" then release.await else ZIO.unit,
            )
            .tap:
              case CompileFeed.Began(module, _, _) if module.value == "after" => begun.succeed(()).unit
              case _                                                          => ZIO.unit
            .runDrain
            .provide(Resolve.live(root.resolve("coursier")))
            .forkScoped
          started <- Live.live(begun.await.timeout(90.seconds))
          _ <- release.succeed(())
          _ <- fiber.join
        yield assertTrue(started.isDefined)
    ,
    test("a compile diagnostic is on the console before the cell fails"):
      ZIO.scoped:
        for
          root <- temp
          built <- ZIO
            .attempt {
              write(
                root,
                "src/Bad.scala",
                "object Bad:\n  def n: Int = missing\n"
              )
              write(root, ".histos/dependencies.lock", "cell core 3.9.0 jvm\n")
              Projects(module("core", Jvm)(ScalaVersion("3.9.0")))
            }
            .flatMap(ZIO.fromEither(_))
          seen <- Ref.make(Chunk.empty[CompileFeed])
          result <- GraphCompile
            .console(root, built, Nil, jdk, NativeOrigin.Central)
            .tap(feed => seen.update(_ :+ feed))
            .runDrain
            .either
            .provide(Resolve.live(root.resolve("coursier")))
          feeds <- seen.get
        yield
          val missingAt = feeds.indexWhere:
            case CompileFeed.Diagnostic(_, _, _, problem) =>
              problem.text.contains("missing")
            case _ => false
          val failedAt = feeds.indexWhere:
            case CompileFeed.Failed(
                  GraphCompileError
                    .Failed(module, version, platform, CompileError.Failed(_))
                ) =>
              module.value == "core" && version.value == "3.9.0" && platform == Jvm
            case _ => false
          assertTrue(result == Right(()), missingAt >= 0, failedAt > missingAt)
    ,
    test("an upstream built for another Scala version is not a classpath"):
      ZIO.scoped:
        for
          root <- temp
          built <- ZIO
            .attempt {
              val core = module("core", Jvm)(ScalaVersion("2.13.16"))
              val app =
                module("app", Jvm)(ScalaVersion("3.9.0")).dependsOn(core)
              write(
                root,
                "core/src/main/scala/Core.scala",
                "object Core {\n  def n: Int = 1\n}\n"
              )
              write(
                root,
                "app/src/main/scala/App.scala",
                "object App:\n  def x: Int = 1\n"
              )
              write(
                root,
                ".histos/dependencies.lock",
                "cell core 2.13.16 jvm\ncell app 3.9.0 jvm\n"
              )
              Projects(core, app)
            }
            .flatMap(ZIO.fromEither(_))
          out <- GraphCompile
            .run(root, built, List("app"), jdk, NativeOrigin.Central)
            .provide(Resolve.live(root.resolve("coursier")))
            .either
        yield assertTrue:
          out match
            case Left(
                  GraphCompileError.Cells(
                    GraphCompileError
                      .MissingUpstream(module, dependency, version, platform),
                    _
                  )
                ) =>
              module.value == "app" && dependency.value == "core" && version.value == "3.9.0" && platform == Jvm
            case _ => false
    ,
    test("a js-only upstream is on the js classpath and not the jvm one"):
      ZIO.scoped:
        for
          root <- temp
          built <- ZIO
            .attempt {
              val types = module("types", Jvm, Js)(ScalaVersion("3.9.0"))
              val facade =
                module("facade", Js)(ScalaVersion("3.9.0")).dependsOn(types)
              val core = module("core", Jvm, Js)(ScalaVersion("3.9.0"))
                .dependsOn(types)
                .dependsOn(Js)(facade)
              write(
                root,
                "types/src/main/scala/Types.scala",
                "object Types:\n  def n: Int = 1\n"
              )
              write(
                root,
                "facade/src/main/scala/Facade.scala",
                "object Facade:\n  def n: Int = Types.n\n"
              )
              write(
                root,
                "core/src/main/scala/Core.scala",
                "object Core:\n  def n: Int = Types.n\n"
              )
              write(
                root,
                "core/src/main/scalajs/UseFacade.scala",
                "object UseFacade:\n  def n: Int = Facade.n\n"
              )
              write(
                root,
                ".histos/dependencies.lock",
                "cell types 3.9.0 jvm\ncell types 3.9.0 js\ncell facade 3.9.0 js\ncell core 3.9.0 jvm\ncell core 3.9.0 js\n"
              )
              Projects(types, facade, core)
            }
            .flatMap(ZIO.fromEither(_))
          cells <- GraphCompile
            .run(root, built, List("core"), jdk, NativeOrigin.Central)
            .provide(Resolve.live(root.resolve("coursier")))
        yield
          val names = cells.map(cell => (cell.module.value, cell.platform))
          val useFacade = cells.collect:
            case cell if cell.module.value == "core" && cell.platform == Js =>
              entries(cell.classes)
          assertTrue(
            !names.contains(("facade", Jvm)),
            names.contains(("facade", Js)),
            names.contains(("core", Jvm)),
            names.contains(("core", Js)),
            useFacade.exists(_.exists(_.contains("UseFacade")))
          )
    ,
    test("sources can live under a directory that is not the module name"):
      ZIO.scoped:
        for
          root <- temp
          _ <- ZIO.attempt {
            write(
              root,
              "example/todo/src/main/scala/Todo.scala",
              "object Todo:\n  def n: Int = 1\n"
            )
            write(root, ".histos/dependencies.lock", "cell todo 3.9.0 jvm\n")
          }
          built <- ZIO
            .fromEither(
              Projects(
                module("todo", Jvm)(ScalaVersion("3.9.0"))
                  .in(ProjectPath("example/todo"))
              )
            )
          cells <- GraphCompile
            .run(root, built, Nil, jdk, NativeOrigin.Central)
            .provide(Resolve.live(root.resolve("coursier")))
        yield assertTrue:
          cells match
            case cell :: Nil =>
              cell.module.value == "todo" && cell.platform == Jvm && cell.classFiles > 0
            case _ => false
    ,
    test(
      "a js module compiles to sjsir, and its tests are not on that classpath"
    ):
      ZIO.scoped:
        for
          root <- temp
          _ <- ZIO.attempt {
            write(
              root,
              "ui/src/main/scala/Ui.scala",
              "object Ui:\n  def n: Int = 1\n"
            )
            write(
              root,
              "ui/src/test/scala/UiTest.scala",
              "object UiTest:\n  def missing = notDefined\n"
            )
            write(
              root,
              "ui/src/main/scalajvm/JvmOnly.scala",
              "object JvmOnly:\n  def n: Int = 1\n"
            )
            write(root, ".histos/dependencies.lock", "cell ui 3.9.0 js\n")
          }
          built <- ZIO
            .fromEither(Projects(module("ui", Js)(ScalaVersion("3.9.0"))))
          out <- GraphCompile
            .run(root, built, Nil, jdk, NativeOrigin.Central)
            .provide(Resolve.live(root.resolve("coursier")))
        yield
          val files = out match
            case cell :: Nil => entries(cell.classes)
            case _           => Nil
          assertTrue(
            out match
              case cell :: Nil => cell.platform == Js && cell.classFiles > 0
              case _           => false
            ,
            files.exists(_.endsWith(".sjsir")),
            !files.exists(_.contains("UiTest")),
            !files.exists(_.contains("JvmOnly"))
          )
  )

  private val roots = suite("source roots")(
    test(
      "a module directory wins, and the shared src is only the single-module layout"
    ):
      ZIO.scoped:
        for
          root <- temp
          _ <- ZIO.attempt {
            write(root, "core/src/place.txt", "x")
            write(root, "src/place.txt", "x")
          }
          sharedRoot <- temp
          _ <- ZIO.attempt(write(sharedRoot, "src/place.txt", "x"))
          both <- ZIO
            .fromEither(
              Projects(
                module("core", Jvm)(ScalaVersion("3.9.0")),
                module("app", Jvm)(ScalaVersion("3.9.0"))
              )
            )
          one <- ZIO
            .fromEither(Projects(module("core", Jvm)(ScalaVersion("3.9.0"))))
          coreBoth <- base(root, both, "core")
          appBoth <- base(root, both, "app")
          shared <- base(sharedRoot, one, "core")
        yield assertTrue(
          coreBoth == Some(Some(root.resolve("core").resolve("src"))),
          appBoth == Some(None),
          shared == Some(Some(sharedRoot.resolve("src")))
        )
  )

  private val refused = suite("refused before the compiler")(
    test("an unknown project is not a lock error"):
      ZIO.scoped:
        for
          root <- temp
          built <- ZIO
            .fromEither(Projects(module("core", Jvm)(ScalaVersion("3.9.0"))))
          out <- GraphCompile
            .run(root, built, List("missing"), jdk, NativeOrigin.Central)
            .provide(Resolve.live(root.resolve("coursier")))
            .either
        yield assertTrue:
          out match
            case Left(GraphCompileError.Target(CiError.Unknown(name))) =>
              name.value == "missing"
            case _ => false
    ,
    test("a missing lock is refused"):
      ZIO.scoped:
        for
          root <- temp
          built <- ZIO
            .fromEither(Projects(module("core", Jvm)(ScalaVersion("3.9.0"))))
          out <- GraphCompile
            .run(root, built, Nil, jdk, NativeOrigin.Central)
            .provide(Resolve.live(root.resolve("coursier")))
            .either
        yield assertTrue:
          out match
            case Left(GraphCompileError.Lock(ResolveError.NoLock(path))) =>
              path == root.resolve(".histos").resolve("dependencies.lock")
            case _ => false
    ,
    test("a module with no source root is refused"):
      ZIO.scoped:
        for
          root <- temp
          _ <- ZIO.attempt(
            write(root, ".histos/dependencies.lock", "cell core 3.9.0 jvm\n")
          )
          built <- ZIO
            .fromEither(Projects(module("core", Jvm)(ScalaVersion("3.9.0"))))
          out <- GraphCompile
            .run(root, built, Nil, jdk, NativeOrigin.Central)
            .provide(Resolve.live(root.resolve("coursier")))
            .either
        yield assertTrue:
          built.projects.collectFirst { case module: Module =>
            module.name
          } match
            case Some(name) =>
              out == Left(
                GraphCompileError.Cells(GraphCompileError.NoRoot(name), Nil)
              )
            case None => false
    ,
    test(
      "tests alone compile an empty main, and another platform's tests do not"
    ):
      ZIO.scoped:
        for
          root <- temp
          _ <- ZIO.attempt {
            write(
              root,
              "e2e/src/test/scala/Only.scala",
              "object Only:\n  def n: Int = missing\n"
            )
            write(
              root,
              "probe/src/test/scalajs/JsOnly.scala",
              "object JsOnly:\n  def n: Int = 1\n"
            )
            write(
              root,
              ".histos/dependencies.lock",
              "cell e2e 3.9.0 jvm\ncell probe 3.9.0 jvm\n"
            )
          }
          built <- ZIO
            .fromEither(
              Projects(
                module("e2e", Jvm)(ScalaVersion("3.9.0")),
                module("probe", Jvm)(ScalaVersion("3.9.0"))
              )
            )
          compiled <- GraphCompile
            .run(root, built, List("e2e"), jdk, NativeOrigin.Central)
            .provide(Resolve.live(root.resolve("coursier")))
          refused <- GraphCompile
            .run(root, built, List("probe"), jdk, NativeOrigin.Central)
            .provide(Resolve.live(root.resolve("coursier")))
            .either
          files <- ZIO.attempt(
            entries(root.resolve(".histos/classes/e2e/3.9.0/jvm"))
          )
        yield assertTrue(
          compiled match
            case cell :: Nil =>
              cell.module.value == "e2e" && cell.platform == Jvm && cell.classFiles == 0
            case _ => false
          ,
          files.isEmpty,
          refused match
            case Left(
                  GraphCompileError
                    .Cells(GraphCompileError.NoSources(module, _), _)
                ) =>
              module.value == "probe"
            case _ => false
        )
    ,
    test("an empty source root is refused"):
      ZIO.scoped:
        for
          root <- temp
          _ <- ZIO.attempt {
            write(root, ".histos/dependencies.lock", "cell core 3.9.0 jvm\n")
            write(root, "src/place.txt", "no scala")
          }
          built <- ZIO
            .fromEither(Projects(module("core", Jvm)(ScalaVersion("3.9.0"))))
          out <- GraphCompile
            .run(root, built, Nil, jdk, NativeOrigin.Central)
            .provide(Resolve.live(root.resolve("coursier")))
            .either
        yield assertTrue:
          out match
            case Left(
                  GraphCompileError
                    .Cells(GraphCompileError.NoSources(module, base), _)
                ) =>
              module.value == "core" && base == root.resolve("src")
            case _ => false
  )

  private def base(
      root: Path,
      graph: histos.ProjectGraph,
      name: String
  ): ZIO[Any, GraphCompileError, Option[Option[Path]]] =
    val modules = graph.projects.collect { case module: Module => module }
    ZIO.foreach(modules.find(_.name.value == name))(module =>
      GraphCompile.sourceBase(root, module, modules)
    )

  /** Classpath-root entries. A later jar reads these paths, so a nested source
    * tree is a failed compile.
    */
  private def entries(classes: Path): List[String] =
    if !Files.isDirectory(classes) then Nil
    else
      val walk = Files.walk(classes)
      try
        walk.iterator.asScala
          .filter(Files.isRegularFile(_))
          .map(path => classes.relativize(path).toString.replace('\\', '/'))
          .toList
          .sorted
      finally walk.close()

  private def write(root: Path, name: String, text: String): Unit =
    val path = root.resolve(name)
    Files.createDirectories(path.getParent)
    Files.writeString(path, text)

  private def temp: ZIO[Scope, Throwable, Path] =
    ZIO.acquireRelease(
      ZIO.attempt(Files.createTempDirectory("histos-compile"))
    ): dir =>
      ZIO.attempt {
        if Files.exists(dir) then
          val walk = Files.walk(dir)
          val paths =
            try walk.iterator.asScala.toList
            finally walk.close()
          paths
            .sortBy(_.getNameCount)
            .reverse
            .foreach(path => Files.deleteIfExists(path))
      }.ignore

  private val oneModule =
    """import histos.*
      |object build extends HistosBuild:
      |  def graph = Projects(module("core", Jvm)(scala"3.9.0"))
      |""".stripMargin

  def spec = suite("graph compile")(
    compile,
    roots,
    refused
  ) @@ TestAspect.timeout(3.minutes)
