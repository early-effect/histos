package histos

import histos.version.Ship

import zio.test.*

object GraphSpec extends ZIOSpecDefault:
  private val scala3 = ScalaVersion("3.9.0")
  private val scala2 = ScalaVersion("2.13.16")
  private val zioDep = dep"dev.zio::zio:2.1.26"

  private def summary(result: Either[BuildError, ProjectGraph]): String =
    result.fold(_.message, _.projects.map(_.name.value).mkString(","))

  private def ring(n: Int): Either[BuildError, ProjectGraph] =
    val count = if n < 2 then 2 else n
    val bases = (0 until count).toList.map(i => module(s"p$i", Jvm)(scala3))
    val linked = bases.zipWithIndex.map: (item, index) =>
      bases.lift((index + 1) % count) match
        case Some(dependency) => item.dependsOn(dependency)
        case None             => item
    Projects(linked*)

  private val graph = suite("ProjectGraph")(
    test("stores platforms, versions, and library dependencies"):
      val core = module("core", Jvm, Js, Native)(scala3, scala2)
        .libraryDependencies(zioDep)
        .clang(ClangMajor(21))
      val tests = module("tests", Jvm)(scala3).dependsOn(core)
      val built = Projects(aggregate("root", core, tests), core, tests)
      assertTrue:
        built match
          case Right(graph) =>
            val roots = graph.projects.collect { case item: Aggregate => item }
            val modules = graph.projects.collect { case item: Module => item }
            (roots, modules) match
              case (List(root), List(core, tests)) =>
                root.children.map(_.value) == List("core", "tests") &&
                graph.splits == Splits.aggregate &&
                graph.organization.isEmpty &&
                graph.resolvers.isEmpty &&
                core.platforms.all == List(Jvm, Js, Native) &&
                core.versions.all == List(scala3, scala2) &&
                core.dependencies == List(zioDep) &&
                core.dependsOn.isEmpty &&
                tests.dependsOn.map(_.name.value) == List("core") &&
                core.clang == ClangPin.Major(ClangMajor(21))
              case _ => false
          case Left(_) => false
    ,
    test("a module stores the class histos run starts"):
      val core = module("core", Jvm)(scala3).main(MainClass("demo.Check"))
      val built = Projects(core)
      assertTrue(
        built match
          case Right(graph) =>
            graph.projects.collect { case module: Module =>
              module.application.map(_.value)
            } == List(Some("demo.Check"))
          case Left(_) => false
        ,
        MainClass.from("demo.Check").exists(_.value == "demo.Check"),
        MainClass.from("") == Left(MainClassError.Empty),
        MainClass.from("1bad") == Left(MainClassError.Bad("1bad"))
      )
    ,
    test("a platform token is jvm, js, or native"):
      assertTrue(
        Platform.from("jvm") == Right(Jvm),
        Platform.from("js") == Right(Js),
        Platform.from("native") == Right(Native),
        Jvm.token == "jvm" && Js.token == "js" && Native.token == "native",
        Platform.from("JVM") == Left(PlatformError.Unknown("JVM")),
        Platform.from("scala") == Left(PlatformError.Unknown("scala")),
        PlatformError.Unknown("scala").message.contains("not a platform")
      )
    ,
    test("a module stores one test main"):
      val core = module("core", Jvm)(scala3).test(TestMain("demo.Check"))
      val built = Projects(core)
      assertTrue(
        built match
          case Right(graph) =>
            graph.projects.collect { case item: Module =>
              item.test.map(_.value)
            } == List(Some("demo.Check"))
          case Left(_) => false
        ,
        TestMain.from("demo.Check").exists(_.value == "demo.Check"),
        TestMain.from("") == Left(TestMainError.Empty),
        TestMain.from("1bad") == Left(TestMainError.Bad("1bad"))
      )
    ,
    test("a provider is stored, and a main beside a provider is refused"):
      val custom = TestProvider(
        FrameworkClass("munit.Framework"),
        "org.scalameta",
        "munit"
      )
      val named =
        Projects(module("core", Jvm)(scala3).testProvider(TestProvider.Zio))
      val mixed = module("core", Jvm)(scala3)
        .test(TestMain("demo.Check"))
        .testProvider(TestProvider.Zio)
      assertTrue(
        custom.map(_.library) == ProviderLibrary.from("org.scalameta", "munit"),
        named match
          case Right(graph) =>
            graph.projects.collect { case module: Module =>
              module.provider
            } == List(Some(TestProvider.Zio))
          case Left(_) => false
        ,
        Projects(mixed)
          .fold(_.message, _ => "")
          .contains("names a test main and a test provider"),
        FrameworkClass.from("") == Left(FrameworkClassError.Empty),
        ProviderLibrary.from("", "munit") == Left(ProviderLibraryError.Empty)
      )
    ,
    test("a js module names its environment and module kind"):
      val ui = module("ui", Js)(scala3)
        .jsEnv(JsEnv.Jsdom)
        .jsModule(JsModuleKind.ESModule)
      val firefox = module("browser", Js)(scala3).jsEnv(JsEnv.Firefox)
      val jvm = module("core", Jvm)(scala3).jsEnv(JsEnv.Node)
      val refused = module("core", Jvm)(scala3).jsModule(JsModuleKind.CommonJS)
      assertTrue(
        Projects(ui, firefox) match
          case Right(graph) =>
            graph.projects.collect { case module: Module =>
              (module.name.value, module.jsEnv, module.jsModule)
            } ==
              List(
                ("ui", JsEnv.Jsdom, JsModuleKind.ESModule),
                ("browser", JsEnv.Firefox, JsModuleKind.NoModule)
              )
          case Left(_) => false
        ,
        Projects(jvm).fold(
          _ => false,
          graph =>
            graph.projects.collect { case module: Module =>
              module.jsEnv
            } == List(JsEnv.Node)
        ),
        Projects(refused)
          .fold(_.message, _ => "")
          .contains("names a js module kind and does not target js")
      )
    ,
    test("test dependencies stay off the compile classpath"):
      val tests = dep"dev.zio::zio-test:2.1.27"
      val core = module("core", Jvm)(scala3)
        .libraryDependencies(zioDep)
        .testDependencies(tests)
      val built = Projects(core)
      assertTrue:
        built match
          case Right(graph) =>
            graph.projects.collect { case item: Module =>
              (item.dependencies, item.testDependencies)
            } == List((List(zioDep), List(tests)))
          case Left(_) => false
    ,
    test(
      "a listed repository is not implied to be central, and a repeat is refused"
    ):
      val core = module("core", Jvm)(scala3)
      val file = Projects.resolvers(maven"file:///repo")(core)
      val both = Projects.resolvers(maven"file:///repo", central)(core)
      val twice = Projects.resolvers(central, central)(core)
      assertTrue(
        file match
          case Right(graph) =>
            graph.resolvers == List(
              Resolver.Maven(RepositoryUrl("file:///repo"))
            )
          case Left(_) => false
        ,
        both match
          case Right(graph) =>
            graph.resolvers == List(
              Resolver.Maven(RepositoryUrl("file:///repo")),
              central
            )
          case Left(_) => false
        ,
        twice.fold(_.message, _ => "") == "repository central is listed twice"
      )
    ,
    test("the empty split set is the default"):
      val core = module("core", Jvm)(scala3)
      assertTrue(
        Projects(core).map(_.splits) == Right(Splits.aggregate),
        Projects(Splits.of(Axis.Module), core).map(_.splits) == Right(
          Splits.of(Axis.Module)
        ),
        Splits.of(Axis.Module, Axis.Module) == Splits.of(Axis.Module)
      )
    ,
    test("rejects a duplicate name"):
      val first = module("core", Jvm)(scala3)
      val second = module("core", Js)(scala3)
      val message = summary(Projects(first, second))
      assertTrue(message.contains("project 'core' is declared twice"))
    ,
    test("rejects a duplicate platform and a duplicate Scala version"):
      val platforms = summary(Projects(module("core", Jvm, Jvm)(scala3)))
      val versions = summary(Projects(module("core", Jvm)(scala3, scala3)))
      assertTrue(platforms.contains("jvm"), versions.contains("3.9.0"))
    ,
    test("rejects an empty or illegal name"):
      val empty = summary(Projects(module("", Jvm)(scala3)))
      val bad = summary(Projects(module("1core", Jvm)(scala3)))
      assertTrue(empty.contains("not empty"), bad.contains("1core"))
    ,
    test("rejects a self-dependency"):
      val base = module("core", Jvm)(scala3)
      assertTrue(
        summary(Projects(base.dependsOn(base))).contains("depends on itself")
      )
    ,
    test("rejects a dependency on an aggregate"):
      val core = module("core", Jvm)(scala3)
      val root = aggregate("root", core)
      val tests = module("tests", Jvm)(scala3).dependsOn(root)
      assertTrue(
        summary(Projects(root, core, tests)).contains("aggregate 'root'")
      )
    ,
    test("rejects a project that was not listed"):
      val core = module("core", Jvm)(scala3)
      val tests = module("tests", Jvm)(scala3).dependsOn(core)
      assertTrue(summary(Projects(tests)).contains("core"))
    ,
    test("rejects an aggregate that includes itself"):
      val loop = aggregate("root", module("root", Jvm)(scala3))
      assertTrue(summary(Projects(loop)).contains("includes itself"))
    ,
    test("rejects an aggregate that lists a child twice"):
      val core = module("core", Jvm)(scala3)
      assertTrue(
        summary(Projects(aggregate("root", core, core), core)).contains("twice")
      )
    ,
    test("a library coordinate can name its platforms"):
      val time = dep"io.github.cquiroz::scala-java-time:2.7.0".on(Js, Native)
      val core = module("core", Jvm, Js, Native)(scala3)
        .libraryDependencies(zioDep, time)
        .clang(ClangMajor(21))
      val built = Projects(core)
      assertTrue:
        built match
          case Right(graph) =>
            graph.projects.collect { case item: Module =>
              item.dependencies
            } match
              case List(dependencies) =>
                Dep.select(dependencies, Jvm) == List(zioDep) &&
                Dep.select(dependencies, Js) == List(zioDep, time) &&
                Dep.select(dependencies, Native) == List(zioDep, time)
              case _ => false
          case Left(_) => false
    ,
    test("an upstream edge can name its platforms"):
      val types = module("types", Jvm, Js, Native)(scala3).clang(ClangMajor(21))
      val facade = module("facade", Js)(scala3).dependsOn(types)
      val core = module("core", Jvm, Js, Native)(scala3)
        .clang(ClangMajor(21))
        .dependsOn(types)
        .dependsOn(Js)(facade)
      val built = Projects(types, facade, core)
      val recorded = built.map(_.projects.collect:
        case module: Module if module.name.value == "core" =>
          (
            module.dependsOn.map(edge => (edge.name.value, edge.platforms)),
            module.upstream(Jvm).map(_.value),
            module.upstream(Js).map(_.value),
            module.upstream(Native).map(_.value)
          ))
      assertTrue:
        recorded == Right(
          List(
            (
              List(("types", None), ("facade", Some(List(Js)))),
              List("types"),
              List("types", "facade"),
              List("types")
            )
          )
        )
    ,
    test("an unscoped edge requires every platform the depender builds"):
      val facade = module("facade", Js)(scala3)
      val core = module("core", Jvm, Js)(scala3).dependsOn(facade)
      assertTrue(
        summary(Projects(facade, core)).contains("'facade' does not build it")
      )
    ,
    test("a scoped edge cannot name a platform either side lacks"):
      val types = module("types", Jvm)(scala3)
      val facade = module("facade", Js)(scala3)
      val missingHere = summary(
        Projects(facade, module("core", Js)(scala3).dependsOn(Jvm)(facade))
      )
      val missingThere = summary(
        Projects(types, module("core", Jvm, Js)(scala3).dependsOn(Js)(types))
      )
      assertTrue(
        missingHere.contains("depends on jvm and does not build it"),
        missingThere.contains("'types' does not build it")
      )
    ,
    test("the same upstream twice is refused"):
      val types = module("types", Jvm)(scala3)
      val core =
        module("core", Jvm)(scala3).dependsOn(types).dependsOn(Js)(types)
      assertTrue(
        summary(Projects(types, core)).contains("depends on 'types' twice")
      )
    ,
    test(
      "edges on different platforms are not a cycle, and one platform still is"
    ):
      val left = module("left", Jvm, Js)(scala3)
      val right = module("right", Jvm, Js)(scala3)
      val split =
        Projects(left.dependsOn(Js)(right), right.dependsOn(Jvm)(left))
      val loop =
        summary(Projects(left.dependsOn(Js)(right), right.dependsOn(Js)(left)))
      assertTrue(split.isRight, loop.contains("cycle on js"))
    ,
    test("a module can live under a directory that is not its name"):
      val todo = module("todo", Js)(scala3).in(path"example/todo-conduit")
      val built = Projects(todo)
      val shared = summary(
        Projects(
          module("core", Jvm)(scala3),
          module("app", Jvm)(scala3).in(path"core")
        )
      )
      assertTrue(
        built match
          case Right(ProjectGraph(List(found: Module), _, _, _, _, _, _)) =>
            found.baseDirectory == "example/todo-conduit" && found.directory == Some(
              path"example/todo-conduit"
            )
          case other => summary(other) == "nested"
        ,
        shared.contains("both use directory core"),
        ProjectPath
          .from("example/todo-conduit")
          .exists(_.value == "example/todo-conduit"),
        ProjectPath.from("../secret") == Left(
          ProjectPathError.Escapes("../secret")
        ),
        ProjectPath.from("/tmp/secret") == Left(
          ProjectPathError.Escapes("/tmp/secret")
        ),
        ProjectPath.from("") == Left(ProjectPathError.Empty),
        ProjectPath.from("example//todo") == Left(
          ProjectPathError.Escapes("example//todo")
        )
      )
    ,
    test("a ring of modules is a cycle"):
      check(Gen.int(2, 6)): n =>
        val message = summary(ring(n))
        assertTrue(
          message.contains("cycle") && message.contains("p0") && message
            .contains("p1")
        )
    ,
    test("a native version belongs on a native module"):
      val native = Projects(
        module("core", Native)(scala3)
          .clang(ClangMajor(21))
          .nativeVersion(NativeVersion("0.5.13-SNAPSHOT"))
      )
      val jvm = summary(
        Projects(
          module("core", Jvm)(scala3).nativeVersion(NativeVersion("0.5.12"))
        )
      )
      assertTrue(
        native match
          case Right(graph) =>
            graph.projects.collect { case item: Module =>
              item.nativeVersion.map(_.value)
            } == List(Some("0.5.13-SNAPSHOT"))
          case Left(_) => false
        ,
        jvm.contains("does not target Native"),
        NativeVersion.from("0.5") == Left(NativeVersionError.Bad("0.5")),
        NativeVersion.from("latest") == Left(NativeVersionError.Bad("latest"))
      )
    ,
    test("a native module names a clang major, and a jvm module does not"):
      val missing = summary(Projects(module("core", Native)(scala3)))
      val named = Projects(module("core", Native)(scala3).clang(ClangMajor(21)))
      val extra =
        summary(Projects(module("core", Jvm)(scala3).clang(ClangMajor(21))))
      val pin = named match
        case Right(graph) =>
          graph.projects.collect { case item: Module => item.clang }
        case Left(_) => Nil
      assertTrue(
        missing.contains("clang major"),
        extra.contains("does not target Native"),
        pin == List(ClangPin.Major(ClangMajor(21)))
      )
    ,
    test("a ship names the modules that publish its number"):
      val core = module("core", Jvm)(scala3)
      val tests = module("tests", Jvm)(scala3).dependsOn(core)
      val built = Projects(ships(ship("libs", ship"1.4.2")(core)), core, tests)
      val shipsOnGraph = built.map(_.catalog.ships.map:
        case Ship.Line(name, shipNumber, members) =>
          s"${name.value} ${shipNumber.render} ${members.map(_.value).mkString(",")}"
        case Ship.Manual(name, version, members) =>
          s"${name.value} ${version.value} ${members.map(_.value).mkString(",")}")
      assertTrue(shipsOnGraph == Right(List("libs 1.4.2 core")))
    ,
    test(
      "a manual version is not an early-effect ship, and the two are not mixed"
    ):
      val core = module("core", Jvm)(scala3)
      val app = module("app", Jvm)(scala3)
      val stated =
        Projects(ships(manual("libs", manual"1.2.3-SNAPSHOT")(core)), core)
      val mixed = summary(
        Projects(
          ships(
            ship("libs", ship"1.0.0")(core),
            manual("app", manual"2026.10.17")(app)
          ),
          core,
          app
        )
      )
      val rendered = stated.map(_.catalog.ships.map:
        case histos.version.Ship.Manual(_, version, _) => version.value
        case histos.version.Ship.Line(_, number, _)    => number.render)
      assertTrue(
        rendered == Right(List("1.2.3-SNAPSHOT")),
        mixed.contains("manual versions")
      )
    ,
    test("a module is in at most one ship and an aggregate is not a member"):
      val core = module("core", Jvm)(scala3)
      val tests = module("tests", Jvm)(scala3)
      val root = aggregate("root", core)
      val shared = summary(
        Projects(
          ships(
            ship("libs", ship"1.0.0")(core),
            ship("also", ship"2.0.0")(core)
          ),
          core
        )
      )
      val again = summary(
        Projects(
          ships(
            ship("libs", ship"1.0.0")(core),
            ship("libs", ship"2.0.0")(tests)
          ),
          core,
          tests
        )
      )
      val agg =
        summary(Projects(ships(ship("libs", ship"1.0.0")(root)), root, core))
      val repeat =
        summary(Projects(ships(ship("libs", ship"1.0.0")(core, core)), core))
      val absent = summary(Projects(ships(ship("libs", ship"1.0.0")(core))))
      assertTrue(
        shared.contains("ship 'libs'") && shared.contains("ship 'also'"),
        again.contains("declared twice"),
        agg.contains("aggregate 'root'"),
        repeat.contains("lists 'core' twice"),
        absent.contains("core")
      )
    ,
    test("records a builder position"):
      val core = module("core", Jvm)(scala3)
      assertTrue(
        core.position.line >= 1 && core.position.column >= 1 && core.position.path.nonEmpty
      )
  )

  def spec = graph
