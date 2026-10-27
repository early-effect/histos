package histos

import histos.version.{Ship, ShipName, ShipNameError}

import zio.{IO, ZIO}
import zio.test.*

object GraphSpec extends ZIOSpecDefault:
  private val scala3 = ScalaVersion("3.9.0")
  private val scala2 = ScalaVersion("2.13.16")
  private val zioDep = dep"dev.zio::zio:2.1.26"

  private def projectName(raw: String): IO[ProjectNameError, ProjectName] =
    ZIO.fromEither(ProjectName.from(raw))

  private def shipName(raw: String): IO[ShipNameError, ShipName] =
    ZIO.fromEither(ShipName.from(raw))

  private def ring(n: Int): Either[BuildError, ProjectGraph] =
    val count  = if n < 2 then 2 else n
    val bases  = (0 until count).toList.map(i => module(s"p$i", Jvm)(scala3))
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
      val tests   = module("tests", Jvm)(scala3).dependsOn(core)
      val built   = Projects(aggregate("root", core, tests), core, tests)
      val roots   = built.map(_.projects.collect { case item: Aggregate => item.children.map(_.value) })
      val modules = built.map(_.projects.collect:
        case item: Module =>
          (item.platforms.all, item.versions.all, item.dependencies, item.dependsOn.map(_.name.value), item.clang))
      assertTrue(
        roots == Right(List(List("core", "tests"))),
        built.is(_.right).splits == Splits.aggregate,
        built.is(_.right).organization.isEmpty,
        built.is(_.right).resolvers.isEmpty,
        modules == Right(
          List(
            (List(Jvm, Js, Native), List(scala3, scala2), List(zioDep), Nil, ClangPin.Major(ClangMajor(21))),
            (List(Jvm), List(scala3), Nil, List("core"), ClangPin.Unused),
          )
        ),
      )
    ,
    test("a module stores the class histos run starts"):
      val core  = module("core", Jvm)(scala3).main(MainClass("demo.Check"))
      val built = Projects(core)
      assertTrue(
        built.map(_.projects.collect { case module: Module =>
          module.application.map(_.value)
        }) == Right(List(Some("demo.Check"))),
        MainClass.from("demo.Check").map(_.value) == Right("demo.Check"),
        MainClass.from("") == Left(MainClassError.Empty),
        MainClass.from("1bad") == Left(MainClassError.Bad("1bad")),
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
        PlatformError.Unknown("scala").message.contains("not a platform"),
      )
    ,
    test("a module stores one test main"):
      val core  = module("core", Jvm)(scala3).test(TestMain("demo.Check"))
      val built = Projects(core)
      assertTrue(
        built.map(_.projects.collect { case item: Module =>
          item.test.map(_.value)
        }) == Right(List(Some("demo.Check"))),
        TestMain.from("demo.Check").map(_.value) == Right("demo.Check"),
        TestMain.from("") == Left(TestMainError.Empty),
        TestMain.from("1bad") == Left(TestMainError.Bad("1bad")),
      )
    ,
    test("a provider is stored, and a main beside a provider is refused"):
      val custom = TestProvider(
        FrameworkClass("munit.Framework"),
        "org.scalameta",
        "munit",
      )
      val named =
        Projects(module("core", Jvm)(scala3).testProvider(TestProvider.Zio))
      val mixed = module("core", Jvm)(scala3)
        .test(TestMain("demo.Check"))
        .testProvider(TestProvider.Zio)
      projectName("core").map: core =>
        assertTrue(
          custom.map(_.library) == ProviderLibrary.from("org.scalameta", "munit"),
          named.map(_.projects.collect { case module: Module =>
            module.provider
          }) == Right(List(Some(TestProvider.Zio))),
          Projects(mixed) == Left(BuildError.MixedTest(core, mixed.position)),
          FrameworkClass.from("") == Left(FrameworkClassError.Empty),
          ProviderLibrary.from("", "munit") == Left(ProviderLibraryError.Empty),
        )
    ,
    test("a js module names its environment and module kind"):
      val ui = module("ui", Js)(scala3)
        .jsEnv(JsEnv.Jsdom)
        .jsModule(JsModuleKind.ESModule)
      val firefox = module("browser", Js)(scala3).jsEnv(JsEnv.Firefox)
      val jvm     = module("core", Jvm)(scala3)
      val kinds   = Projects(ui, firefox).map(_.projects.collect { case module: Module =>
        (module.name.value, module.jsEnv, module.jsModule)
      })
      assertTrue(
        kinds == Right(
          List(
            ("ui", JsEnv.Jsdom, JsModuleKind.ESModule),
            ("browser", JsEnv.Firefox, JsModuleKind.NoModule),
          )
        ),
        Projects(jvm).map(_.projects.collect { case module: Module =>
          module.jsEnv
        }) == Right(List(JsEnv.Node)),
      )
    ,
    test("test dependencies stay off the compile classpath"):
      val tests = dep"dev.zio::zio-test:2.1.27"
      val core  = module("core", Jvm)(scala3)
        .libraryDependencies(zioDep)
        .testDependencies(tests)
      val built = Projects(core)
      assertTrue:
        built.map(_.projects.collect { case item: Module =>
          (item.dependencies, item.testDependencies)
        }) == Right(List((List(zioDep), List(tests))))
    ,
    test(
      "a listed repository is not implied to be central, and a repeat is refused"
    ):
      val core  = module("core", Jvm)(scala3)
      val file  = Projects.resolvers(maven"file:///repo")(core)
      val both  = Projects.resolvers(maven"file:///repo", central)(core)
      val twice = Projects.resolvers(central, central)(core)
      assertTrue(
        file.map(_.resolvers) == Right(
          List(
            Resolver.Maven(RepositoryUrl("file:///repo"))
          )
        ),
        both.map(_.resolvers) == Right(
          List(
            Resolver.Maven(RepositoryUrl("file:///repo")),
            central,
          )
        ),
        twice == Left(BuildError.DuplicateResolver("central")),
      )
    ,
    test("the empty split set is the default"):
      val core = module("core", Jvm)(scala3)
      assertTrue(
        Projects(core).map(_.splits) == Right(Splits.aggregate),
        Projects(Splits.of(Axis.Module), core).map(_.splits) == Right(
          Splits.of(Axis.Module)
        ),
        Splits.of(Axis.Module, Axis.Module) == Splits.of(Axis.Module),
      )
    ,
    test("rejects a duplicate name"):
      val first  = module("core", Jvm)(scala3)
      val second = module("core", Js)(scala3)
      projectName("core").map: core =>
        assertTrue(Projects(first, second) == Left(BuildError.DuplicateName(core, first.position, second.position)))
    ,
    test("rejects a duplicate platform and a duplicate Scala version"):
      val platforms = module("core", Jvm, Jvm)(scala3)
      val versions  = module("core", Jvm)(scala3, scala3)
      projectName("core").map: core =>
        assertTrue(
          Projects(platforms) == Left(BuildError.DuplicatePlatform(core, Jvm, platforms.position)),
          Projects(versions) == Left(BuildError.DuplicateScalaVersion(core, scala3, versions.position)),
        )
    ,
    test("rejects an empty or illegal name"):
      val empty = module("", Jvm)(scala3)
      val bad   = module("1core", Jvm)(scala3)
      assertTrue(
        Projects(empty) == Left(BuildError.EmptyName(empty.position)),
        Projects(bad) == Left(BuildError.BadName("1core", bad.position)),
      )
    ,
    test("rejects a self-dependency"):
      val base = module("core", Jvm)(scala3)
      projectName("core").map: core =>
        assertTrue(
          Projects(base.dependsOn(base)) == Left(BuildError.SelfDependency(core, base.position))
        )
    ,
    test("rejects a dependency on an aggregate"):
      val core  = module("core", Jvm)(scala3)
      val root  = aggregate("root", core)
      val tests = module("tests", Jvm)(scala3).dependsOn(root)
      for
        testsName <- projectName("tests")
        rootName  <- projectName("root")
      yield assertTrue(
        Projects(root, core, tests) == Left(BuildError.DependsOnAggregate(testsName, rootName, tests.position))
      )
    ,
    test("rejects a project that was not listed"):
      val core  = module("core", Jvm)(scala3)
      val tests = module("tests", Jvm)(scala3).dependsOn(core)
      assertTrue(Projects(tests) == Left(BuildError.MissingProject("core", core.position)))
    ,
    test("rejects an aggregate that includes itself"):
      val loop = aggregate("root", module("root", Jvm)(scala3))
      projectName("root").map: root =>
        assertTrue(Projects(loop) == Left(BuildError.AggregateIncludesItself(root, loop.position)))
    ,
    test("rejects an aggregate that lists a child twice"):
      val core = module("core", Jvm)(scala3)
      val root = aggregate("root", core, core)
      for
        rootName <- projectName("root")
        coreName <- projectName("core")
      yield assertTrue(
        Projects(root, core) == Left(BuildError.DuplicateChild(rootName, coreName, root.position))
      )
    ,
    test("rejects an aggregate child whose name is another aggregate"):
      val core  = module("core", Jvm)(scala3)
      val tools = module("tools", Jvm)(scala3)
      val root  = aggregate("root", core)
      for
        rootName <- projectName("root")
        coreName <- projectName("core")
      yield assertTrue(
        Projects(root, aggregate("core", tools), tools) ==
          Left(BuildError.AggregateChild(rootName, coreName, root.position))
      )
    ,
    test("a library coordinate can name its platforms"):
      val time = dep"io.github.cquiroz::scala-java-time:2.7.0".on(Js, Native)
      val core = module("core", Jvm, Js, Native)(scala3)
        .libraryDependencies(zioDep, time)
        .clang(ClangMajor(21))
      val built    = Projects(core)
      val selected = built.map(_.projects.collect { case item: Module =>
        (
          Dep.select(item.dependencies, Jvm),
          Dep.select(item.dependencies, Js),
          Dep.select(item.dependencies, Native),
        )
      })
      assertTrue:
        selected == Right(List((List(zioDep), List(zioDep, time), List(zioDep, time))))
    ,
    test("an upstream edge can name its platforms"):
      val types  = module("types", Jvm, Js, Native)(scala3).clang(ClangMajor(21))
      val facade = module("facade", Js)(scala3).dependsOn(types)
      val core   = module("core", Jvm, Js, Native)(scala3)
        .clang(ClangMajor(21))
        .dependsOn(types)
        .dependsOn(Js)(facade)
      val built    = Projects(types, facade, core)
      val recorded = built.map(_.projects.collect:
        case module: Module if module.name.value == "core" =>
          (
            module.dependsOn.map(edge => (edge.name.value, edge.platforms)),
            module.upstream(Jvm).map(_.value),
            module.upstream(Js).map(_.value),
            module.upstream(Native).map(_.value),
          ))
      assertTrue:
        recorded == Right(
          List(
            (
              List(("types", None), ("facade", Some(List(Js)))),
              List("types"),
              List("types", "facade"),
              List("types"),
            )
          )
        )
    ,
    test("an unscoped edge requires every platform the depender builds"):
      val facade = module("facade", Js)(scala3)
      val core   = module("core", Jvm, Js)(scala3).dependsOn(facade)
      for
        coreName   <- projectName("core")
        facadeName <- projectName("facade")
      yield assertTrue(
        Projects(facade, core) ==
          Left(BuildError.UpstreamMissingPlatform(coreName, facadeName, Jvm, core.position))
      )
    ,
    test("a scoped edge cannot name a platform either side lacks"):
      val types  = module("types", Jvm)(scala3)
      val facade = module("facade", Js)(scala3)
      val here   = module("core", Js)(scala3).dependsOn(Jvm)(facade)
      val there  = module("core", Jvm, Js)(scala3).dependsOn(Js)(types)
      for
        core      <- projectName("core")
        typesName <- projectName("types")
      yield assertTrue(
        Projects(facade, here) == Left(BuildError.EdgeWithoutPlatform(core, Jvm, here.position)),
        Projects(types, there) == Left(BuildError.UpstreamMissingPlatform(core, typesName, Js, there.position)),
      )
    ,
    test("the same upstream twice is refused"):
      val types = module("types", Jvm)(scala3)
      val core  =
        module("core", Jvm)(scala3).dependsOn(types).dependsOn(Js)(types)
      for
        coreName  <- projectName("core")
        typesName <- projectName("types")
      yield assertTrue(
        Projects(types, core) == Left(BuildError.DuplicateDependency(coreName, typesName, core.position))
      )
    ,
    test(
      "edges on different platforms are not a cycle, and one platform still is"
    ):
      val left  = module("left", Jvm, Js)(scala3)
      val right = module("right", Jvm, Js)(scala3)
      val split =
        Projects(left.dependsOn(Js)(right), right.dependsOn(Jvm)(left))
      val loop =
        Projects(left.dependsOn(Js)(right), right.dependsOn(Js)(left))
      val edges = split.map(_.projects.collect:
        case module: Module =>
          (module.name.value, module.upstream(Jvm).map(_.value), module.upstream(Js).map(_.value)))
      for
        leftName  <- projectName("left")
        rightName <- projectName("right")
      yield assertTrue(
        edges == Right(List(("left", Nil, List("right")), ("right", List("left"), Nil))),
        loop == Left(BuildError.Cycle(Js, List(leftName, rightName, leftName))),
      )
    ,
    test("a module can live under a directory that is not its name"):
      val todo   = module("todo", Js)(scala3).in(path"example/todo-conduit")
      val built  = Projects(todo)
      val shared = Projects(
        module("core", Jvm)(scala3),
        module("app", Jvm)(scala3).in(path"core"),
      )
      val placed = built.map(_.projects.collect { case found: Module =>
        (found.baseDirectory, found.directory)
      })
      for
        core <- projectName("core")
        app  <- projectName("app")
      yield assertTrue(
        placed == Right(List(("example/todo-conduit", Some(path"example/todo-conduit")))),
        shared == Left(BuildError.DuplicateDirectory(app, core, "core")),
        ProjectPath
          .from("example/todo-conduit")
          .map(_.value) == Right("example/todo-conduit"),
        ProjectPath.from("../secret") == Left(
          ProjectPathError.Escapes("../secret")
        ),
        ProjectPath.from("/tmp/secret") == Left(
          ProjectPathError.Escapes("/tmp/secret")
        ),
        ProjectPath.from("") == Left(ProjectPathError.Empty),
        ProjectPath.from("example//todo") == Left(
          ProjectPathError.Escapes("example//todo")
        ),
      )
    ,
    test("a ring of modules is a cycle"):
      check(Gen.int(2, 6)): n =>
        ZIO
          .foreach((0 until n).toList :+ 0)(index => projectName(s"p$index"))
          .map: path =>
            assertTrue(ring(n) == Left(BuildError.Cycle(Jvm, path)))
    ,
    test("a native version belongs on a native module"):
      val native = Projects(
        module("core", Native)(scala3)
          .clang(ClangMajor(21))
          .nativeVersion(NativeVersion("0.5.13-SNAPSHOT"))
      )
      assertTrue(
        native.map(_.projects.collect { case item: Module =>
          item.nativeVersion.map(_.value)
        }) == Right(List(Some("0.5.13-SNAPSHOT"))),
        NativeVersion.from("0.5") == Left(NativeVersionError.Bad("0.5")),
        NativeVersion.from("latest") == Left(NativeVersionError.Bad("latest")),
      )
    ,
    test("a native module names a clang major"):
      val missing = module("core", Native)(scala3)
      val named   = Projects(module("core", Native)(scala3).clang(ClangMajor(21)))
      val pin     = named.map(_.projects.collect { case item: Module => item.clang })
      projectName("core").map: core =>
        assertTrue(
          Projects(missing) == Left(BuildError.MissingClang(core, missing.position)),
          pin == Right(List(ClangPin.Major(ClangMajor(21)))),
        )
    ,
    test("a ship names the modules that publish its number"):
      val core   = module("core", Jvm)(scala3)
      val tests  = module("tests", Jvm)(scala3).dependsOn(core)
      val number = ship"1.4.2"
      val built  = Projects(ships(ship("libs", number)(core)), core, tests)
      for
        libs     <- shipName("libs")
        coreName <- projectName("core")
      yield assertTrue(
        built.map(_.catalog.ships) == Right(List(Ship.Line(libs, number, ::(coreName, Nil))))
      )
    ,
    test("a ship name is not empty and is a name"):
      val core    = module("core", Jvm)(scala3)
      val unnamed = ship("", ship"1.0.0")(core)
      val bad     = ship("1libs", ship"1.0.0")(core)
      assertTrue(
        Projects(ships(unnamed), core) == Left(BuildError.EmptyShipName(unnamed.position)),
        Projects(ships(bad), core) == Left(BuildError.BadShipName("1libs", bad.position)),
      )
    ,
    test(
      "a manual version is not an early-effect ship, and the two are not mixed"
    ):
      val core    = module("core", Jvm)(scala3)
      val app     = module("app", Jvm)(scala3)
      val version = manual"1.2.3-SNAPSHOT"
      val stated  =
        Projects(ships(manual("libs", version)(core)), core)
      val appShip = manual("app", manual"2026.10.17")(app)
      val mixed   = Projects(
        ships(
          ship("libs", ship"1.0.0")(core),
          appShip,
        ),
        core,
        app,
      )
      for
        libs     <- shipName("libs")
        coreName <- projectName("core")
      yield assertTrue(
        stated.map(_.catalog.ships) == Right(List(Ship.Manual(libs, version, ::(coreName, Nil)))),
        mixed == Left(BuildError.MixedScheme(appShip.position)),
      )
    ,
    test("a module is in at most one ship and an aggregate is not a member"):
      val core       = module("core", Jvm)(scala3)
      val tests      = module("tests", Jvm)(scala3)
      val root       = aggregate("root", core)
      val libs       = ship("libs", ship"1.0.0")(core)
      val also       = ship("also", ship"2.0.0")(core)
      val libsAgain  = ship("libs", ship"2.0.0")(tests)
      val aggregated = ship("libs", ship"1.0.0")(root)
      val repeated   = ship("libs", ship"1.0.0")(core, core)
      val shared     = Projects(ships(libs, also), core)
      val again      = Projects(ships(libs, libsAgain), core, tests)
      val agg        = Projects(ships(aggregated), root, core)
      val repeat     = Projects(ships(repeated), core)
      val absent     = Projects(ships(libs))
      for
        libsName <- shipName("libs")
        alsoName <- shipName("also")
        coreName <- projectName("core")
        rootName <- projectName("root")
      yield assertTrue(
        shared == Left(BuildError.MemberInTwoShips(coreName, libsName, alsoName, also.position)),
        again == Left(BuildError.DuplicateShip(libsName, libs.position, libsAgain.position)),
        agg == Left(BuildError.AggregateShipMember(libsName, rootName, aggregated.position)),
        repeat == Left(BuildError.RepeatedShipMember(libsName, coreName, repeated.position)),
        absent == Left(BuildError.MissingProject("core", core.position)),
      )
    ,
    test("records a builder position"):
      val core = module("core", Jvm)(scala3)
      assertTrue(
        core.position.line >= 1 && core.position.column >= 1 && core.position.path.nonEmpty
      ),
  )

  def spec = graph
end GraphSpec
