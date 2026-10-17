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
      val core  = module("core", Jvm, Js, Native)(scala3, scala2).libraryDependencies(zioDep).clang(ClangMajor(21))
      val tests = module("tests", Jvm)(scala3).dependsOn(core)
      val built = Projects(aggregate("root", core, tests), core, tests)
      assertTrue:
        built match
          case Right(
                ProjectGraph(
                  List(
                    Aggregate(_, children, _),
                    Module(_, platforms, versions, dependencies, coreDeps, _, ClangPin.Major(clang)),
                    Module(_, _, _, _, testDeps, _, _),
                  ),
                  Splits.aggregate,
                  _,
                  None,
                  Nil,
                )
              ) =>
            children.map(_.value) == List("core", "tests") &&
            platforms.all == List(Jvm, Js, Native) &&
            versions.all == List(scala3, scala2) &&
            dependencies == List(zioDep) &&
            coreDeps.isEmpty &&
            testDeps.map(_.value) == List("core") &&
            clang == ClangMajor(21)
          case other => summary(other) == "core module"
    ,
    test("a listed repository is not implied to be central, and a repeat is refused"):
      val core = module("core", Jvm)(scala3)
      val file = Projects.resolvers(maven"file:///repo")(core)
      val both = Projects.resolvers(maven"file:///repo", central)(core)
      val twice = Projects.resolvers(central, central)(core)
      assertTrue(
        file match
          case Right(graph) => graph.resolvers == List(Resolver.Maven(RepositoryUrl("file:///repo")))
          case Left(_)      => false
        ,
        both match
          case Right(graph) => graph.resolvers == List(Resolver.Maven(RepositoryUrl("file:///repo")), central)
          case Left(_)      => false
        ,
        twice.fold(_.message, _ => "") == "repository central is listed twice",
      )
    ,
    test("the empty split set is the default"):
      val core = module("core", Jvm)(scala3)
      assertTrue(
        Projects(core).map(_.splits) == Right(Splits.aggregate),
        Projects(Splits.of(Axis.Module), core).map(_.splits) == Right(Splits.of(Axis.Module)),
        Splits.of(Axis.Module, Axis.Module) == Splits.of(Axis.Module),
      )
    ,
    test("rejects a duplicate name"):
      val first  = module("core", Jvm)(scala3)
      val second = module("core", Js)(scala3)
      val message = summary(Projects(first, second))
      assertTrue(message.contains("project 'core' is declared twice")),
    test("rejects a duplicate platform and a duplicate Scala version"):
      val platforms = summary(Projects(module("core", Jvm, Jvm)(scala3)))
      val versions  = summary(Projects(module("core", Jvm)(scala3, scala3)))
      assertTrue(platforms.contains("jvm"), versions.contains("3.9.0")),
    test("rejects an empty or illegal name"):
      val empty = summary(Projects(module("", Jvm)(scala3)))
      val bad   = summary(Projects(module("1core", Jvm)(scala3)))
      assertTrue(empty.contains("not empty"), bad.contains("1core")),
    test("rejects a self-dependency"):
      val base = module("core", Jvm)(scala3)
      assertTrue(summary(Projects(base.dependsOn(base))).contains("depends on itself")),
    test("rejects a dependency on an aggregate"):
      val core  = module("core", Jvm)(scala3)
      val root  = aggregate("root", core)
      val tests = module("tests", Jvm)(scala3).dependsOn(root)
      assertTrue(summary(Projects(root, core, tests)).contains("aggregate 'root'")),
    test("rejects a project that was not listed"):
      val core  = module("core", Jvm)(scala3)
      val tests = module("tests", Jvm)(scala3).dependsOn(core)
      assertTrue(summary(Projects(tests)).contains("core")),
    test("rejects an aggregate that includes itself"):
      val loop = aggregate("root", module("root", Jvm)(scala3))
      assertTrue(summary(Projects(loop)).contains("includes itself")),
    test("rejects an aggregate that lists a child twice"):
      val core = module("core", Jvm)(scala3)
      assertTrue(summary(Projects(aggregate("root", core, core), core)).contains("twice")),
    test("a ring of modules is a cycle"):
      check(Gen.int(2, 6)): n =>
        val message = summary(ring(n))
        assertTrue(message.contains("cycle") && message.contains("p0") && message.contains("p1")),
    test("a native module names a clang major, and a jvm module does not"):
      val missing = summary(Projects(module("core", Native)(scala3)))
      val named   = Projects(module("core", Native)(scala3).clang(ClangMajor(21)))
      val extra   = summary(Projects(module("core", Jvm)(scala3).clang(ClangMajor(21))))
      val pin = named match
        case Right(ProjectGraph(List(Module(_, _, _, _, _, _, clang)), _, _, _, _)) => clang
        case _                                                              => ClangPin.Unused
      assertTrue(
        missing.contains("clang major"),
        extra.contains("does not target Native"),
        pin == ClangPin.Major(ClangMajor(21)),
      )
    ,
    test("a ship names the modules that publish its number"):
      val core  = module("core", Jvm)(scala3)
      val tests = module("tests", Jvm)(scala3).dependsOn(core)
      val built = Projects(ships(ship("libs", ship"1.4.2")(core)), core, tests)
      val shipsOnGraph = built.map(_.catalog.ships.map:
        case Ship.Line(name, shipNumber, members) =>
          s"${name.value} ${shipNumber.render} ${members.map(_.value).mkString(",")}"
        case Ship.Manual(name, version, members) =>
          s"${name.value} ${version.value} ${members.map(_.value).mkString(",")}"
      )
      assertTrue(shipsOnGraph == Right(List("libs 1.4.2 core")))
    ,
    test("a manual version is not an early-effect ship, and the two are not mixed"):
      val core = module("core", Jvm)(scala3)
      val app = module("app", Jvm)(scala3)
      val stated = Projects(ships(manual("libs", manual"1.2.3-SNAPSHOT")(core)), core)
      val mixed = summary(Projects(ships(ship("libs", ship"1.0.0")(core), manual("app", manual"2026.10.17")(app)), core, app))
      val rendered = stated.map(_.catalog.ships.map:
        case histos.version.Ship.Manual(_, version, _) => version.value
        case histos.version.Ship.Line(_, number, _)    => number.render
      )
      assertTrue(rendered == Right(List("1.2.3-SNAPSHOT")), mixed.contains("manual versions"))
    ,
    test("a module is in at most one ship and an aggregate is not a member"):
      val core  = module("core", Jvm)(scala3)
      val tests = module("tests", Jvm)(scala3)
      val root  = aggregate("root", core)
      val shared = summary(Projects(ships(ship("libs", ship"1.0.0")(core), ship("also", ship"2.0.0")(core)), core))
      val again  = summary(Projects(ships(ship("libs", ship"1.0.0")(core), ship("libs", ship"2.0.0")(tests)), core, tests))
      val agg    = summary(Projects(ships(ship("libs", ship"1.0.0")(root)), root, core))
      val repeat = summary(Projects(ships(ship("libs", ship"1.0.0")(core, core)), core))
      val absent = summary(Projects(ships(ship("libs", ship"1.0.0")(core))))
      assertTrue(
        shared.contains("ship 'libs'") && shared.contains("ship 'also'"),
        again.contains("declared twice"),
        agg.contains("aggregate 'root'"),
        repeat.contains("lists 'core' twice"),
        absent.contains("core"),
      )
    ,
    test("records a builder position"):
      val core = module("core", Jvm)(scala3)
      assertTrue(core.position.line >= 1 && core.position.column >= 1 && core.position.path.nonEmpty),
  )

  def spec = graph
