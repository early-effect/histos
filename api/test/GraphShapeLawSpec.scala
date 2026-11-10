package histos

import histos.plugin.Need

import _root_.scala.compiletime.testing.typeCheckErrors

import zio.test.*

object GraphShapeLawSpec extends ZIOSpecDefault:
  private val scala3 = scala"3.10.0"
  private val scala2 = scala"2.13.16"
  private val here   = SourcePos("law", 1, 1)

  def spec = suite("graph shape laws")(
    test("a legal graph keeps platforms, upstreams, and the defaults a build did not set"):
      val lib   = js("lib")
      val app   = both("app").dependsOn(Js)(lib).dependsOn(Jvm, Jvm)(jvm("support"))
      val built = Projects(app, lib, jvm("support"))
      assertTrue:
        built.map(upstream("app", Js)) == Right(List("lib")) &&
        built.map(upstream("app", Jvm)) == Right(List("support")) &&
        built.map(edgePlatforms("app")) == Right(List(Some(List(Js)), Some(List(Jvm)))) &&
        built.map(_.docCheck) == Right(DocCheck.Off) &&
        built.map(_.resolution) == Right(Resolution.Solve) &&
        built.map(_.promote) == Right(Promote.Rebuild) &&
        built.map(_.testLog) == Right(false)
    ,
    test("a diamond is not a cycle, and a cycle names the platform it closes on"):
      val d     = jvm("d")
      val b     = jvm("b").dependsOn(d)
      val c     = jvm("c").dependsOn(d)
      val a     = jvm("a").dependsOn(b, c)
      val left  = both("left").dependsOn(Js)(both("right"))
      val right = both("right").dependsOn(Js)(both("left"))
      assertTrue(
        Projects(a, b, c, d).map(chainOf("a")) == Right(List("d", "b", "c")),
        Projects(a, b, c, d).map(closureOf("c")) == Right(List("c", "d")),
        fault(Projects(left, right)) == "Cycle",
        cyclePlatform(Projects(left, right)) == Some(Js),
        cycleCloses(Projects(left, right)),
      )
    ,
    test("a chain drops a name that is not a module and does not include itself"):
      val present = ProjectName("present")
      val missing = ProjectName("missing")
      val module  = Module(
        present,
        Platforms(Jvm, Nil),
        ScalaVersions(scala3, Nil),
        Nil,
        List(ProjectDep(missing, None), ProjectDep(present, Some(List(Js)))),
        here,
      )
      assertTrue(
        Module.chain(module, Nil, Jvm).isEmpty,
        Module.chain(module, Nil, Js).isEmpty,
        ProjectDep(missing, None).applies(Js),
        !ProjectDep(missing, Some(List(Jvm))).applies(Js),
      )
    ,
    test("repeated platforms, scala versions, children, and directories are refused"):
      val core = jvm("core")
      assertTrue(
        fault(Projects(module("core", Jvm, Jvm)(scala3))) == "DuplicatePlatform",
        fault(Projects(module("core", Jvm)(scala3, scala3))) == "DuplicateScalaVersion",
        fault(Projects(aggregate("root", core, core), core)) == "DuplicateChild",
        fault(Projects(aggregate("core", core))) == "AggregateIncludesItself",
        fault(Projects(core, jvm("app").in(path"core"))) == "DuplicateDirectory",
        fault(Projects(module("", Jvm)(scala3))) == "EmptyName",
        fault(Projects(module("core.name", Jvm)(scala3))) == "BadName",
      )
    ,
    test("an edge must name a module, on a platform both sides build"):
      val core   = jvm("core")
      val lib    = js("lib")
      val shared = jvm("shared")
      val root   = aggregate("root", core)
      val mid    = aggregate("mid", core)
      val hole   = AggregateDef.make("root", List(module("mid", Jvm)(scala3)), here)
      assertTrue(
        fault(Projects(core.dependsOn(jvm("ghost")))) == "MissingProject",
        fault(Projects(core.dependsOn(root), root)) == "DependsOnAggregate",
        fault(Projects(jvm("app").dependsOn(shared, shared), shared)) == "DuplicateDependency",
        fault(Projects(core.dependsOn(Js)(lib), lib)) == "EdgeWithoutPlatform",
        fault(Projects(core.dependsOn(lib), lib)) == "UpstreamMissingPlatform",
        fault(Projects(hole, mid, core)) == "AggregateChild",
      )
    ,
    test("native without a clang major is refused, and a major is kept"):
      val bare   = module("native", Native)(scala3)
      val pinned = bare.clang(ClangMajor(21)).nativeVersion(NativeVersion("0.5.13"))
      assertTrue(
        fault(Projects(bare)) == "MissingClang",
        Projects(pinned).map(clangOf("native")) == Right(Some(ClangPin.Major(ClangMajor(21)))),
        Projects(js("core")).map(clangOf("core")) == Right(Some(ClangPin.Unused)),
      )
    ,
    test("one publication is kept, and an assembly is one jvm jar with at most one asset"):
      val main  = MainClass("app.Main")
      val jar   = asset"dist/app.jar"
      val again = asset"dist/other.jar"
      val built = Projects(jvm("app").assembly(main).asset(jar))
      assertTrue(
        fault(Projects(jvm("app").library.application)) == "TwoPublications",
        fault(Projects(js("app").assembly(main))) == "AssemblyShape",
        fault(Projects(module("app", Jvm)(scala3, scala2).assembly(main))) == "AssemblyShape",
        fault(Projects(jvm("app").library.asset(jar))) == "AssetWithoutAssembly",
        fault(Projects(jvm("app").assembly(main).asset(jar).asset(again))) == "TwoAssets",
        built.map(publicationOf("app")) == Right(Some(Publication.Assembly(main))),
        built.map(assetOf("app")) == Right(Some(jar)),
        Projects(jvm("core")).map(publicationOf("core")) == Right(Some(Publication.Library)),
        Projects(js("app").application).map(publicationOf("app")) == Right(Some(Publication.Application)),
        Projects(jvm("sbt").sbtPlugin(sbt"2")).map(publicationOf("sbt")) == Right(Some(Publication.SbtPlugin(sbt"2"))),
      )
    ,
    test("a generated resource is kept, and the same name twice is refused"):
      val note  = GeneratedAsset.Text(ResourceName("note.txt"), "hello")
      val again = GeneratedAsset.Text(ResourceName("note.txt"), "again")
      val other = GeneratedAsset.Text(ResourceName("other.txt"), "other")
      assertTrue(
        fault(Projects(jvm("core").resources(note, again))) == "DuplicateResource",
        fault(Projects(jvm("core").resources(note).resources(again))) == "DuplicateResource",
        Projects(jvm("core").resources(note)).map(resourcesOf("core")) == Right(List(note)),
        Projects(jvm("core").resources(note).resources(other)).map(resourcesOf("core")) == Right(List(note, other)),
      )
    ,
    test("a remote store with a need is refused, and a test main excludes a provider"):
      val remote = jvm("core").testStore(TestStore.Remote).needs(Need.Network)
      val mixed  = jvm("core").test(TestMain("app.Test")).testProvider(TestProvider.Zio)
      assertTrue(
        fault(Projects(remote)) == "RemoteNeeds",
        fault(Projects(mixed)) == "MixedTest",
        Projects(jvm("core").testStore(TestStore.Remote)).map(storeOf("core")) == Right(TestStore.Remote),
        Projects(jvm("core").needs(Need.Network, Need.Docker, Need.Network)).map(needsOf("core")) == Right(
          Set(Need.Network, Need.Docker)
        ),
        Projects(jvm("core").test(TestMain("app.Test"))).map(testOf("core")) == Right(Some("app.Test")),
        Projects(jvm("core").testProvider(TestProvider.Zio)).map(providerOf("core")) == Right(true),
      )
    ,
    test("a second scalac call replaces the list, and js settings stay on the js module"):
      val flagsOn = jvm("core").scalac(ScalacFlag.Feature, ScalacFlag.Deprecation).scalac(ScalacFlag.UnusedAll)
      val script  = js("ui")
        .jsEnv(JsEnv.Firefox)
        .jsModule(JsModuleKind.CommonJS)
        .jsVersion(JsVersion("1.22.0"))
      val refused    = typeCheckErrors("""module("core", Jvm)(scala"3.10.0").jsEnv(JsEnv.Node)""")
      val clangOnJvm = typeCheckErrors("""module("core", Jvm)(scala"3.10.0").clang(ClangMajor(21))""")
      assertTrue(
        Projects(flagsOn).map(canonicalOf("core")) == Right("unused-all"),
        Projects(script).map(jsOf("ui")) == Right(("firefox", "commonjs", Some("1.22.0"))),
        Projects(js("ui")).map(jsOf("ui")) == Right(("node", "nomodule", None)),
        Projects(jvm("core").in(path"modules/core")).map(baseOf("core")) == Right("modules/core"),
        refused.nonEmpty,
        clangOnJvm.nonEmpty,
      )
    ,
    test("organization settings accumulate, and ci replaces a timeout set before it"):
      val repo      = maven"https://example.com/repo"
      val mit       = License(license"MIT", url"https://opensource.org/license/mit")
      val apache    = License(license"Apache-2.0", url"https://www.apache.org/licenses/LICENSE-2.0")
      val who       = Developer(developer"early-effect", pom"Early Effect")
      val organized = Projects
        .organization(organization"rocks.earlyeffect")
        .resolvers(central)
        .resolvers(repo)
        .testLog
        .locked
        .promote(promote"copy-index")
        .license(apache)
        .license(mit)
        .developer(who)
        .coverage(CoverageFloors(statements = Some(80)))
      val replaced = Projects.jobTimeout(JobTimeout.Minutes(12)).ci(plugin.CiSettings.empty)
      val kept     = Projects.ci(plugin.CiSettings.empty).jobTimeout(JobTimeout.Minutes(12))
      assertTrue(
        fault(Projects.resolvers(central, central)(jvm("core"))) == "DuplicateResolver",
        fault(Projects.resolvers(repo, repo)(jvm("core"))) == "DuplicateResolver",
        organized(jvm("core")).map(settingsOf) == Right(
          Settings(
            Some(organization"rocks.earlyeffect"),
            List(central, repo),
            true,
            Resolution.Lock,
            Promote.CopyIndex,
            Some(mit),
            Some(who),
            CoverageJob.Ask(CoverageFloors(statements = Some(80))),
          )
        ),
        replaced(jvm("core")).map(_.ci.timeout) == Right(JobTimeout.default),
        kept(jvm("core")).map(_.ci.timeout) == Right(JobTimeout.Minutes(12)),
        Projects(Splits.of(Axis.Platform, Axis.Platform), jvm("core")).map(_.splits.axes) == Right(Set(Axis.Platform)),
        Projects().map(_.projects) == Right(Nil),
      )
    ,
    test("a ship row keeps its number, and a bad row is that build error"):
      val core   = jvm("core")
      val number = ship"1.4.2"
      val row    = Ship("histos")
        .next(1, 4, 2)(core)
        .description(pom"library")
        .url(url"https://example.com/histos")
      val root = aggregate("root", core)
      assertTrue(
        shipNumber(Projects(ships(row), core)) == Right(number),
        shipText(Projects(ships(row), core)) == Right(("library", "https://example.com/histos")),
        fault(Projects(ships(Ship("histos").next(1, 4, 2)(core), Ship("histos").next(1, 4, 3)(core)), core)) ==
          "DuplicateShip",
        fault(Projects(ships(Ship("histos").next(1, 4, 2)(core, core)), core)) == "RepeatedShipMember",
        fault(Projects(ships(Ship("left").next(1, 4, 2)(core), Ship("right").next(1, 4, 2)(core)), core)) ==
          "MemberInTwoShips",
        fault(Projects(ships(Ship("histos").next(1, 4, 2)(root)), root, core)) == "AggregateShipMember",
        fault(Projects(ships(Ship("histos").next(1, 4, 2)(jvm("ghost"))), core)) == "MissingProject",
        fault(Projects(ships(ShipDef.line("", number, ::(core, Nil), here)), core)) == "EmptyShipName",
        fault(Projects(ships(ShipDef.line("1bad", number, ::(core, Nil), here)), core)) == "BadShipName",
      )
    ,
    test("an unnamed tool catalog is the default, and tools replaces that catalog"):
      val replaced = ToolCatalog.default.copy(
        compilers = ToolCatalog.default.compilers.copy(semanticdb = ToolRevision.trusted("9.8.7"))
      )
      assertTrue(
        Projects().map(_.tools) == Right(ToolCatalog.default),
        Projects.tools(replaced)().map(_.tools) == Right(replaced),
      )
    ,
    test("a fixed build is the graph it was given"):
      val graph = ProjectGraph.trusted(Nil)
      assertTrue(HistosBuild.fixed(graph).graph == Right(graph)),
  )

  private def jvm(name: String) = module(name, Jvm)(scala3)

  private def js(name: String) = module(name, Js)(scala3)

  private def both(name: String) = module(name, Jvm, Js)(scala3)

  private def fault(built: Either[BuildError, ProjectGraph]): String =
    built match
      case Left(err) => err.productPrefix
      case Right(_)  => "graph"

  private def cyclePlatform(built: Either[BuildError, ProjectGraph]): Option[Platform] =
    built match
      case Left(BuildError.Cycle(platform, _)) => Some(platform)
      case _                                   => None

  private def cycleCloses(built: Either[BuildError, ProjectGraph]): Boolean =
    built match
      case Left(BuildError.Cycle(_, first :: rest)) =>
        rest.lastOption.contains(first) && rest.nonEmpty
      case _ => false

  private def modules(graph: ProjectGraph): List[Module] =
    graph.projects.collect { case found: Module => found }

  private def named(graph: ProjectGraph, name: String): Option[Module] =
    modules(graph).find(_.name.value == name)

  private def resourcesOf(name: String)(graph: ProjectGraph): List[GeneratedAsset] =
    named(graph, name).toList.flatMap(_.resources)

  private def upstream(name: String, platform: Platform)(graph: ProjectGraph): List[String] =
    named(graph, name).toList.flatMap(_.upstream(platform).map(_.value))

  private def edgePlatforms(name: String)(graph: ProjectGraph): List[Option[List[Platform]]] =
    named(graph, name).toList.flatMap(_.dependsOn.map(_.platforms))

  private def chainOf(name: String)(graph: ProjectGraph): List[String] =
    named(graph, name).toList.flatMap: found =>
      Module.chain(found, modules(graph), Jvm).map(_.name.value)

  private def closureOf(name: String)(graph: ProjectGraph): List[String] =
    named(graph, name).toList.flatMap: found =>
      Module.closure(modules(graph), Set(found.name)).map(_.name.value)

  private def clangOf(name: String)(graph: ProjectGraph): Option[ClangPin] =
    named(graph, name).map(_.clang)

  private def publicationOf(name: String)(graph: ProjectGraph): Option[Publication] =
    named(graph, name).map(_.publication)

  private def assetOf(name: String)(graph: ProjectGraph): Option[plugin.RepoPath] =
    named(graph, name).flatMap(_.asset)

  private def needsOf(name: String)(graph: ProjectGraph): Set[Need] =
    named(graph, name).map(_.needs).getOrElse(Set.empty)

  private def storeOf(name: String)(graph: ProjectGraph): TestStore =
    named(graph, name).map(_.testStore).getOrElse(TestStore.Off)

  private def testOf(name: String)(graph: ProjectGraph): Option[String] =
    named(graph, name).flatMap(_.test.map(_.value))

  private def providerOf(name: String)(graph: ProjectGraph): Boolean =
    named(graph, name).exists(_.provider.contains(TestProvider.Zio))

  private def canonicalOf(name: String)(graph: ProjectGraph): String =
    named(graph, name).map(_.flags.canonical).getOrElse("")

  private def jsOf(name: String)(graph: ProjectGraph): (String, String, Option[String]) =
    named(graph, name) match
      case Some(found) => (found.jsEnv.token, found.jsModule.token, found.jsVersion.map(_.value))
      case None        => ("", "", None)

  private def baseOf(name: String)(graph: ProjectGraph): String =
    named(graph, name).map(_.baseDirectory).getOrElse("")

  private final case class Settings(
      organization: Option[Organization],
      resolvers: List[Resolver],
      logged: Boolean,
      resolution: Resolution,
      promote: Promote,
      license: Option[License],
      developer: Option[Developer],
      coverage: CoverageJob,
  )

  private def settingsOf(graph: ProjectGraph): Settings =
    Settings(
      graph.organization,
      graph.resolvers,
      graph.testLog,
      graph.resolution,
      graph.promote,
      graph.facts.license,
      graph.facts.developer,
      graph.coverage,
    )

  private def shipLine(graph: ProjectGraph): Option[version.Ship.Line] =
    graph.catalog.ships.collectFirst { case line: version.Ship.Line => line }

  private def shipNumber(built: Either[BuildError, ProjectGraph]): Either[BuildError, version.ShipNumber] =
    built.flatMap(graph => shipLine(graph).map(_.number).toRight(BuildError.EmptyShipName(here)))

  private def shipText(built: Either[BuildError, ProjectGraph]): Either[BuildError, (String, String)] =
    built.flatMap: graph =>
      shipLine(graph) match
        case Some(line) =>
          (line.description.map(_.value), line.url.map(_.value)) match
            case (Some(text), Some(site)) => Right((text, site))
            case _                        => Left(BuildError.EmptyShipName(here))
        case None => Left(BuildError.EmptyShipName(here))

end GraphShapeLawSpec
