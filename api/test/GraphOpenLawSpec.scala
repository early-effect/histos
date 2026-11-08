package histos

import zio.test.*

object GraphOpenLawSpec extends ZIOSpecDefault:
  private val scala3 = scala"3.10.0"
  private val site   = url"https://example.com/histos"
  private val old    = url"https://example.com/old"
  private val tools  = RunnerTools(
    Some(NodeFloor(25)),
    Some(ToolRevision("21.0.0")),
    Some(ToolRevision("1.56.0")),
    Some(NodePath("/usr/bin/node")),
  )

  def spec = suite("graph open laws")(
    test("splits, a catalog, facts, and runner tools survive one organized build, and lookup is that project"):
      val core   = jvm("core")
      val root   = aggregate("root", core)
      val row    = Ship("histos").next(1, 4, 2)(core).description(pom"ship").url(site)
      val first  = Scm(old, connection"scm:git:git@github.com:early-effect/old.git")
      val second = Scm(site, connection"scm:git:git@github.com:early-effect/histos.git")
      val built  = Projects
        .organization(organization"rocks.earlyeffect")
        .description(pom"library")
        .url(old)
        .scm(first)
        .description(pom"replaced")
        .url(site)
        .scm(second)
        .runnerTools(tools)
        .jobTimeout(JobTimeout.Unbounded)(Splits.of(Axis.Module), ships(row), root, core)
      assertTrue(
        built.map(_.splits.axes) == Right(Set(Axis.Module)),
        built.map(_.organization.map(_.value)) == Right(Some("rocks.earlyeffect")),
        built.map(_.ci.timeout) == Right(JobTimeout.Unbounded),
        built.map(_.runnerTools) == Right(tools),
        built.map(_.facts) == Right(
          OrganizationFacts(
            license = None,
            developer = None,
            scm = Some(second),
            description = Some(pom"replaced"),
            url = Some(site),
          )
        ),
        built.map(shipNumber) == Right(Some("1.4.2")),
        built.map(shipText) == Right(Some(("ship", site.value))),
        built.map(kindOf(_, "core")) == Right(Right(Some("module"))),
        built.map(kindOf(_, "root")) == Right(Right(Some("aggregate"))),
        built.map(kindOf(_, "missing")) == Right(Right(None)),
      )
    ,
    test("an assembly with no asset is still one jvm jar and names no file"):
      val main  = MainClass("app.Main")
      val built = Projects(jvm("app").assembly(main))
      assertTrue(built.map(assemblyOf) == Right(Some((Publication.Assembly(main), None)))),
  )

  private def jvm(name: String) = module(name, Jvm)(scala3)

  private def kindOf(graph: ProjectGraph, raw: String): Either[ProjectNameError, Option[String]] =
    ProjectName
      .from(raw)
      .map: name =>
        graph.get(name) match
          case Some(_: Module)    => Some("module")
          case Some(_: Aggregate) => Some("aggregate")
          case None               => None

  private def shipNumber(graph: ProjectGraph): Option[String] =
    graph.catalog.ships.collectFirst { case line: version.Ship.Line => line.number.render }

  private def shipText(graph: ProjectGraph): Option[(String, String)] =
    graph.catalog.ships
      .collectFirst { case line: version.Ship.Line =>
        (line.description.map(_.value), line.url.map(_.value))
      }
      .flatMap:
        case (Some(text), Some(found)) => Some((text, found))
        case _                         => None

  private def assemblyOf(graph: ProjectGraph): Option[(Publication, Option[plugin.RepoPath])] =
    graph.projects.collectFirst:
      case module: Module if module.name.value == "app" => (module.publication, module.asset)
end GraphOpenLawSpec
