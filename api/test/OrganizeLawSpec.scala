package histos

import histos.version.ShipNumber

import zio.test.*

object OrganizeLawSpec extends ZIOSpecDefault:
  private val scala3  = scala"3.10.0"
  private val core    = module("core", Jvm)(scala3)
  private val number  = ShipNumber.trusted(1, 4, 2)
  private val here    = SourcePos("law", 1, 1)
  private val row     = ShipDef.line("core", number, ::(core, Nil), here)
  private val catalog = CatalogSpec(row)
  private val splits  = Splits.of(Axis.Platform)
  private val uses    = Uses(Nil)
  private val org     = organization"rocks.earlyeffect"
  private val kept    = Projects.testLog.organization(org)

  def spec = suite("organize laws")(
    test("test logs, a lock, and a replaced organization survive into the graph"):
      assertTrue(
        read(Projects.testLog(core)) == Some(Shape(true, None, Set.empty, Nil, false)),
        read(Projects.locked(core)) == Some(Shape(false, None, Set.empty, Nil, true)),
        read(Projects.organization(org).organization(organization"histos.law")(core)) ==
          Some(Shape(false, Some("histos.law"), Set.empty, Nil, false)),
      )
    ,
    test("each projects entry keeps the splits, the catalog, and the uses it was given"):
      assertTrue(
        read(Projects(splits, core)) == Some(Shape(false, None, Set(Axis.Platform), Nil, false)),
        read(Projects(catalog, core)) == Some(Shape(false, None, Set.empty, List("core"), false)),
        read(Projects(splits, catalog, core)) == Some(Shape(false, None, Set(Axis.Platform), List("core"), false)),
        read(Projects(uses, core)) == Some(Shape(false, None, Set.empty, Nil, false)),
        read(Projects(uses, splits, core)) == Some(Shape(false, None, Set(Axis.Platform), Nil, false)),
        read(Projects(uses, catalog, core)) == Some(Shape(false, None, Set.empty, List("core"), false)),
        read(Projects(uses, splits, catalog, core)) ==
          Some(Shape(false, None, Set(Axis.Platform), List("core"), false)),
      )
    ,
    test("an organized entry keeps the test log and the organization under every overload"):
      val expect = Shape(true, Some(org.value), Set(Axis.Platform), List("core"), false)
      val axes   = Shape(true, Some(org.value), Set(Axis.Platform), Nil, false)
      val ships  = Shape(true, Some(org.value), Set.empty, List("core"), false)
      val plain  = Shape(true, Some(org.value), Set.empty, Nil, false)
      assertTrue(
        read(kept(splits, core)) == Some(axes),
        read(kept(catalog, core)) == Some(ships),
        read(kept(splits, catalog, core)) == Some(expect),
        read(kept(uses, core)) == Some(plain),
        read(kept(uses, splits, core)) == Some(axes),
        read(kept(uses, catalog, core)) == Some(ships),
        read(kept(uses, splits, catalog, core)) == Some(expect),
      )
    ,
    test("a test dependency is not a compile dependency, and the artifact id is the one named"):
      val library = Dep("dev.zio::zio-test:2.1.27")
      val built   = module("core", Jvm)(scala3).testDependencies(library).artifact(ArtifactBase("core-api"))
      val modules = Projects(built).map(_.projects.collect { case item: Module => item })
      assertTrue(
        modules.map(_.map(_.dependencies)) == Right(List(Nil)),
        modules.map(_.map(_.testDependencies)) == Right(List(List(library))),
        modules.map(_.flatMap(_.published.map(_.value))) == Right(List("core-api")),
      )
    ,
    test("a cycle through the first dependency is that cycle, and the extra sibling does not replace it"):
      val a = module("a", Jvm)(scala3)
      val b = module("b", Jvm)(scala3)
      val c = module("c", Jvm)(scala3)
      assertTrue(
        Projects(a.dependsOn(b, c), b.dependsOn(a), c) ==
          Left(BuildError.Cycle(Platform.Jvm, List(ProjectName("a"), ProjectName("b"), ProjectName("a"))))
      ),
  )

  private def read(built: Either[BuildError, ProjectGraph]): Option[Shape] =
    built.toOption.map: graph =>
      Shape(
        graph.testLog,
        graph.organization.map(_.value),
        graph.splits.axes,
        graph.catalog.ships.map(_.name.value),
        graph.resolution == Resolution.Lock,
      )
end OrganizeLawSpec

final case class Shape(
    logged: Boolean,
    organization: Option[String],
    axes: Set[Axis],
    ships: List[String],
    locked: Boolean,
)
