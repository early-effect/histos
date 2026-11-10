package histos

import histos.plugin.{LinkStrategy, PluginCoordinate}

import zio.test.*

object PluginSourceLawSpec extends ZIOSpecDefault:
  private val scala3 = scala"3.10.0"

  def spec = suite("plugin coordinates")(
    test("a module coordinate is that module, and another module may depend on it"):
      val plugins  = module("plugins", Jvm)(scala3)
      val core     = module("core", Jvm)(scala3).dependsOn(plugins)
      val built    = Projects.plugins(PluginSources.module(plugins))(plugins, core)
      val depended = built.toOption
        .flatMap(_.get(ProjectName("core")))
        .collect:
          case found: Module => found.dependsOn.map(_.name)
      assertTrue(
        built.map(_.coordinates) == Right(List(PluginCoordinate.Module(ProjectName("plugins")))),
        depended.contains(List(ProjectName("plugins"))),
      )
    ,
    test("a ship pin keeps its link, and the default is binary or source"):
      val core   = module("core", Jvm)(scala3)
      val pin    = dep"rocks.earlyeffect::demo:1.0.0"
      val linked =
        Projects.plugins(PluginSources.ship(pin).link(LinkStrategy.ExactBinary))(core)
      val plain = Projects.plugins(PluginSources.ship(pin))(core)
      (linked, plain) match
        case (Right(exact), Right(either)) =>
          assertTrue(
            exact.coordinates == List(PluginCoordinate.Published(pin, LinkStrategy.ExactBinary)),
            either.coordinates == List(PluginCoordinate.Published(pin)),
          )
        case _ => assertTrue(linked.isRight, plain.isRight)
    ,
    test("an unknown module is refused"):
      val core    = module("core", Jvm)(scala3)
      val missing = PluginRef.Module("plugins", SourcePos("law", 1, 1))
      assertTrue(
        Projects.plugins(missing)(core) ==
          Left(BuildError.PluginGraph(PluginGraphFault.UnknownPluginModule("plugins", SourcePos("law", 1, 1))))
      )
    ,
    test("an aggregate is not a plugin module"):
      val core  = module("core", Jvm)(scala3)
      val root  = aggregate("root", core)
      val named = PluginRef.Module("root", SourcePos("law", 2, 1))
      assertTrue(
        Projects.plugins(named)(core, root) ==
          Left(BuildError.PluginGraph(PluginGraphFault.PluginOnAggregate(ProjectName("root"), SourcePos("law", 2, 1))))
      )
    ,
    test("the same module twice is refused"):
      val plugins = module("plugins", Jvm)(scala3)
      val again   = PluginRef.Module("plugins", SourcePos("law", 3, 1))
      assertTrue(
        Projects.plugins(PluginSources.module(plugins), again)(plugins) ==
          Left(
            BuildError.PluginGraph(PluginGraphFault.DuplicatePluginCoordinate("plugins", SourcePos("law", 3, 1)))
          )
      ),
  )
end PluginSourceLawSpec
