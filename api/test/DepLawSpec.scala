package histos

import histos.doctor.Level

import zio.test.*

object DepLawSpec extends ZIOSpecDefault:
  def spec = suite("dependency laws")(
    test("a coordinate is java, binary, or full, and each bad shape is that error"):
      val java   = Dep.from("dev.zio:zio:2.1.26")
      val binary = Dep.from("dev.zio::zio:2.1.26")
      val full   = Dep.from("dev.zio:::zio:2.1.26")
      assertTrue(
        java.map(seen) == Right(("dev.zio", "zio", "2.1.26", DepKind.Java, DepLink.Published)),
        binary.map(seen) == Right(("dev.zio", "zio", "2.1.26", DepKind.Binary, DepLink.Published)),
        full.map(seen) == Right(("dev.zio", "zio", "2.1.26", DepKind.Full, DepLink.Published)),
        Dep.from("") == Left(DepError.Empty),
        Dep.from("dev.zio zio:2") == Left(DepError.BadShape("dev.zio zio:2")),
        Dep.from("dev.zio:zio") == Left(DepError.BadShape("dev.zio:zio")),
        Dep.from("dev.zio::zio") == Left(DepError.BadShape("dev.zio::zio")),
        Dep.from("::zio:2") == Left(DepError.BadShape("::zio:2")),
        Dep.from("dev.zio:zio:2:extra") == Left(DepError.BadShape("dev.zio:zio:2:extra")),
        DepError.Empty.diagnose.check.value == "dep.empty",
        DepError.BadShape("dev.zio:zio").diagnose.check.value == "dep.shape",
        DepError.Empty.diagnose.level == Level.Error,
        DepError.BadShape("x").diagnose.level == Level.Error,
      )
    ,
    test("a platform selection is those platforms, and compile-only stays off the pom"):
      val dep    = dep"dev.zio::zio:2.1.26"
      val chosen = dep.on(Jvm, Jvm, Js).compileOnly
      val listed = List(dep, chosen)
      assertTrue(
        dep.applies(Native),
        !chosen.applies(Native),
        chosen.applies(Jvm),
        chosen.applies(Js),
        chosen.platforms == Some(List(Jvm, Js)),
        chosen.link == DepLink.CompileOnly,
        Dep.select(listed, Native) == List(dep),
        Dep.published(listed, Jvm) == List(dep),
        Dep.published(listed, Native) == List(dep),
      )
    ,
    test("a plugin coordinate renders the same separator the dependency parsed"):
      val java   = Dep.trusted("o", "n", "1", DepKind.Java)
      val binary = Dep.trusted("o", "n", "1", DepKind.Binary)
      val full   = Dep.trusted("o", "n", "1", DepKind.Full)
      assertTrue(
        plugin.PluginCoordinate.Published(java).render == "o:n:1",
        plugin.PluginCoordinate.Published(binary).render == "o::n:1",
        plugin.PluginCoordinate.Published(full).render == "o:::n:1",
        plugin.PluginCoordinate.Path(path"modules/core").render == "modules/core",
      ),
  )

  private def seen(dep: Dep): (String, String, String, DepKind, DepLink) =
    (dep.organization, dep.name, dep.revision, dep.kind, dep.link)
end DepLawSpec
