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
        Dep.from("org.example:demo:1.+") == Left(DepError.Ranged("org.example:demo:1.+")),
        Dep.from("org.example:demo:latest.release") == Left(DepError.Ranged("org.example:demo:latest.release")),
        Dep.from("org.example:demo:[1.0,2.0)") == Left(DepError.Ranged("org.example:demo:[1.0,2.0)")),
        Dep.from("dev.zio::zio:2.1.0-RC1").isRight,
        Dep.from("com.google.guava:guava:33.8.0-jre").isRight,
        DepError.Empty.diagnose.check.value == "dep.empty",
        DepError.BadShape("dev.zio:zio").diagnose.check.value == "dep.shape",
        DepError.Ranged("org.example:demo:1.+").diagnose.check.value == "dep.ranged",
        DepError.Empty.diagnose.level == Level.Error,
        DepError.BadShape("x").diagnose.level == Level.Error,
        DepError.Ranged("1.+").diagnose.level == Level.Error,
      )
    ,
    test("a platform selection is those platforms, compile-only stays off the pom, and provided stays off the runtime"):
      val dep      = dep"dev.zio::zio:2.1.26"
      val chosen   = dep.on(Jvm, Jvm, Js).compileOnly
      val supplied = dep.provided
      val listed   = List(dep, chosen, supplied)
      assertTrue(
        dep.applies(Native),
        !chosen.applies(Native),
        chosen.applies(Jvm),
        chosen.applies(Js),
        chosen.platforms == Some(List(Jvm, Js)),
        chosen.link == DepLink.CompileOnly,
        supplied.link == DepLink.Provided,
        Dep.select(listed, Jvm) == listed,
        Dep.select(listed, Native) == List(dep, supplied),
        Dep.runtime(listed, Jvm) == List(dep, chosen),
        Dep.runtime(listed, Native) == List(dep),
        Dep.declared(listed, Jvm) == List(dep, supplied),
        Dep.declared(listed, Native) == List(dep, supplied),
        Dep.declared(List(chosen.excluding(organization"org.example", artifact"left-out")), Jvm).isEmpty,
        supplied.module("zio-streams").link == DepLink.Published,
      )
    ,
    test("a module copies organization, kind, and revision, and clears the other fields"):
      val root = dep"io.get-coursier::coursier:2.1.26"
        .on(Jvm)
        .compileOnly
        .excluding(organization"org.codehaus.plexus", artifact"plexus-archiver")
        .sbt(sbt"2")
      val member  = root.module("coursier-cache")
      val forward = member
        .excluding(organization"org.tukaani", artifact"xz")
        .excluding(organization"org.codehaus.plexus", artifact"plexus-utils")
      val backward = member
        .excluding(organization"org.codehaus.plexus", artifact"plexus-utils")
        .excluding(organization"org.tukaani", artifact"xz")
      assertTrue(
        member.organization == root.organization,
        member.name == "coursier-cache",
        member.revision == root.revision,
        member.kind == DepKind.Binary,
        member.platforms.isEmpty,
        member.link == DepLink.Published,
        member.exclusions.isEmpty,
        member.sbtSeries.isEmpty,
        forward == backward,
        root.platforms == Some(List(Jvm)),
        root.link == DepLink.CompileOnly,
        root.exclusions == List(DepExclusion(organization"org.codehaus.plexus", artifact"plexus-archiver")),
        root.sbtSeries.contains(sbt"2"),
        root.module("sbt-other").sbt(sbt"1").sbtSeries.contains(sbt"1"),
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
        plugin.PluginCoordinate.Module(ProjectName("core")).render == "core",
      ),
  )

  private def seen(dep: Dep): (String, String, String, DepKind, DepLink) =
    (dep.organization, dep.name, dep.revision.value, dep.kind, dep.link)
end DepLawSpec
