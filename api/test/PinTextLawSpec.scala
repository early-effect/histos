package histos

import zio.test.*

object PinTextLawSpec extends ZIOSpecDefault:
  def spec = suite("pin text laws")(
    test("a project name is one letter-led segment, and the error names which failure"):
      assertTrue(
        ProjectName.from("a").map(_.value) == Right("a"),
        ProjectName.from("core-").map(_.value) == Right("core-"),
        ProjectName.from("_core") == Left(ProjectNameError.Bad("_core")),
        ProjectName.from("core name") == Left(ProjectNameError.Bad("core name")),
        ProjectNameError.Empty.message == "a project name is not empty",
        ProjectNameError.Bad("core name").message == "'core name' is not a project name",
      )
    ,
    test("a toolchain revision accepts a qualifier and a leading zero, and the two errors do not share a sentence"):
      val qualified = "1.22.0-RC1"
      val build     = "1.22.0+build.1"
      val broken    = "1.22.0-"
      assertTrue(
        JsVersion.from(qualified).map(_.value) == Right(qualified),
        JsVersion.from(build).map(_.value) == Right(build),
        JsVersion.from("01.22.0").map(_.value) == Right("01.22.0"),
        JsVersion.from(broken) == Left(JsVersionError.NotExact(broken)),
        JsVersion.from("1.22.0-RC_1") == Left(JsVersionError.NotExact("1.22.0-RC_1")),
        ToolRevision.from(qualified).map(_.value) == Right(qualified),
        ToolRevision.from(build).map(_.value) == Right(build),
        ToolRevision.from(broken) == Left(ToolRevisionError.NotExact(broken)),
        JsVersionError.Empty.message == "a Scala.js toolchain revision is not empty",
        JsVersionError.NotExact(broken).message == s"'$broken' is not an exact Scala.js toolchain revision",
        ToolRevisionError.Empty.message == "a build tool revision is not empty",
        ToolRevisionError.NotExact("21").message == "'21' is not an exact build tool revision",
      )
    ,
    test("a timeout, a node floor, and a node path each say why they were refused"):
      assertTrue(
        JobTimeoutError(0).message == "a job timeout is between 1 and 360 minutes, not 0",
        JobTimeoutError(361).message == "a job timeout is between 1 and 360 minutes, not 361",
        NodeFloorError.NotPositive(0).message == "0 is not a Node major",
        NodeFloorError.NotPositive(-2).message == "-2 is not a Node major",
        NodePathError.Empty.message == "is empty",
        NodePathError.Relative("./node").message == "is not absolute: ./node",
        NodePathError.NotBinary("/usr/bin/Node").message == "is not a node binary: /usr/bin/Node",
        NodePathError.Invalid("\u0000").message == "is not a path: \u0000",
      )
    ,
    test("a framework is a java binary name, a provider library is a non-empty pair, and zio names that framework"):
      val framework = FrameworkClass.trusted("zio.test.ZTestFramework")
      assertTrue(
        FrameworkClass.from("histos.Client").map(_.value) == Right("histos.Client"),
        FrameworkClass.from("_Foo").map(_.value) == Right("_Foo"),
        FrameworkClass.from("a.b$c").map(_.value) == Right("a.b$c"),
        FrameworkClass.from("") == Left(FrameworkClassError.Empty),
        FrameworkClass.from("a-b") == Left(FrameworkClassError.Bad("a-b")),
        FrameworkClass.from("a..b") == Left(FrameworkClassError.Bad("a..b")),
        FrameworkClass.from("a.$b") == Left(FrameworkClassError.Bad("a.$b")),
        FrameworkClassError.Empty.message == "a test framework is not empty",
        FrameworkClassError.Bad("a-b").message == "'a-b' is not a test framework",
        ProviderLibrary.from("dev.zio", "zio-test-sbt").map(library => (library.organization, library.name)) ==
          Right(("dev.zio", "zio-test-sbt")),
        ProviderLibrary.from("", "zio") == Left(ProviderLibraryError.Empty),
        ProviderLibrary.from("dev.zio", "") == Left(ProviderLibraryError.Empty),
        ProviderLibrary.from("dev zio", "zio") == Left(ProviderLibraryError.Bad("dev zio", "zio")),
        ProviderLibrary.from("dev.zio", "zio test") == Left(ProviderLibraryError.Bad("dev.zio", "zio test")),
        ProviderLibrary.from("  ", "zio") == Left(ProviderLibraryError.Bad("  ", "zio")),
        ProviderLibraryError.Empty.message == "a test provider library is not empty",
        ProviderLibraryError
          .Bad("dev.zio", "zio test")
          .message == "'dev.zio' 'zio test' is not a test provider library",
        TestProvider(framework, "dev.zio", "zio-test").map(provider =>
          (provider.framework.value, provider.library.organization, provider.library.name)
        ) == Right(("zio.test.ZTestFramework", "dev.zio", "zio-test")),
        TestProvider(framework, "dev.zio", "zio test") == Left(ProviderLibraryError.Bad("dev.zio", "zio test")),
        TestProvider(framework, "", "zio") == Left(ProviderLibraryError.Empty),
        TestProvider.Zio.framework.value == "zio.test.sbt.ZTestFramework",
        TestProvider.Zio.library.organization == "dev.zio",
        TestProvider.Zio.library.name == "zio-test-sbt",
      ),
  )
