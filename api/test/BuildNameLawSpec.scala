package histos

import zio.test.*

object BuildNameLawSpec extends ZIOSpecDefault:
  def spec = suite("build name laws")(
    test("an organization is dotted segments, and a project name is one segment"):
      val group = Organization.from("rocks.early-effect")
      assertTrue(
        group.map(_.value) == Right("rocks.early-effect"),
        group.map(_.segments) == Right(List("rocks", "early-effect")),
        Organization.from("") == Left(OrganizationError.Bad("")),
        Organization.from("rocks.") == Left(OrganizationError.Bad("rocks.")),
        Organization.from(".rocks") == Left(OrganizationError.Bad(".rocks")),
        Organization.from("rocks..effect") == Left(OrganizationError.Bad("rocks..effect")),
        ProjectName.from("histos_api-2").map(_.value) == Right("histos_api-2"),
        ProjectName.from("") == Left(ProjectNameError.Empty),
        ProjectName.from("1core") == Left(ProjectNameError.Bad("1core")),
        ProjectName.from("core.name") == Left(ProjectNameError.Bad("core.name")),
      )
    ,
    test("an artifact rejects dotdot, and a project path stays inside the repository"):
      assertTrue(
        ArtifactBase.from("histos-api_3").map(_.value) == Right("histos-api_3"),
        ArtifactBase.from("") == Left(ArtifactBaseError.Bad("")),
        ArtifactBase.from("1api") == Left(ArtifactBaseError.Bad("1api")),
        ArtifactBase.from("his..api") == Left(ArtifactBaseError.Bad("his..api")),
        ProjectPath.from("modules/core").map(_.value) == Right("modules/core"),
        ProjectPath.from("") == Left(ProjectPathError.Empty),
        ProjectPath.from("/core") == Left(ProjectPathError.Escapes("/core")),
        ProjectPath.from("core\\src") == Left(ProjectPathError.Escapes("core\\src")),
        ProjectPath.from("c:core") == Left(ProjectPathError.Escapes("c:core")),
        ProjectPath.from("..") == Left(ProjectPathError.Escapes("..")),
        ProjectPath.from(".git") == Left(ProjectPathError.Escapes(".git")),
      )
    ,
    test("a main, a test main, and a framework are java binary names"):
      val kept = "histos.Client"
      assertTrue(
        MainClass.from(kept).map(_.value) == Right(kept),
        MainClass.from("") == Left(MainClassError.Empty),
        MainClass.from("1a") == Left(MainClassError.Bad("1a")),
        MainClass.from("a-b") == Left(MainClassError.Bad("a-b")),
        TestMain.from(kept).map(_.value) == Right(kept),
        TestMain.from("") == Left(TestMainError.Empty),
        TestMain.from("a-b") == Left(TestMainError.Bad("a-b")),
        FrameworkClass.from(kept).map(_.value) == Right(kept),
        FrameworkClass.from("") == Left(FrameworkClassError.Empty),
        FrameworkClass.from("a-b") == Left(FrameworkClassError.Bad("a-b")),
      )
    ,
    test("a provider library is a non-empty pair, and zio names that framework"):
      assertTrue(
        ProviderLibrary.from("dev.zio", "zio-test-sbt").map(library => (library.organization, library.name)) ==
          Right(("dev.zio", "zio-test-sbt")),
        ProviderLibrary.from("", "zio") == Left(ProviderLibraryError.Empty),
        ProviderLibrary.from("dev zio", "zio") == Left(ProviderLibraryError.Bad("dev zio", "zio")),
        TestProvider(FrameworkClass.trusted("a.B"), "dev.zio", "zio-test").map(_.framework.value) == Right("a.B"),
        TestProvider(FrameworkClass.trusted("a.B"), "", "zio") == Left(ProviderLibraryError.Empty),
        TestProvider.Zio.framework.value == "zio.test.sbt.ZTestFramework",
        TestProvider.Zio.library.organization == "dev.zio",
        TestProvider.Zio.library.name == "zio-test-sbt",
      )
    ,
    test("a repository url is https or file, and central is the central token"):
      assertTrue(
        RepositoryUrl.from("https://example.com/repo").map(_.value) == Right("https://example.com/repo"),
        RepositoryUrl.from("file:/repo").map(_.value) == Right("file:/repo"),
        RepositoryUrl.from("https://") == Left(RepositoryUrlError.Bad("https://")),
        RepositoryUrl.from("file:") == Left(RepositoryUrlError.Bad("file:")),
        RepositoryUrl.from("http://example.com") == Left(RepositoryUrlError.Bad("http://example.com")),
        RepositoryUrl.from("https://example.com/a b") == Left(RepositoryUrlError.Bad("https://example.com/a b")),
        central == Resolver.Central,
        Resolver.Maven(RepositoryUrl.trusted("https://example.com/repo")) != Resolver.Central,
      ),
  )
