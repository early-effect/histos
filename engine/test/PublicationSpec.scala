package histos.publish

import histos.central.Publisher
import histos.version.{ManualVersion, ShipName}
import histos.zinc.GraphCompile
import histos.{
  Developer,
  Jvm,
  License,
  Module,
  Organization,
  PomFact,
  Projects,
  Publication,
  ScalaVersion,
  Scm,
  connection,
  developer,
  license,
  module,
  pom,
  url,
}

import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import zio.{Scope, ZIO, durationInt}
import zio.test.*

object PublicationSpec extends ZIOSpecDefault:
  private val licenseValue =
    License(license"Apache-2.0", url"https://www.apache.org/licenses/LICENSE-2.0.txt")
  private val developerValue = Developer(developer"early-effect", pom"Early Effect")
  private val scmValue       =
    Scm(url"https://example.com/source", connection"scm:git:https://example.com/source.git")

  def spec =
    suite("publication")(
      test("a library pom uses the organization, and a ship replaces description and url"):
        ZIO.scoped:
          for
            jdk  <- GraphCompile.jdkHere
            root <- temp
            repo <- temp
            _    <- ZIO.attempt {
              write(root, "core/src/main/scala/demo/Hello.scala", hello)
              write(root, "app/src/main/scala/App.scala", appSource)
              write(root, ".histos/dependencies.lock", "cell core 3.10.0 jvm\ncell app 3.10.0 jvm\n")
            }
            built <- ZIO.fromEither(published)
            _     <- histos.ToolFixture.seed(root, built)
            _     <- LocalPublish
              .run(root, built, Nil, jdk, Map.empty, PublishWrite.Local)
              .provideSomeLayer[histos.resolve.LockedTools](
                histos.ToolFixture.resolver ++ Publisher.local(repo)
              )
            corePom <- ZIO.attempt(Files.readString(repo.resolve("com/example/core_3/1.5.0/core_3-1.5.0.pom")))
            appPom  <- ZIO.attempt(Files.readString(repo.resolve("com/example/app_3/2.0.0/app_3-2.0.0.pom")))
          yield assertTrue(
            built.projects.collect { case item: Module => item.publication } == List(
              Publication.Library,
              Publication.Library,
            ),
            built.catalog.get(ShipName("base")).flatMap(_.description).isEmpty,
            built.catalog.get(ShipName("tool")).flatMap(_.description).map(_.value) == Some("Tool text"),
            corePom.contains("<name>core_3</name>"),
            corePom.contains("<artifactId>core_3</artifactId>"),
            corePom.contains("<version>1.5.0</version>"),
            corePom.contains("<packaging>jar</packaging>"),
            corePom.contains("<description>Org text</description>"),
            corePom.contains("<url>https://example.com/org</url>"),
            !corePom.contains("Tool text"),
            appPom.contains("<name>app_3</name>"),
            appPom.contains("<description>Tool text</description>"),
            appPom.contains("<url>https://example.com/tool</url>"),
            !appPom.contains("Org text"),
            !appPom.contains("<url>https://example.com/org</url>"),
            appPom.contains("<version>2.0.0</version>"),
            appPom.contains(coreDependency),
            corePom.contains("<name>Apache-2.0</name>"),
            corePom.contains("<distribution>repo</distribution>"),
            corePom.contains("<id>early-effect</id>"),
            corePom.contains("<name>Early Effect</name>"),
            corePom.contains("<connection>scm:git:https://example.com/source.git</connection>"),
            corePom.contains("<developerConnection>scm:git:https://example.com/source.git</developerConnection>"),
          )
      ,
      test("a release without a license, a developer, or an scm writes nothing"):
        ZIO.scoped:
          for
            jdk        <- GraphCompile.jdkHere
            root       <- temp
            repo       <- temp
            noLicense  <- ZIO.fromEither(release(_.developer(developerValue).scm(scmValue)))
            noDev      <- ZIO.fromEither(release(_.license(licenseValue).scm(scmValue)))
            noScm      <- ZIO.fromEither(release(_.license(licenseValue).developer(developerValue)))
            none       <- ZIO.fromEither(release(identity))
            licenseGap <- refuse(root, repo, jdk, noLicense)
            devGap     <- refuse(root, repo, jdk, noDev)
            scmGap     <- refuse(root, repo, jdk, noScm)
            allGap     <- refuse(root, repo, jdk, none)
            wrote      <- ZIO.attempt(Option(repo.toFile.list()).map(_.toList).getOrElse(Nil))
          yield assertTrue(
            licenseGap == Left(LocalPublishError.Facts(::(PomFact.License, Nil))),
            devGap == Left(LocalPublishError.Facts(::(PomFact.Developer, Nil))),
            scmGap == Left(LocalPublishError.Facts(::(PomFact.Scm, Nil))),
            allGap == Left(
              LocalPublishError.Facts(::(PomFact.License, List(PomFact.Developer, PomFact.Scm)))
            ),
            wrote.isEmpty,
          ),
    ).provideSomeLayerShared[zio.test.TestEnvironment & zio.Scope](
      histos.ToolFixture.layer(
        histos.Projects(histos.module("library", histos.Jvm)(histos.ScalaVersion("3.10.0")))
      )
    ) @@ TestAspect.withLiveSystem @@ TestAspect.timeout(3.minutes)

  private val hello =
    "package demo\n\nobject Hello {\n  def greet: String = \"hi\"\n}\n"

  private val appSource =
    "object App {\n  def text: String = demo.Hello.greet\n}\n"

  private val coreDependency =
    """    <dependency>
      |      <groupId>com.example</groupId>
      |      <artifactId>core_3</artifactId>
      |      <version>1.5.0</version>
      |    </dependency>""".stripMargin

  private val published =
    val scalaVersion = ScalaVersion("3.10.0")
    val core         = module("core", Jvm)(scalaVersion)
    val app          = module("app", Jvm)(scalaVersion).dependsOn(core)
    val base         = histos.manual("base", ManualVersion("1.5.0"))(core)
    val tool         = histos
      .manual("tool", ManualVersion("2.0.0"))(app)
      .description(pom"Tool text")
      .url(url"https://example.com/tool")
    Projects
      .organization(Organization("com.example"))
      .license(licenseValue)
      .developer(developerValue)
      .scm(scmValue)
      .description(pom"Org text")
      .url(url"https://example.com/org")(
        histos.ships(base, tool),
        core,
        app,
      )

  private def release(fill: histos.Organized => histos.Organized) =
    val core = module("core", Jvm)(ScalaVersion("3.10.0"))
    fill(Projects.organization(Organization("com.example")))(
      histos.ships(histos.manual("libs", ManualVersion("1.0.0"))(core)),
      core,
    )

  private def refuse(
      root: Path,
      repo: Path,
      jdk: histos.jdk.JdkRuntime,
      graph: histos.ProjectGraph,
  ) =
    LocalPublish
      .run(root, graph, Nil, jdk, Map.empty, PublishWrite.Local)
      .provideSomeLayer[histos.resolve.LockedTools](
        histos.ToolFixture.resolver ++ Publisher.local(repo)
      )
      .either

  private def write(root: Path, name: String, text: String): Unit =
    val path = root.resolve(name)
    Files.createDirectories(path.getParent)
    Files.writeString(path, text)

  private def temp: ZIO[Scope, Throwable, Path] =
    ZIO.acquireRelease(ZIO.attempt(Files.createTempDirectory("histos-publication"))): dir =>
      ZIO.attempt {
        if Files.exists(dir) then
          val walk  = Files.walk(dir)
          val paths =
            try walk.iterator.asScala.toList
            finally walk.close()
          paths.sortBy(_.getNameCount).reverse.foreach(path => Files.deleteIfExists(path))
      }.ignore
end PublicationSpec
