package histos.publish

import histos.central.Publisher
import histos.jdk.{JdkMajor, JdkVersion}
import histos.resolve.{RepoUrl, Repositories, Repository, Resolve, ResolveCell}
import histos.version.{ManualVersion, ShipNumber}
import histos.zinc.{CompileRequest, CompileToolchain, JvmCompile, ResolvedJdk, ScalacFlags}
import histos.{Dep, Jvm, Organization, Platform, ProjectName, Projects, ScalaVersion, module}

import java.nio.file.{Files, Path}
import java.util.zip.ZipFile

import scala.jdk.CollectionConverters.*

import zio.{Scope, ZIO, durationInt}
import zio.test.*

object PublishSpec extends ZIOSpecDefault:
  private val jdk: ResolvedJdk =
    val home = Path.of(java.lang.System.getProperty("java.home"))
    val text = if Files.isRegularFile(home.resolve("release")) then Files.readString(home.resolve("release")) else ""
    JdkVersion.fromRelease(text) match
      case Right(version) => ResolvedJdk(home, version)
      case Left(_)        => ResolvedJdk(home, JdkVersion.trusted("21.0.2", JdkMajor.trusted(21)))

  private val publish = suite("local publish")(
    test("a manual library is a jar a second project can compile against"):
      ZIO.scoped:
        for
          root <- temp
          repo <- temp
          _ <- ZIO.attempt {
                 write(root, "core/src/main/scala/demo/Hello.scala", "package demo\n\nobject Hello {\n  def greet: String = \"hi\"\n}\n")
                 write(root, "app/src/main/scala/App.scala", "object App {\n  def text: String = demo.Hello.greet\n}\n")
                 write(root, ".histos/dependencies.lock", "cell core 3.10.0 jvm\ncell app 3.10.0 jvm\n")
               }
          built <- ZIO.fromEither(library).mapError(error => new IllegalStateException(error.message))
          lines <- LocalPublish.run(root, built, Nil, jdk, Map.empty).provide(Resolve.live(root.resolve("coursier")), Publisher.local(repo))
          appPom <- ZIO.attempt(Files.readString(repo.resolve("com/example/app_3/1.0.0/app_3-1.0.0.pom")))
          names <- ZIO.attempt(zipNames(repo.resolve("com/example/core_3/1.0.0/core_3-1.0.0.jar")))
          used <- consume(repo, root.resolve("use"))
        yield assertTrue(
          lines.map(line => s"${line.group.value} ${line.artifact} ${line.revision}") == List(
            "com.example app_3 1.0.0",
            "com.example core_3 1.0.0",
          ),
          names.contains("demo/Hello.class"),
          names.contains("demo/Hello.tasty"),
          appPom.contains("<artifactId>core_3</artifactId>"),
          appPom.contains("<version>1.0.0</version>"),
          used,
        )
    ,
    test("development does not publish, and a catalog without an organization does not write"):
      ZIO.scoped:
        for
          root <- temp
          repo <- temp
          develop <- ZIO.fromEither(development).mapError(error => new IllegalStateException(error.message))
          plain <- ZIO.fromEither(unorganized).mapError(error => new IllegalStateException(error.message))
          held <- LocalPublish.run(root, develop, Nil, jdk, Map.empty).provide(Resolve.live(root.resolve("coursier")), Publisher.local(repo)).either
          missing <- LocalPublish.run(root, plain, Nil, jdk, Map.empty).provide(Resolve.live(root.resolve("coursier")), Publisher.local(repo)).either
          wrote <- ZIO.attempt(Option(repo.toFile.list()).map(_.toList).getOrElse(Nil))
        yield assertTrue(
          held match
            case Left(LocalPublishError.Development(ship, _)) => ship.value == "libs"
            case _                                            => false
          ,
          missing == Left(LocalPublishError.NoOrganization),
          wrote.isEmpty,
        )
    ,
  )

  private def consume(repo: Path, work: Path): ZIO[Scope, Throwable, Boolean] =
    val version = ScalaVersion("3.10.0")
    val dependency = Dep("com.example::core:1.0.0")
    ZIO.fromEither(RepoUrl.from(repo.toUri.toASCIIString)).mapError(error => new IllegalStateException(error.message)).flatMap: url =>
      ZIO.fromEither(ProjectName.from("use")).mapError(error => new IllegalStateException(error.message)).flatMap: name =>
        val cell = ResolveCell(name, List(dependency), version, Platform.Jvm, Repositories(Repository.Maven(url), Nil))
        for
          _ <- ZIO.attempt(write(work, "Use.scala", "object Use {\n  def text: String = demo.Hello.greet\n}\n"))
          classpath <- ZIO.serviceWithZIO[Resolve](_.resolve(cell)).provide(Resolve.live(work.resolve("coursier"))).mapError(error => new IllegalStateException(error.message))
          compiled <- JvmCompile.compile(CompileRequest(
                        CompileToolchain(version, Platform.Jvm, jdk, ScalacFlags.empty),
                        List(work.resolve("Use.scala")),
                        classpath.artifacts.map(_.path),
                        work.resolve("classes"),
                        work.resolve("analysis"),
                        Nil,
                      )).mapError(error => new IllegalStateException(error.message))
        yield Files.isRegularFile(compiled.classes.resolve("Use.class"))

  private val library =
    val scalaVersion = ScalaVersion("3.10.0")
    val core = module("core", Jvm)(scalaVersion)
    val app = module("app", Jvm)(scalaVersion).dependsOn(core)
    Projects.organization(Organization("com.example"))(histos.ships(histos.manual("libs", ManualVersion("1.0.0"))(core, app)), core, app)

  private val development =
    val core = module("core", Jvm)(ScalaVersion("3.10.0"))
    Projects.organization(Organization("com.example"))(histos.ships(histos.ship("libs", ShipNumber("1.0.0"))(core)), core)

  private val unorganized =
    val core = module("core", Jvm)(ScalaVersion("3.10.0"))
    Projects(histos.ships(histos.manual("libs", ManualVersion("1.0.0"))(core)), core)

  private def zipNames(jar: Path): List[String] =
    val zip = new ZipFile(jar.toFile)
    try zip.entries.asScala.map(_.getName).toList
    finally zip.close()

  private def write(root: Path, name: String, text: String): Unit =
    val path = root.resolve(name)
    Files.createDirectories(path.getParent)
    Files.writeString(path, text)

  private def temp: ZIO[Scope, Throwable, Path] =
    ZIO.acquireRelease(ZIO.attempt(Files.createTempDirectory("histos-publish"))): dir =>
      ZIO.attempt {
        if Files.exists(dir) then
          val walk = Files.walk(dir)
          val paths =
            try walk.iterator.asScala.toList
            finally walk.close()
          paths.sortBy(_.getNameCount).reverse.foreach(path => Files.deleteIfExists(path))
      }.ignore

  def spec = publish @@ TestAspect.sequential @@ TestAspect.timeout(3.minutes)
