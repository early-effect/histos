package histos.publish

import histos.jdk.JdkRuntime
import histos.central.Publisher
import histos.native.NativeOrigin
import histos.resolve.{BuildResolvers, Resolve, ResolveError}
import histos.version.{ManualVersion, ShipNumber}
import histos.zinc.{GraphCompile, GraphCompileError}
import histos.{
  BuildError,
  Dep,
  Js,
  Jvm,
  Organization,
  Projects,
  RepositoryUrl,
  RepositoryUrlError,
  Resolver,
  ScalaVersion,
  module
}

import java.nio.file.{Files, Path}
import java.util.zip.ZipFile

import scala.jdk.CollectionConverters.*

import zio.{Scope, ZIO, durationInt}
import zio.test.*

object PublishSpec extends ZIOSpecDefault:
  private val publish = suite("local publish")(
    test("a manual library is a jar a second project can compile against"):
      ZIO.scoped:
        for
          jdk <- GraphCompile.jdkHere
          root <- temp
          repo <- temp
          _ <- ZIO.attempt {
            write(
              root,
              "core/src/main/scala/demo/Hello.scala",
              "package demo\n\nobject Hello {\n  def greet: String = \"hi\"\n}\n"
            )
            write(
              root,
              "app/src/main/scala/App.scala",
              "object App {\n  def text: String = demo.Hello.greet\n}\n"
            )
            write(
              root,
              ".histos/dependencies.lock",
              "cell core 3.10.0 jvm\ncell app 3.10.0 jvm\n"
            )
          }
          built <- ZIO.fromEither(library)
          _ <- histos.ToolFixture.seed(root, built)
          published <- LocalPublish
            .run(root, built, Nil, jdk, Map.empty)
            .provideSomeLayer[histos.resolve.LockedTools](
              histos.ToolFixture.resolver ++ Publisher.local(repo)
            )
          appPom <- ZIO.attempt(
            Files.readString(
              repo.resolve("com/example/app_3/1.0.0/app_3-1.0.0.pom")
            )
          )
          names <- ZIO.attempt(
            zipNames(repo.resolve("com/example/core_3/1.0.0/core_3-1.0.0.jar"))
          )
          used <- consume(repo, root.resolve("use"), jdk)
        yield assertTrue(
          published.modules.map(line =>
            s"${line.group.value} ${line.artifact} ${line.revision}"
          ) == List(
            "com.example app_3 1.0.0",
            "com.example core_3 1.0.0"
          ),
          names.contains("demo/Hello.class"),
          names.contains("demo/Hello.tasty"),
          appPom.contains("<artifactId>core_3</artifactId>"),
          !appPom.contains("zio-test"),
          appPom.contains("<version>1.0.0</version>"),
          used
        )
    ,
    test("a module on Jvm and Js publishes one coordinate per platform"):
      ZIO.scoped:
        for
          jdk <- GraphCompile.jdkHere
          root <- temp
          repo <- temp
          _ <- ZIO.attempt {
            write(
              root,
              "core/src/main/scala/demo/Hello.scala",
              "package demo\n\nobject Hello {\n  def greet: String = \"hi\"\n}\n"
            )
            write(
              root,
              "app/src/main/scala/App.scala",
              "object App {\n  def text: String = demo.Hello.greet\n}\n"
            )
            write(
              root,
              ".histos/dependencies.lock",
              "cell core 3.10.0 jvm\ncell core 3.10.0 js\ncell app 3.10.0 jvm\ncell app 3.10.0 js\n"
            )
          }
          built <- ZIO.fromEither(crossLibrary)
          _ <- histos.ToolFixture.seed(root, built)
          published <- LocalPublish
            .run(root, built, Nil, jdk, Map.empty)
            .provideSomeLayer[histos.resolve.LockedTools](
              histos.ToolFixture.resolver ++ Publisher.local(repo)
            )
          jvm <- ZIO.attempt(
            zipNames(repo.resolve("com/example/core_3/1.0.0/core_3-1.0.0.jar"))
          )
          js <- ZIO.attempt(
            zipNames(
              repo.resolve(
                "com/example/core_sjs1_3/1.0.0/core_sjs1_3-1.0.0.jar"
              )
            )
          )
          jvmPom <- ZIO.attempt(
            Files.readString(
              repo.resolve("com/example/app_3/1.0.0/app_3-1.0.0.pom")
            )
          )
          jsPom <- ZIO.attempt(
            Files.readString(
              repo.resolve("com/example/app_sjs1_3/1.0.0/app_sjs1_3-1.0.0.pom")
            )
          )
        yield assertTrue(
          published.modules.map(_.artifact) == List(
            "app_3",
            "app_sjs1_3",
            "core_3",
            "core_sjs1_3"
          ),
          jvm.contains("demo/Hello.class"),
          !jvm.exists(_.endsWith(".sjsir")),
          js.exists(_.endsWith(".sjsir")),
          jvmPom.contains("<artifactId>core_3</artifactId>"),
          jsPom.contains("<artifactId>core_sjs1_3</artifactId>"),
          !jsPom.contains("<artifactId>core_3</artifactId>")
        )
    ,
    test(
      "development does not publish, and a catalog without an organization does not write"
    ):
      ZIO.scoped:
        for
          jdk <- GraphCompile.jdkHere
          root <- temp
          repo <- temp
          develop <- ZIO.fromEither(development)
          plain <- ZIO.fromEither(unorganized)
          held <- LocalPublish
            .run(root, develop, Nil, jdk, Map.empty)
            .provideSomeLayer[histos.resolve.LockedTools](
              histos.ToolFixture.resolver ++ Publisher.local(repo)
            )
            .either
          missing <- LocalPublish
            .run(root, plain, Nil, jdk, Map.empty)
            .provideSomeLayer[histos.resolve.LockedTools](
              histos.ToolFixture.resolver ++ Publisher.local(repo)
            )
            .either
          wrote <- ZIO.attempt(
            Option(repo.toFile.list()).map(_.toList).getOrElse(Nil)
          )
        yield assertTrue(
          held match
            case Left(LocalPublishError.Development(ship, _)) =>
              ship.value == "libs"
            case _ => false
          ,
          missing == Left(LocalPublishError.NoOrganization),
          wrote.isEmpty
        )
  )

  /** The second build names the published directory. Resolve and compile use
    * that list, not Central.
    */
  private def consume(repo: Path, work: Path, jdk: JdkRuntime): ZIO[
    Scope & histos.resolve.LockedTools,
    Throwable | RepositoryUrlError | BuildError | ResolveError |
      GraphCompileError | histos.workspace.IoFailure,
    Boolean
  ] =
    val version = ScalaVersion("3.10.0")
    val dependency = Dep("com.example::core:1.0.0")
    ZIO
      .fromEither(RepositoryUrl.from(repo.toUri.toASCIIString))
      .flatMap: url =>
        val use = module("use", Jvm)(version).libraryDependencies(dependency)
        ZIO
          .fromEither(
            Projects.resolvers(Resolver.Maven(url), Resolver.Central)(use)
          )
          .flatMap: graph =>
            val repositories = BuildResolvers(graph.resolvers)
            for
              _ <- ZIO.attempt(
                write(
                  work,
                  "src/main/scala/Use.scala",
                  "object Use {\n  def text: String = demo.Hello.greet\n}\n"
                )
              )
              _ <- Resolve
                .update(work, graph, repositories)
                .provideSomeLayer[histos.resolve.LockedTools](
                  histos.ToolFixture.resolver
                )
              cells <- GraphCompile
                .run(work, graph, Nil, jdk, NativeOrigin.Central)
                .provideSomeLayer[histos.resolve.LockedTools](
                  histos.ToolFixture.resolver
                )
            yield cells.exists(cell =>
              Files.isRegularFile(cell.classes.resolve("Use.class"))
            )

  private val library =
    val scalaVersion = ScalaVersion("3.10.0")
    val core = module("core", Jvm)(scalaVersion)
    val app = module("app", Jvm)(scalaVersion)
      .dependsOn(core)
      .testDependencies(Dep("dev.zio::zio-test:2.1.27"))
    Projects.organization(Organization("com.example"))(
      histos.ships(histos.manual("libs", ManualVersion("1.0.0"))(core, app)),
      core,
      app
    )

  private val crossLibrary =
    val scalaVersion = ScalaVersion("3.10.0")
    val core = module("core", Jvm, Js)(scalaVersion)
    val app = module("app", Jvm, Js)(scalaVersion).dependsOn(core)
    Projects.organization(Organization("com.example"))(
      histos.ships(histos.manual("libs", ManualVersion("1.0.0"))(core, app)),
      core,
      app
    )

  private val development =
    val core = module("core", Jvm)(ScalaVersion("3.10.0"))
    Projects.organization(Organization("com.example"))(
      histos.ships(histos.ship("libs", ShipNumber("1.0.0"))(core)),
      core
    )

  private val unorganized =
    val core = module("core", Jvm)(ScalaVersion("3.10.0"))
    Projects(
      histos.ships(histos.manual("libs", ManualVersion("1.0.0"))(core)),
      core
    )

  private def zipNames(jar: Path): List[String] =
    val zip = new ZipFile(jar.toFile)
    try zip.entries.asScala.map(_.getName).toList
    finally zip.close()

  private def write(root: Path, name: String, text: String): Unit =
    val path = root.resolve(name)
    Files.createDirectories(path.getParent)
    Files.writeString(path, text)

  private def temp: ZIO[Scope, Throwable, Path] =
    ZIO.acquireRelease(
      ZIO.attempt(Files.createTempDirectory("histos-publish"))
    ): dir =>
      ZIO.attempt {
        if Files.exists(dir) then
          val walk = Files.walk(dir)
          val paths =
            try walk.iterator.asScala.toList
            finally walk.close()
          paths
            .sortBy(_.getNameCount)
            .reverse
            .foreach(path => Files.deleteIfExists(path))
      }.ignore

  def spec =
    publish.provideSomeLayerShared[zio.test.TestEnvironment & zio.Scope](
      histos.ToolFixture.layer(
        histos.Projects(
          histos.module("library", histos.Jvm, histos.Js)(
            histos.ScalaVersion("3.10.0")
          )
        )
      )
    ) @@ TestAspect.withLiveSystem @@ TestAspect.timeout(3.minutes)
