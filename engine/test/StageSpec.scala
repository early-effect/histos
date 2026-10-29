package histos.publish

import histos.central.{Digest, Publisher}
import histos.pack.{AssemblyError, JavadocJar}
import histos.version.ManualVersion
import histos.zinc.GraphCompile
import histos.{
  ArtifactBase,
  Developer,
  Jvm,
  License,
  MainClass,
  Organization,
  Platform,
  Projects,
  SbtSeries,
  ScalaVersion,
  Scm,
  connection,
  developer,
  license,
  module,
  pom,
  url,
}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.util.zip.ZipFile

import scala.jdk.CollectionConverters.*

import zio.{Chunk, Scope, ZIO, durationInt}
import zio.test.*

object StageSpec extends histos.SuiteClock:
  private val licenseValue =
    License(license"Apache-2.0", url"https://www.apache.org/licenses/LICENSE-2.0.txt")
  private val developerValue = Developer(developer"early-effect", pom"Early Effect")
  private val scmValue       =
    Scm(url"https://example.com/source", connection"scm:git:https://example.com/source.git")

  private val hello =
    "package demo\n\nobject Hello {\n  def greet: String = \"hi\"\n}\n"

  private val plug =
    "package demo\n\nobject Plug {\n  def name: String = \"plug\"\n}\n"

  private val descriptor = "plugin.demo.Plug\n"

  private val lib =
    "package lib\n\nobject Lib {\n  def word: String = \"lib\"\n}\n"

  private val app =
    "package demo\n\nobject App {\n  def go: String = lib.Lib.word\n}\n"

  private val other =
    "package demo\n\nobject Other {\n  def go: String = \"other\"\n}\n"

  def spec =
    suite("stage")(
      test("a library stage is the jar, sources, a readme javadoc, the pom, and three checksums"):
        ZIO.scoped:
          for
            jdk  <- GraphCompile.jdkHere
            root <- temp
            repo <- temp
            _    <- ZIO.attempt {
              write(root, "core/src/main/scala/demo/Hello.scala", hello)
              write(root, "core/src/test/scala/demo/HelloTest.scala", "package demo\n\nobject HelloTest\n")
              write(root, ".histos/dependencies.lock", "cell core 3.10.0 jvm\n")
            }
            built <- ZIO.fromEither(published)
            _     <- histos.ToolFixture.seed(root, built)
            _     <- LocalPublish
              .run(root, built, Nil, jdk, Map.empty, PublishWrite.Local)
              .provideSomeLayer[histos.resolve.LockedTools](
                histos.ToolFixture.resolver ++ Publisher.local(repo)
              )
            found <- files(repo)
            version = repo.resolve("com/example/core_3/1.0.0")
            jar     = version.resolve("core_3-1.0.0.jar")
            sources = version.resolve("core_3-1.0.0-sources.jar")
            javadoc = version.resolve("core_3-1.0.0-javadoc.jar")
            pomPath = version.resolve("core_3-1.0.0.pom")
            classes <- names(jar)
            packed  <- names(sources)
            source  <- entry(sources, "demo/Hello.scala")
            docs    <- names(javadoc)
            readme  <- entry(javadoc, JavadocJar.Entry)
            pom     <- ZIO.attempt(Files.readString(pomPath))
            jarSums <- sums(jar)
            srcSums <- sums(sources)
            docSums <- sums(javadoc)
            pomSums <- sums(pomPath)
            jarHash <- hashed(jar)
            srcHash <- hashed(sources)
            docHash <- hashed(javadoc)
            pomHash <- hashed(pomPath)
          yield assertTrue(
            found == staged,
            !found.exists(_.endsWith(".asc")),
            classes.contains("demo/Hello.class"),
            !classes.exists(_.contains("HelloTest")),
            packed == List("demo/Hello.scala"),
            source == Some(hello),
            docs == List(JavadocJar.Entry),
            readme == Some(JavadocJar.Text),
            pom.contains("<groupId>com.example</groupId>"),
            pom.contains("<artifactId>core_3</artifactId>"),
            pom.contains("<name>core_3</name>"),
            pom.contains("<version>1.0.0</version>"),
            pom.contains("<packaging>jar</packaging>"),
            jarSums == jarHash,
            srcSums == srcHash,
            docSums == docHash,
            pomSums == pomHash,
          )
      ,
      test("an sbt 2 plugin stages under the sbt 2 artifact id and keeps its descriptor"):
        ZIO.scoped:
          for
            jdk  <- GraphCompile.jdkHere
            root <- temp
            repo <- temp
            _    <- ZIO.attempt {
              write(root, "plug/src/main/scala/demo/Plug.scala", plug)
              write(root, "plug/src/main/resources/sbt/sbt.autoplugins", descriptor)
              write(root, ".histos/dependencies.lock", "cell plug 3.10.0 jvm\n")
            }
            built <- ZIO.fromEither(pluginProject)
            _     <- histos.ToolFixture.seed(root, built)
            _     <- LocalPublish
              .run(root, built, Nil, jdk, Map.empty, PublishWrite.Local)
              .provideSomeLayer[histos.resolve.LockedTools](
                histos.ToolFixture.resolver ++ Publisher.local(repo)
              )
            found <- files(repo)
            version = repo.resolve("com/example/plug_sbt2_3/1.0.0")
            jar     = version.resolve("plug_sbt2_3-1.0.0.jar")
            pomPath = version.resolve("plug_sbt2_3-1.0.0.pom")
            classes <- names(jar)
            text    <- entry(jar, "sbt/sbt.autoplugins")
            pom     <- ZIO.attempt(Files.readString(pomPath))
          yield assertTrue(
            found == coordinateFiles("com/example/plug_sbt2_3/1.0.0", "plug_sbt2_3", "1.0.0"),
            !found.exists(_.endsWith(".asc")),
            classes.contains("demo/Plug.class"),
            text == Some(descriptor),
            pom.contains("<artifactId>plug_sbt2_3</artifactId>"),
            pom.contains("<name>plug_sbt2_3</name>"),
            !pom.contains("_sbt2.0"),
          )
      ,
      test("an assembly stages a fat jar, an empty dependency list, and Main-Class"):
        ZIO.scoped:
          for
            jdk  <- GraphCompile.jdkHere
            root <- temp
            repo <- temp
            _    <- ZIO.attempt {
              write(root, "lib/src/main/scala/lib/Lib.scala", lib)
              write(root, "app/src/main/scala/demo/App.scala", app)
              write(root, ".histos/dependencies.lock", "cell lib 3.10.0 jvm\ncell app 3.10.0 jvm\n")
            }
            built <- ZIO.fromEither(assemblyProject)
            _     <- histos.ToolFixture.seed(root, built)
            _     <- LocalPublish
              .run(root, built, Nil, jdk, Map.empty, PublishWrite.Local)
              .provideSomeLayer[histos.resolve.LockedTools](
                histos.ToolFixture.resolver ++ Publisher.local(repo)
              )
            found <- files(repo)
            version = repo.resolve("com/example/tool/1.0.0")
            jar     = version.resolve("tool-1.0.0.jar")
            pomPath = version.resolve("tool-1.0.0.pom")
            classes  <- names(jar)
            manifest <- entry(jar, "META-INF/MANIFEST.MF")
            pom      <- ZIO.attempt(Files.readString(pomPath))
          yield assertTrue(
            found == (
              coordinateFiles("com/example/lib_3/1.0.0", "lib_3", "1.0.0") ++
                coordinateFiles("com/example/tool/1.0.0", "tool", "1.0.0")
            ).sorted,
            classes.contains("demo/App.class"),
            classes.contains("lib/Lib.class"),
            classes.contains("scala/collection/immutable/List.class"),
            manifest.exists(_.contains("Main-Class: demo.App")),
            pom.contains("<artifactId>tool</artifactId>"),
            pom.contains("<name>tool</name>"),
            pom.contains("<version>1.0.0</version>"),
            !pom.contains("<dependencies>"),
            !pom.contains("_3"),
            !found.exists(_.endsWith(".asc")),
          )
      ,
      test("an application stages a thin jar whose pom lists the classpath"):
        ZIO.scoped:
          for
            jdk  <- GraphCompile.jdkHere
            root <- temp
            repo <- temp
            _    <- ZIO.attempt {
              write(root, "lib/src/main/scala/lib/Lib.scala", lib)
              write(root, "app/src/main/scala/demo/App.scala", app)
              write(root, ".histos/dependencies.lock", "cell lib 3.10.0 jvm\ncell app 3.10.0 jvm\n")
            }
            built <- ZIO.fromEither(applicationProject)
            _     <- histos.ToolFixture.seed(root, built)
            _     <- LocalPublish
              .run(root, built, Nil, jdk, Map.empty, PublishWrite.Local)
              .provideSomeLayer[histos.resolve.LockedTools](
                histos.ToolFixture.resolver ++ Publisher.local(repo)
              )
            found <- files(repo)
            version = repo.resolve("com/example/app_3/2.0.0")
            jar     = version.resolve("app_3-2.0.0.jar")
            pomPath = version.resolve("app_3-2.0.0.pom")
            classes <- names(jar)
            pom     <- ZIO.attempt(Files.readString(pomPath))
          yield assertTrue(
            found == (
              coordinateFiles("com/example/lib_3/1.5.0", "lib_3", "1.5.0") ++
                coordinateFiles("com/example/app_3/2.0.0", "app_3", "2.0.0")
            ).sorted,
            classes.contains("demo/App.class"),
            !classes.contains("lib/Lib.class"),
            pom.contains("<artifactId>lib_3</artifactId>"),
            pom.contains("<version>1.5.0</version>"),
            pom.contains("<artifactId>app_3</artifactId>"),
            pom.contains("<name>app_3</name>"),
            pom.contains("<version>2.0.0</version>"),
            !found.exists(_.endsWith(".asc")),
          )
      ,
      test("an assembly whose main class is missing writes nothing"):
        ZIO.scoped:
          for
            jdk  <- GraphCompile.jdkHere
            root <- temp
            repo <- temp
            _    <- ZIO.attempt {
              write(root, "app/src/main/scala/demo/Other.scala", other)
              write(root, ".histos/dependencies.lock", "cell app 3.10.0 jvm\n")
            }
            built   <- ZIO.fromEither(missingMain)
            _       <- histos.ToolFixture.seed(root, built)
            refused <- LocalPublish
              .run(root, built, Nil, jdk, Map.empty, PublishWrite.Local)
              .provideSomeLayer[histos.resolve.LockedTools](
                histos.ToolFixture.resolver ++ Publisher.local(repo)
              )
              .either
            found <- files(repo)
          yield assertTrue(
            refused match
              case Left(LocalPublishError.Pack(module, version, platform, AssemblyError.MissingMain(main))) =>
                module.value == "app" &&
                version.value == "3.10.0" &&
                platform == Platform.Jvm &&
                main.binary == "demo.Missing"
              case _ => false,
            found.isEmpty,
          ),
    ).provideSomeLayerShared[zio.test.TestEnvironment & zio.Scope](
      histos.ToolFixture.layer(
        histos.Projects(histos.module("library", histos.Jvm)(histos.ScalaVersion("3.10.0")))
      )
    ) @@ TestAspect.withLiveSystem @@ TestAspect.timeout(3.minutes)

  private val staged = List(
    "com/example/core_3/1.0.0/core_3-1.0.0-javadoc.jar",
    "com/example/core_3/1.0.0/core_3-1.0.0-javadoc.jar.md5",
    "com/example/core_3/1.0.0/core_3-1.0.0-javadoc.jar.sha1",
    "com/example/core_3/1.0.0/core_3-1.0.0-javadoc.jar.sha256",
    "com/example/core_3/1.0.0/core_3-1.0.0-sources.jar",
    "com/example/core_3/1.0.0/core_3-1.0.0-sources.jar.md5",
    "com/example/core_3/1.0.0/core_3-1.0.0-sources.jar.sha1",
    "com/example/core_3/1.0.0/core_3-1.0.0-sources.jar.sha256",
    "com/example/core_3/1.0.0/core_3-1.0.0.jar",
    "com/example/core_3/1.0.0/core_3-1.0.0.jar.md5",
    "com/example/core_3/1.0.0/core_3-1.0.0.jar.sha1",
    "com/example/core_3/1.0.0/core_3-1.0.0.jar.sha256",
    "com/example/core_3/1.0.0/core_3-1.0.0.pom",
    "com/example/core_3/1.0.0/core_3-1.0.0.pom.md5",
    "com/example/core_3/1.0.0/core_3-1.0.0.pom.sha1",
    "com/example/core_3/1.0.0/core_3-1.0.0.pom.sha256",
  )

  private def coordinateFiles(path: String, artifact: String, version: String): List[String] =
    val names = List(
      s"$artifact-$version-javadoc.jar",
      s"$artifact-$version-sources.jar",
      s"$artifact-$version.jar",
      s"$artifact-$version.pom",
    )
    names.flatMap(name => List(s"$path/$name", s"$path/$name.md5", s"$path/$name.sha1", s"$path/$name.sha256")).sorted

  private val pluginProject =
    val plug = module("plug", Jvm)(ScalaVersion("3.10.0")).sbtPlugin(SbtSeries("2"))
    released(histos.ships(histos.manual("libs", ManualVersion("1.0.0"))(plug)), plug)

  private val assemblyProject =
    val library = module("lib", Jvm)(ScalaVersion("3.10.0"))
    val program = module("app", Jvm)(ScalaVersion("3.10.0"))
      .artifact(ArtifactBase("tool"))
      .assembly(MainClass("demo.App"))
      .dependsOn(library)
    released(histos.ships(histos.manual("libs", ManualVersion("1.0.0"))(library, program)), library, program)

  private val applicationProject =
    val library = module("lib", Jvm)(ScalaVersion("3.10.0"))
    val program = module("app", Jvm)(ScalaVersion("3.10.0")).application.dependsOn(library)
    released(
      histos.ships(
        histos.manual("libs", ManualVersion("1.5.0"))(library),
        histos.manual("tool", ManualVersion("2.0.0"))(program),
      ),
      library,
      program,
    )

  private val missingMain =
    val program = module("app", Jvm)(ScalaVersion("3.10.0")).assembly(MainClass("demo.Missing"))
    Projects
      .organization(Organization("com.example"))(
        histos.ships(histos.manual("tool", ManualVersion("0.0.1-SNAPSHOT"))(program)),
        program,
      )

  private def released(catalog: histos.CatalogSpec, modules: histos.ProjectDef*) =
    Projects
      .organization(Organization("com.example"))
      .license(licenseValue)
      .developer(developerValue)
      .scm(scmValue)(catalog, modules*)

  private val published =
    val core = module("core", Jvm)(ScalaVersion("3.10.0"))
    Projects
      .organization(Organization("com.example"))
      .license(licenseValue)
      .developer(developerValue)
      .scm(scmValue)(
        histos.ships(histos.manual("libs", ManualVersion("1.0.0"))(core)),
        core,
      )

  private final case class Sums(md5: String, sha1: String, sha256: String)

  private def sums(path: Path): zio.Task[Sums] =
    ZIO.attempt:
      def side(suffix: String): String =
        val name   = Option(path.getFileName).map(_.toString).getOrElse("artifact")
        val parent = Option(path.getParent).getOrElse(path)
        Files.readString(parent.resolve(s"$name.$suffix")).stripLineEnd
      Sums(side("md5"), side("sha1"), side("sha256"))

  private def hashed(path: Path): zio.Task[Sums] =
    ZIO.attempt:
      val body = Chunk.fromArray(Files.readAllBytes(path))
      Sums(Digest.md5(body), Digest.sha1(body), Digest.sha256(body))

  private def names(jar: Path): zio.Task[List[String]] =
    ZIO.attempt:
      val zip = new ZipFile(jar.toFile)
      try zip.entries.asScala.map(_.getName).toList.sorted
      finally zip.close()

  private def entry(jar: Path, name: String): zio.Task[Option[String]] =
    ZIO.attempt:
      val zip = new ZipFile(jar.toFile)
      try
        Option(zip.getEntry(name)).map: found =>
          val stream = zip.getInputStream(found)
          try new String(stream.readAllBytes(), StandardCharsets.UTF_8)
          finally stream.close()
      finally zip.close()

  private def files(root: Path): zio.Task[List[String]] =
    ZIO.attempt:
      val walk = Files.walk(root)
      try
        walk.iterator.asScala
          .filter(path => Files.isRegularFile(path))
          .map(path => root.relativize(path).toString.replace('\\', '/'))
          .toList
          .sorted
      finally walk.close()

  private def write(root: Path, name: String, text: String): Unit =
    val path = root.resolve(name)
    Option(path.getParent).foreach(parent => Files.createDirectories(parent))
    Files.writeString(path, text)

  private def temp: ZIO[Scope, Throwable, Path] =
    ZIO.acquireRelease(ZIO.attempt(Files.createTempDirectory("histos-stage"))): dir =>
      ZIO.attempt {
        if Files.exists(dir) then
          val walk  = Files.walk(dir)
          val paths =
            try walk.iterator.asScala.toList
            finally walk.close()
          paths.sortBy(_.getNameCount).reverse.foreach(path => Files.deleteIfExists(path))
      }.ignore
end StageSpec
