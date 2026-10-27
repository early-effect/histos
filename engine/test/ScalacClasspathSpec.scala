package histos.zinc

import histos.jdk.Sha256
import histos.resolve.{
  BuildResolvers,
  ClasspathScope,
  DependencyLock,
  LockedArtifact,
  LockedCell,
  LockedDirect,
  RepoUrl,
  Repository,
  Resolve,
  ResolveCell,
}
import histos.{Dep, DepKind, Jvm, Module, Platform, ProjectGraph, Projects, ScalaVersion, module}

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.util.jar.JarOutputStream

import scala.jdk.CollectionConverters.*

import zio.{Chunk, Scope, ZIO, durationInt}
import zio.test.*

object ScalacClasspathSpec extends ZIOSpecDefault:
  private val version = ScalaVersion("3.10.0")

  def spec = suite("scalac classpath")(
    test("runtime, then lock jars without the stdlib copy, then a class directory that is not there yet"):
      ZIO.scoped:
        for
          root  <- temp
          maven <- ZIO.attempt(Files.createDirectories(root.resolve("repo")).toRealPath())
          cache <- ZIO.attempt(Files.createDirectories(root.resolve("cache")))
          extraJar   = emptyJar
          probeJar   = emptyJar
          libraryJar = emptyJar
          extraSha   = Sha256.of(Chunk.fromArray(extraJar))
          probeSha   = Sha256.of(Chunk.fromArray(probeJar))
          librarySha = Sha256.of(Chunk.fromArray(libraryJar))
          _ <- ZIO.attempt {
            writeMaven(maven, "dev.example", "extra", "1.0.0", extraJar)
            writeMaven(maven, "dev.example", "probe", "1.0.0", probeJar)
            writeMaven(maven, "org.scala-lang", "scala-library", "2.13.18", libraryJar)
          }
          repo   = Repository.Maven(RepoUrl.file(maven))
          extra  = Dep.trusted("dev.example", "extra", "1.0.0", DepKind.Java)
          probe  = Dep.trusted("dev.example", "probe", "1.0.0", DepKind.Java)
          libDef = module("lib", Jvm)(version)
          appDef = module("app", Jvm)(version).dependsOn(libDef).libraryDependencies(extra).testDependencies(probe)
          built <- ZIO.fromEither(Projects(libDef, appDef))
          lib   <- ZIO.fromEither(named(built, "lib"))
          app   <- ZIO.fromEither(named(built, "app"))
          extraArtifact   = artifact("dev.example", "extra", "1.0.0", extraSha, repo)
          probeArtifact   = artifact("dev.example", "probe", "1.0.0", probeSha, repo)
          libraryArtifact = artifact("org.scala-lang", "scala-library", "2.13.18", librarySha, repo)
          compileDirects  = Dep.select(app.dependencies, Platform.Jvm).map(direct)
          testDirects = (Dep.select(app.dependencies, Platform.Jvm) ++ Dep.select(app.testDependencies, Platform.Jvm))
            .map(direct)
          lock = DependencyLock(
            List(
              LockedCell(lib.name, version, Platform.Jvm, Nil, Nil, Nil),
              LockedCell(
                app.name,
                version,
                Platform.Jvm,
                compileDirects,
                List(extraArtifact, libraryArtifact),
                Nil,
              ),
              LockedCell(
                app.name,
                version,
                Platform.Jvm,
                testDirects,
                List(extraArtifact, probeArtifact, libraryArtifact),
                Nil,
                ClasspathScope.Test,
              ),
            )
          )
          _ <- ZIO.attempt(write(root, ".histos/dependencies.lock", DependencyLock.render(lock)))
          repos      = BuildResolvers(built.resolvers)
          libClasses = GraphCompile.classesDir(root, lib.name, version, Platform.Jvm)
          appClasses = GraphCompile.classesDir(root, app.name, version, Platform.Jvm)
          checked <- histos.ToolFixture
            .at(root, built) {
              for
                runtime <- JvmCompile.runtime(version)
                main    <- ScalacClasspath.of(
                  root,
                  modules(built),
                  app,
                  version,
                  Platform.Jvm,
                  lock,
                  repos,
                  ClasspathScope.Compile,
                )
                tested <- ScalacClasspath.of(
                  root,
                  modules(built),
                  app,
                  version,
                  Platform.Jvm,
                  lock,
                  repos,
                  ClasspathScope.Test,
                )
                compiled <- Resolve.materialize(
                  ResolveCell(app.name, Dep.select(app.dependencies, Platform.Jvm), version, Platform.Jvm, repos),
                  lock,
                )
                tests <- Resolve.materialize(
                  ResolveCell(
                    app.name,
                    Dep.select(app.dependencies, Platform.Jvm) ++ Dep.select(app.testDependencies, Platform.Jvm),
                    version,
                    Platform.Jvm,
                    repos,
                    ClasspathScope.Test,
                  ),
                  lock,
                )
              yield (runtime, main, tested, compiled, tests)
            }
            .provideSomeLayer[histos.resolve.LockedTools](histos.ToolFixture.beside(cache))
          (runtime, main, tested, compiled, tests) = checked
          extraPath                                = compiled.artifacts.find(_.name == "extra").map(_.path)
          libraryPath                              = compiled.artifacts.find(_.name == "scala-library").map(_.path)
          probePath                                = tests.artifacts.find(_.name == "probe").map(_.path)
          picked <- ZIO.fromEither(
            (extraPath, libraryPath, probePath) match
              case (Some(jar), Some(library), Some(probeJarPath)) => Right((jar, library, probeJarPath))
              case _                                              => Left(Fault.Shape)
          )
          (jar, library, probeJarPath) = picked
        yield assertTrue(
          absolute(main.paths) == absolute(runtime ++ List(jar) ++ List(libClasses)),
          absolute(main.jars) == absolute(List(jar)),
          main.hashes == List(extraSha),
          JvmCompile.isStandardLibrary(library),
          !absolute(main.paths).contains(library.toAbsolutePath.normalize),
          !absolute(tested.paths).contains(library.toAbsolutePath.normalize),
          absolute(tested.paths) == absolute(runtime ++ List(jar, probeJarPath) ++ List(appClasses, libClasses)),
          !absolute(main.paths).contains(probeJarPath.toAbsolutePath.normalize),
          !Files.exists(libClasses),
          !Files.exists(appClasses),
        )
  ).provideSomeLayerShared[zio.test.TestEnvironment & zio.Scope](
    histos.ToolFixture.jvm(version)
  ) @@ TestAspect.withLiveSystem @@ TestAspect.timeout(4.minutes)

  private enum Fault:
    case Shape

  private def named(graph: ProjectGraph, name: String): Either[Fault, Module] =
    modules(graph).filter(_.name.value == name) match
      case item :: Nil => Right(item)
      case _           => Left(Fault.Shape)

  private def modules(graph: ProjectGraph): List[Module] =
    graph.projects.collect { case item: Module => item }

  private def direct(dep: Dep): LockedDirect =
    LockedDirect(dep.organization, dep.name, dep.revision, dep.kind)

  private def artifact(
      organization: String,
      name: String,
      revision: String,
      sha: Sha256,
      repository: Repository,
  ): LockedArtifact =
    LockedArtifact(organization, name, revision, sha, repository)

  private def absolute(paths: List[Path]): List[Path] =
    paths.map(_.toAbsolutePath.normalize)

  private def writeMaven(
      root: Path,
      group: String,
      name: String,
      revision: String,
      jar: Array[Byte],
  ): Unit =
    val dir = (group.split("\\.").toList :+ name :+ revision).foldLeft(root)(_.resolve(_))
    Files.createDirectories(dir)
    val jarName  = s"$name-$revision.jar"
    val pomName  = s"$name-$revision.pom"
    val pomBytes =
      s"""<?xml version="1.0" encoding="UTF-8"?>
         |<project>
         |  <modelVersion>4.0.0</modelVersion>
         |  <groupId>$group</groupId>
         |  <artifactId>$name</artifactId>
         |  <version>$revision</version>
         |</project>
         |""".stripMargin.getBytes(StandardCharsets.UTF_8)
    Files.write(dir.resolve(jarName), jar)
    Files.write(dir.resolve(pomName), pomBytes)
    Files.writeString(dir.resolve(s"$jarName.sha1"), sha1(jar) + "\n")
    Files.writeString(dir.resolve(s"$pomName.sha1"), sha1(pomBytes) + "\n")

  private def sha1(bytes: Array[Byte]): String =
    java.security.MessageDigest
      .getInstance("SHA-1")
      .digest(bytes)
      .map(byte => f"${byte & 0xff}%02x")
      .mkString

  private def emptyJar: Array[Byte] =
    val buffer = new ByteArrayOutputStream
    val output = new JarOutputStream(buffer)
    output.close()
    buffer.toByteArray

  private def write(root: Path, name: String, text: String): Unit =
    val path = root.resolve(name)
    Files.createDirectories(path.getParent)
    Files.writeString(path, text)

  private def temp: ZIO[Scope, Throwable, Path] =
    ZIO.acquireRelease(ZIO.attempt(Files.createTempDirectory("histos-classpath"))): dir =>
      ZIO.attempt {
        if Files.exists(dir) then
          val walk  = Files.walk(dir)
          val paths =
            try walk.iterator.asScala.toList
            finally walk.close()
          paths.sortBy(_.getNameCount).reverse.foreach(path => Files.deleteIfExists(path))
      }.ignore
end ScalacClasspathSpec
