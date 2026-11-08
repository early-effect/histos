package histos.resolve

import histos.jdk.Sha256
import histos.workspace.Workspace
import histos.{Dep, DepKind, Platform, ProjectName, ScalaVersion}

import java.nio.file.{Files, Path}
import java.util.zip.{ZipEntry, ZipOutputStream}

import zio.{Chunk, ZIO}
import zio.test.*

object CoursierLawSpec extends ZIOSpecDefault:
  private val name     = ProjectName.trusted("core")
  private val scala3   = ScalaVersion.trusted("3.10.0")
  private val scala213 = ScalaVersion.trusted("2.13.16")
  private val fixed    = Dep.trusted("org.example", "missing", "1.0.0", DepKind.Java)
  private val ranged   = Dep.trusted("org.example", "missing", "1.+", DepKind.Java)
  private val wrong    = Sha256.trusted("0000000000000000000000000000000000000000000000000000000000000000")

  def spec = suite("coursier laws")(
    test("an empty cell is an empty classpath, and offline refuses a missing module before any download"):
      Workspace
        .work("histos-coursier")
        .flatMap: root =>
          val repos   = Repositories(Repository.Maven(RepoUrl.file(root.resolve("repo"))), Nil)
          val empty   = ResolveCell(name, Nil, scala3, Platform.Jvm, repos)
          val missing = ResolveCell(name, List(fixed), scala3, Platform.Jvm, repos)
          val program =
            for
              resolve <- ZIO.service[Resolve]
              blank   <- resolve.resolve(empty)
              missed  <- resolve.resolve(missing).either
              plugin  <- resolve.resolvePlugin(fixed, repos).either
              range   <- resolve.resolvePlugin(ranged, repos).either
            yield assertTrue(
              blank == Classpath(Nil, Nil),
              missed == Left(ResolveError.NotFound("org.example", "missing", "1.0.0")),
              plugin == Left(ResolveError.NotFound("org.example", "missing", "1.0.0")),
              range == Left(ResolveError.Range(ranged)),
            )
          program.provideSomeLayer[TestEnvironment](Resolve.offline(root.resolve("cache")))
    ,
    test("a file repository resolves the jar it holds, and a locked fetch checks that checksum"):
      Workspace
        .work("histos-coursier-file")
        .flatMap: root =>
          val repo    = root.resolve("repo")
          val origin  = Repository.Maven(RepoUrl.file(repo))
          val repos   = Repositories(origin, Nil)
          val dep     = Dep.trusted("org.example", "widget", "1.0.0", DepKind.Java)
          val cell    = ResolveCell(name, List(dep), scala3, Platform.Jvm, repos)
          val program =
            for
              _         <- publish(repo, "widget", "1.0.0", "widget", pom("widget", "1.0.0"))
              _         <- sourcesJar(repo, "widget", "1.0.0", "sources")
              mainSha   <- shaOf(jar(repo, "widget", "1.0.0"))
              sourceSha <- shaOf(jar(repo, "widget", "1.0.0", Some("sources")))
              resolve   <- ZIO.service[Resolve]
              found     <- resolve.resolve(cell)
              checked   <- found.artifacts match
                case artifact :: Nil =>
                  val locked = LockedArtifact(
                    "org.example",
                    "widget",
                    "1.0.0",
                    artifact.sha256,
                    origin,
                    None,
                  )
                  val classified = locked.copy(sha256 = sourceSha, classifier = Some(JarClassifier("sources")))
                  for
                    fetched        <- resolve.fetch(locked)
                    checksum       <- resolve.fetch(locked.copy(sha256 = wrong)).either
                    removed        <- ZIO.attemptBlocking(Files.deleteIfExists(fetched.path)).orDie
                    memo           <- resolve.fetch(locked).either
                    sources        <- resolve.sources("org.example", "widget", "1.0.0", origin)
                    absent         <- resolve.sources("org.example", "widget", "9.9.9", origin).either
                    fetchedSources <- resolve.fetch(classified)
                  yield assertTrue(
                    artifact.organization == "org.example",
                    artifact.name == "widget",
                    artifact.revision == "1.0.0",
                    artifact.sha256 == mainSha,
                    artifact.repository == origin,
                    artifact.classifier.isEmpty,
                    artifact.path.getFileName.toString == "widget-1.0.0.jar",
                    found.evictions.isEmpty,
                    fetched == artifact,
                    checksum == Left(ResolveError.Checksum(fetched.path, wrong, fetched.sha256)),
                    removed,
                    memo == Right(fetched),
                    sources.organization == "org.example",
                    sources.name == "widget",
                    sources.revision == "1.0.0",
                    sources.sha256 == sourceSha,
                    sources.repository == origin,
                    sources.classifier.isEmpty,
                    sources.path.getFileName.toString == "widget-1.0.0-sources.jar",
                    sources.sha256 != artifact.sha256,
                    absent == Left(ResolveError.NotFound("org.example", "widget", "9.9.9")),
                    fetchedSources.sha256 == sourceSha,
                    fetchedSources.classifier == Some(JarClassifier("sources")),
                    fetchedSources.path.getFileName.toString == "widget-1.0.0-sources.jar",
                    fetchedSources.repository == origin,
                  )
                case other =>
                  ZIO.succeed(
                    assertTrue(
                      other.map(artifact => s"${artifact.organization}:${artifact.name}:${artifact.revision}") ==
                        List("org.example:widget:1.0.0")
                    )
                  )
            yield checked
          program.provideSomeLayer[TestEnvironment](Resolve.live(root.resolve("cache")))
    ,
    test("a solve fetches the suffixed artifact name for the kind and the platform"):
      Workspace
        .work("histos-coursier-suffix")
        .flatMap: root =>
          val repo   = root.resolve("repo")
          val origin = Repository.Maven(RepoUrl.file(repo))
          val repos  = Repositories(origin, Nil)
          val cases  = List(
            (DepKind.Binary, Platform.Jvm, scala3, "widget_3"),
            (DepKind.Full, Platform.Jvm, scala3, "widget_3.10.0"),
            (DepKind.Binary, Platform.Js, scala3, "widget_sjs1_3"),
            (DepKind.Binary, Platform.Native, scala3, "widget_native0.5_3"),
            (DepKind.Binary, Platform.Jvm, scala213, "widget_2.13"),
          )
          val program =
            for
              _ <- ZIO.foreachDiscard(cases):
                case (_, _, _, artifact) =>
                  publish(repo, artifact, "1.0.0", artifact, pom(artifact, "1.0.0"))
              resolve <- ZIO.service[Resolve]
              got     <- ZIO.foreach(cases):
                case (kind, platform, version, artifact) =>
                  val dep = Dep.trusted("org.example", "widget", "1.0.0", kind)
                  resolve
                    .resolve(ResolveCell(name, List(dep), version, platform, repos))
                    .map: found =>
                      found.artifacts match
                        case hit :: Nil =>
                          (
                            artifact,
                            hit.name,
                            hit.revision,
                            hit.repository,
                            hit.classifier.isEmpty,
                            found.evictions.isEmpty,
                          )
                        case other =>
                          (artifact, other.map(_.name).mkString(","), "", origin, false, false)
            yield assertTrue(
              got == cases.map:
                case (_, _, _, artifact) =>
                  (artifact, artifact, "1.0.0", origin, true, true)
            )
          program.provideSomeLayer[TestEnvironment](Resolve.live(root.resolve("cache")))
    ,
    test("a newer direct dependency evicts the older transitive jar"):
      Workspace
        .work("histos-coursier-evict")
        .flatMap: root =>
          val repo    = root.resolve("repo")
          val origin  = Repository.Maven(RepoUrl.file(repo))
          val repos   = Repositories(origin, Nil)
          val widget  = Dep.trusted("org.example", "widget", "1.0.0", DepKind.Java)
          val lib     = Dep.trusted("org.example", "lib", "2.0.0", DepKind.Java)
          val cell    = ResolveCell(name, List(widget, lib), scala3, Platform.Jvm, repos)
          val program =
            for
              _ <- publish(repo, "lib", "1.0.0", "old", pom("lib", "1.0.0"))
              _ <- publish(repo, "lib", "2.0.0", "new", pom("lib", "2.0.0"))
              _ <- publish(
                repo,
                "widget",
                "1.0.0",
                "widget",
                pom("widget", "1.0.0", List("lib" -> "1.0.0")),
              )
              widgetSha <- shaOf(jar(repo, "widget", "1.0.0"))
              libSha    <- shaOf(jar(repo, "lib", "2.0.0"))
              resolve   <- ZIO.service[Resolve]
              found     <- resolve.resolve(cell)
            yield
              val widgetHit = found.artifacts.find(hit => hit.name == "widget" && hit.revision == "1.0.0")
              val libHit    = found.artifacts.find(hit => hit.name == "lib" && hit.revision == "2.0.0")
              assertTrue(
                found.artifacts.length == 2,
                widgetHit.map(_.sha256) == Some(widgetSha),
                widgetHit.map(_.repository) == Some(origin),
                libHit.map(_.sha256) == Some(libSha),
                libHit.map(_.repository) == Some(origin),
                found.evictions == List(Eviction("org.example", "lib", "1.0.0", "2.0.0")),
              )
          program.provideSomeLayer[TestEnvironment](Resolve.live(root.resolve("cache"))),
  )

  private def pom(artifact: String, version: String, depends: List[(String, String)] = Nil) =
    val body =
      if depends.isEmpty then ""
      else
        val rows = depends
          .map:
            case (name, revision) =>
              s"""|    <dependency>
                  |      <groupId>org.example</groupId>
                  |      <artifactId>$name</artifactId>
                  |      <version>$revision</version>
                  |    </dependency>""".stripMargin
          .mkString("\n")
        s"""|  <dependencies>
            |$rows
            |  </dependencies>
            |""".stripMargin
    s"""|<?xml version="1.0" encoding="UTF-8"?>
        |<project>
        |  <modelVersion>4.0.0</modelVersion>
        |  <groupId>org.example</groupId>
        |  <artifactId>$artifact</artifactId>
        |  <version>$version</version>
        |$body</project>
        |""".stripMargin

  private def jar(repo: Path, artifact: String, version: String, classifier: Option[String] = None) =
    val suffix = classifier match
      case Some(value) => s"-$value"
      case None        => ""
    repo
      .resolve("org")
      .resolve("example")
      .resolve(artifact)
      .resolve(version)
      .resolve(s"$artifact-$version$suffix.jar")

  private def publish(repo: Path, artifact: String, version: String, body: String, pomText: String) =
    val path = jar(repo, artifact, version)
    store(path, body) *> Workspace.writeString(path.resolveSibling(s"$artifact-$version.pom"), pomText)

  private def sourcesJar(repo: Path, artifact: String, version: String, body: String) =
    store(jar(repo, artifact, version, Some("sources")), body)

  private def shaOf(path: Path) =
    ZIO.attemptBlocking(Sha256.of(Chunk.fromArray(Files.readAllBytes(path)))).orDie

  private def store(path: Path, text: String) =
    ZIO
      .attemptBlocking:
        Option(path.getParent).foreach(parent => Files.createDirectories(parent))
        val out = new ZipOutputStream(Files.newOutputStream(path))
        try
          out.putNextEntry(new ZipEntry("widget.txt"))
          out.write(text.getBytes(java.nio.charset.StandardCharsets.UTF_8))
          out.closeEntry()
        finally out.close()
      .orDie
end CoursierLawSpec
