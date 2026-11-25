package histos.load

import histos.jdk.Sha256Digest
import histos.plugin.{ApiDigest, LinkStrategy, PluginCoordinate}
import histos.resolve.{Classpath, Repository, Resolve, ResolvedArtifact}
import histos.suite.HistosSpec
import histos.version.Catalog
import histos.workspace.Workspace
import histos.{
  Dep,
  DepKind,
  Module,
  Platform,
  Platforms,
  PluginPublication,
  ProjectGraph,
  ProjectName,
  ScalaVersion,
  ScalaVersions,
  SourcePos,
  Splits,
}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.util.zip.{ZipEntry, ZipOutputStream}

import scala.jdk.CollectionConverters.*

import zio.test.*
import zio.{Chunk, Scope, ZIO}

/** Red proofs for the closure the current loader does not check. */
object PluginClosureSpec extends HistosSpec:
  def spec = suite("plugin closure")(
    test("the api export manifest includes theme and step and refuses engine packages") {
      assertTrue(
        ClassHome.boundary("histos.theme.Theme"),
        ClassHome.boundary("histos.step.Step"),
        !ClassHome.boundary("histos.schedule.Sendable"),
        !ClassHome.boundary("histos.load.Loader"),
      )
    },
    test("a comment that names HistosBuild does not suppress compilation") {
      ZIO.scoped {
        for
          repo   <- workspace
          source <- write(
            repo.resolve("helper/src/demo/Helper.scala"),
            """|package demo
               |
               |object Helper:
               |  // trait HistosBuild
               |  def token: String = "compiled"
               |""".stripMargin,
          )
          host     <- hostJar(repo.resolve("host.jar"))
          boundary <- Boundary.jars(host)
          graph = moduleGraph(ProjectName("helper"), Nil)
          outs <- PluginModules.compile(repo, graph, boundary)
          classFile = outs.headOption.map(_.resolve("demo/Helper.class"))
          present <- ZIO.attemptBlocking(classFile.exists(path => Files.isRegularFile(path))).orDie
        yield assertTrue(outs.nonEmpty, present, source.endsWith("Helper.scala"))
      }
    },
    test("a local module compile sees its external jar") {
      ZIO.scoped {
        for
          repo     <- workspace
          host     <- hostJar(repo.resolve("host.jar"))
          boundary <- Boundary.jars(host)
          jars = boundary.filterNot(_ == host)
          jar <- externalJar(repo.resolve("ext.jar"), jars)
          _   <- write(
            repo.resolve("helper/src/demo/Use.scala"),
            """|package demo
               |
               |object Use:
               |  def token: String = demo.Ext.n
               |""".stripMargin,
          )
          graph = moduleGraph(ProjectName("helper"), List(Dep.trusted("demo", "ext", "1", DepKind.Java)))
          outs    <- PluginModules.compile(repo, graph, jars, extOf(jar))
          present <- ZIO.attemptBlocking(Files.isRegularFile(jar)).orDie
        yield assertTrue(outs.nonEmpty, present)
      }
    },
    test("a helper stamp is checked, not only the root") {
      ZIO.scoped {
        for
          repo     <- workspace
          launcher <- digest("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
          other    <- digest("bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb")
          direct   <- stampedJar(repo.resolve("direct.jar"), "com/example/Root.class", launcher)
          helper   <- stampedJar(repo.resolve("helper.jar"), "com/example/Helper.class", other)
          graph = published(Dep.trusted("com.example", "reviewed-locks", "1.0.0", DepKind.Binary))
          exit <- PluginShips.jars(graph, resolving(direct, helper), launcher, None).exit
        yield assertTrue(exit.isFailure)
      }
    },
  ) @@ TestAspect.withLiveClock

  private def workspace: ZIO[Scope, Nothing, Path] =
    Workspace.work("histos-p6-red").foldZIO(failure => ZIO.dieMessage(failure.message), ZIO.succeed)

  private def write(path: Path, text: String): ZIO[Any, Nothing, Path] =
    ZIO.attemptBlocking {
      Files.createDirectories(path.getParent)
      Files.writeString(path, text, StandardCharsets.UTF_8)
      path
    }.orDie

  private def hostJar(path: Path): ZIO[Any, Nothing, Path] =
    ZIO.attemptBlocking {
      Files.createDirectories(path.getParent)
      val out = new ZipOutputStream(Files.newOutputStream(path))
      try
        out.putNextEntry(new ZipEntry("histos/HistosBuild.class"))
        out.write(Array[Byte](0, 1, 2))
        out.closeEntry()
      finally out.close()
      path
    }.orDie

  private def externalJar(path: Path, classpath: List[Path]): ZIO[Scope, Nothing, Path] =
    ZIO.scoped {
      for
        out <- Workspace.work("histos-p6-ext").foldZIO(failure => ZIO.dieMessage(failure.message), ZIO.succeed)
        _   <- write(
          out.resolve("Ext.scala"),
          """|package demo
             |
             |object Ext:
             |  def n: String = "ext"
             |""".stripMargin,
        )
        classes = out.resolve("classes")
        _ <- ZIO.attemptBlocking(Files.createDirectories(classes)).orDie
        _ <- BuildFile
          .compileSources(List(out.resolve("Ext.scala")), classpath, classes)
          .foldZIO(failure => ZIO.dieMessage(failure.message), ZIO.succeed)
        jar <- ZIO.attemptBlocking {
          val zip = new ZipOutputStream(Files.newOutputStream(path))
          try packTree(zip, classes)
          finally zip.close()
          path
        }.orDie
      yield jar
    }

  private def packTree(zip: ZipOutputStream, root: Path): Unit =
    val walk = Files.walk(root)
    try
      walk.iterator.asScala.foreach: path =>
        if Files.isRegularFile(path) then
          val name = root.relativize(path).toString.replace('\\', '/')
          if name.endsWith(".class") || name.endsWith(".tasty") then writeEntry(zip, name, Files.readAllBytes(path))
    finally walk.close()

  private def writeEntry(zip: ZipOutputStream, name: String, bytes: Array[Byte]): Unit =
    zip.putNextEntry(new ZipEntry(name))
    zip.write(bytes)
    zip.closeEntry()

  private def stampedJar(path: Path, classEntry: String, api: ApiDigest): ZIO[Any, Nothing, Path] =
    ZIO.attemptBlocking {
      Files.createDirectories(path.getParent)
      val out = new ZipOutputStream(Files.newOutputStream(path))
      try
        out.putNextEntry(new ZipEntry(classEntry))
        out.write(classEntry.getBytes(StandardCharsets.UTF_8))
        out.closeEntry()
        out.putNextEntry(new ZipEntry(ApiDigest.resource))
        out.write(api.value.getBytes(StandardCharsets.UTF_8))
        out.closeEntry()
      finally out.close()
      path
    }.orDie

  private def digest(hex: String): ZIO[Any, Nothing, ApiDigest] =
    ApiDigest.from(hex) match
      case Right(found) => ZIO.succeed(found)
      case Left(error)  => ZIO.dieMessage(error.message)

  private def moduleGraph(name: ProjectName, dependencies: List[Dep]): ProjectGraph =
    val version = ScalaVersion.trusted("3.10.1")
    val module  = Module(
      name,
      Platforms(Platform.Jvm, Nil),
      ScalaVersions(version, Nil),
      dependencies,
      Nil,
      SourcePos("histos.plugins.scala", 1, 1),
      pluginLibrary = Some(PluginPublication.Rebuildable),
    )
    ProjectGraph.trusted(
      List(module),
      Splits.aggregate,
      Catalog.empty,
      None,
      Nil,
      coordinates = List(PluginCoordinate.Module(name)),
    )

  private def published(dependency: Dep): ProjectGraph =
    ProjectGraph.trusted(
      Nil,
      Splits.aggregate,
      Catalog.empty,
      None,
      Nil,
      coordinates = List(PluginCoordinate.Published(dependency, LinkStrategy.BinaryOrSource)),
    )

  private def extOf(path: Path): Resolve =
    new Resolve:
      def resolveTool(request: histos.resolve.ToolRequest) =
        ZIO.dieMessage("resolveTool")
      def resolve(cell: histos.resolve.ResolveCell) =
        val dependency = cell.dependencies match
          case found :: _ => found
          case Nil        => Dep.trusted("demo", "ext", "1", DepKind.Java)
        ZIO.succeed(Classpath(List(artifact(dependency, path)), Nil))
      def fetch(artifact: histos.resolve.LockedArtifact) =
        ZIO.dieMessage("fetch")
      def resolvePlugin(dependency: Dep, repositories: histos.resolve.Repositories) =
        ZIO.dieMessage("resolvePlugin")
      def sources(organization: String, name: String, revision: String, repository: Repository) =
        ZIO.dieMessage("sources")

  private def resolving(direct: Path, helper: Path): Resolve =
    new Resolve:
      def resolveTool(request: histos.resolve.ToolRequest) =
        ZIO.die(new IllegalStateException("resolveTool"))
      def resolve(cell: histos.resolve.ResolveCell) =
        ZIO.die(new IllegalStateException("resolve"))
      def fetch(artifact: histos.resolve.LockedArtifact) =
        ZIO.die(new IllegalStateException("fetch"))
      def resolvePlugin(dependency: Dep, repositories: histos.resolve.Repositories) =
        ZIO.succeed(Classpath(List(artifact(dependency, direct), artifact(dependency, helper)), Nil))
      def sources(organization: String, name: String, revision: String, repository: Repository) =
        ZIO.die(new IllegalStateException("sources"))

  private def artifact(dependency: Dep, path: Path): ResolvedArtifact =
    ResolvedArtifact(
      dependency.organization,
      dependency.name,
      dependency.revision.value,
      path,
      Sha256Digest.of(Chunk.fromArray(path.toString.getBytes(StandardCharsets.UTF_8))),
      Repository.Central,
    )

end PluginClosureSpec
