package histos.load

import histos.cache.ContentStore
import histos.jdk.{Sha256, Sha256Digest}
import histos.plugin.{ApiDigest, LinkStrategy, PluginCoordinate, PluginLoadError}
import histos.resolve.{
  Classpath,
  DependencyLock,
  LockedArtifact,
  LockedDirect,
  LockedPlugin,
  Resolve,
  ResolveCell,
  ResolveError,
  ResolvedArtifact,
  ToolRequest,
}
import histos.workspace.{IoFault, Workspace}
import histos.Dep
import histos.dep

import java.nio.file.{Files, Path}
import java.util.jar.{JarEntry, JarOutputStream}

import scala.jdk.CollectionConverters.*

import zio.{Chunk, IO, Scope, ZIO, ZLayer}
import zio.test.*

object LoaderShipLawSpec extends ZIOSpecDefault:
  private val pin = dep"rocks.earlyeffect::shipped:1.0.0"

  def spec = suite("a ship pin is a jar on the wiring classpath")(
    test("the wiring sees the resolved jar and not a class directory"):
      prepared(Stamp.Launcher).flatMap: made =>
        load(made.root, shipBuild, answering(made.jar)).map: found =>
          val published = found.graph.coordinates.collect:
            case PluginCoordinate.Published(dependency, _) => dependency
          assertTrue(published == List(pin), found.plugins.links.isEmpty)
    ,
    test("a locked ship pin is fetched and is not solved"):
      prepared(Stamp.Launcher).flatMap: made =>
        val held    = DependencyLock(Nil, List(cell))
        val resolve = ShipResolve(
          _ => ZIO.fail(ResolveError.NotFound("called", "resolve", "0")),
          Some(made.jar),
        )
        for
          _ <- Workspace
            .writeString(made.root.resolve("dependencies.lock"), DependencyLock.render(held))
            .mapError(LoadError.Io(_))
          found <- load(made.root, lockedBuild, resolve)
        yield
          val published = found.graph.coordinates.collect:
            case PluginCoordinate.Published(dependency, _) => dependency
          assertTrue(published == List(pin), found.plugins.links.isEmpty)
    ,
    test("a lock that omits the pin does not fetch it"):
      prepared(Stamp.Launcher).flatMap: made =>
        val held = DependencyLock(
          Nil,
          List(
            LockedPlugin("other.org", "other", "1.0.0", LinkStrategy.BinaryOrSource, Nil, Nil, None)
          ),
        )
        for
          _ <- Workspace
            .writeString(made.root.resolve("dependencies.lock"), DependencyLock.render(held))
            .mapError(LoadError.Io(_))
          found <- load(made.root, lockedBuild, answering(made.jar)).either
        yield
          val blocked = found match
            case Left(LoadError.Plugin(PluginLoadError.Unlocked(coordinate))) =>
              coordinate == PluginCoordinate.Published(pin).render
            case _ => false
          assertTrue(blocked)
    ,
    test("a ship jar without the api digest is refused"):
      prepared(Stamp.Absent).flatMap: made =>
        load(made.root, shipBuild, answering(made.jar)).either.map: found =>
          val bare = found match
            case Left(LoadError.Plugin(PluginLoadError.Unversioned(coordinate))) =>
              coordinate == PluginCoordinate.Published(pin).render
            case _ => false
          assertTrue(bare)
    ,
    test("a ship jar built against another api jar is refused"):
      prepared(Stamp.Text(("ab" * 32) + "\n")).flatMap: made =>
        load(made.root, shipBuild, answering(made.jar)).either.map: found =>
          val foreign    = ApiDigest.trusted("ab" * 32)
          val mismatched = found match
            case Left(LoadError.Plugin(PluginLoadError.VersionMismatch(dependency, pluginApi, launcher))) =>
              dependency == pin && pluginApi == foreign && launcher == made.digest
            case _ => false
          assertTrue(mismatched)
    ,
    test("an api link is refused before resolve"):
      Workspace
        .work("histos-load-ship-api")
        .mapError(LoadError.Io(_))
        .flatMap: root =>
          load(root, apiBuild, refused).either.map: found =>
            val blocked = found match
              case Left(LoadError.Plugin(PluginLoadError.ApiLink(dependency))) => dependency == pin
              case _                                                           => false
            assertTrue(blocked)
    ,
    test("a ship pin that does not resolve fails the load"):
      Workspace
        .work("histos-load-ship-miss")
        .mapError(LoadError.Io(_))
        .flatMap: root =>
          load(root, shipBuild, missing).either.map: found =>
            val missed = found match
              case Left(LoadError.Resolve(ResolveError.NotFound(org, name, revision))) =>
                org == pin.organization && name == pin.name && revision == pin.revision
              case _ => false
            assertTrue(missed),
  )

  private val shipBuild =
    """import histos.*
      |
      |object build extends HistosBuild:
      |  def graph =
      |    val app = module("app", Jvm)(scala"3.10.0")
      |    Projects.plugins(PluginSources.ship(dep"rocks.earlyeffect::shipped:1.0.0"))(app)
      |""".stripMargin

  private val apiBuild =
    """import histos.*
      |import histos.plugin.LinkStrategy
      |
      |object build extends HistosBuild:
      |  def graph =
      |    val app = module("app", Jvm)(scala"3.10.0")
      |    Projects.plugins(PluginSources.ship(dep"rocks.earlyeffect::shipped:1.0.0").link(LinkStrategy.Api))(app)
      |""".stripMargin

  private val lockedBuild =
    """import histos.*
      |
      |object build extends HistosBuild:
      |  def graph =
      |    val app = module("app", Jvm)(scala"3.10.0")
      |    Projects.plugins(PluginSources.ship(dep"rocks.earlyeffect::shipped:1.0.0")).locked(app)
      |""".stripMargin

  private val wiring =
    """import histos.plugin.*
      |import zio.*
      |
      |object HistosWiring:
      |  val token: String = shipped.Mark.token
      |  def layer = ZLayer.succeed(Plugins(Chunk.empty))
      |""".stripMargin

  private val mark =
    """package shipped
      |
      |object Mark:
      |  val token: String = "ship"
      |""".stripMargin

  private enum Stamp:
    case Launcher
    case Text(raw: String)
    case Absent

  private final case class Prepared(root: Path, jar: Path, digest: ApiDigest)

  private def cell: LockedPlugin =
    LockedPlugin(
      pin.organization,
      pin.name,
      pin.revision,
      LinkStrategy.BinaryOrSource,
      List(LockedDirect(pin.organization, pin.name, pin.revision, pin.kind)),
      List(
        LockedArtifact(
          pin.organization,
          pin.name,
          pin.revision,
          Sha256.trusted("aa" * 32),
          histos.resolve.Repository.Central,
        )
      ),
      None,
    )

  private def prepared(stamp: Stamp) =
    Workspace
      .work("histos-load-ship")
      .mapError(LoadError.Io(_))
      .flatMap: root =>
        val source  = root.resolve("Mark.scala")
        val classes = root.resolve("classes")
        val jar     = root.resolve("shipped.jar")
        for
          api      <- LoaderApiJar.of(classOf[histos.HistosBuild], root.resolve("histos-api.jar"))
          digest   <- digestOf(api)
          boundary <- Boundary.jars(api)
          _        <- Workspace.writeString(source, mark).mapError(LoadError.Io(_))
          _        <- ZIO
            .attemptBlocking(Files.createDirectories(classes))
            .mapError(BuildFile.failed(classes, IoFault.Create))
          _ <- BuildFile.compileSources(List(source), boundary, classes)
          line = stamp match
            case Stamp.Launcher  => Some(s"${digest.value}\n")
            case Stamp.Text(raw) => Some(raw)
            case Stamp.Absent    => None
          _ <- jarClasses(classes, jar, line)
          _ <- Workspace.writeString(root.resolve("histos.wiring.scala"), wiring).mapError(LoadError.Io(_))
        yield Prepared(root, jar, digest)

  private def digestOf(api: Path): IO[LoadError, ApiDigest] =
    ZIO
      .attemptBlocking(Sha256Digest.of(Chunk.fromArray(Files.readAllBytes(api))))
      .mapError(BuildFile.failed(api, IoFault.Read))
      .flatMap: sha =>
        ApiDigest.from(sha.value) match
          case Right(digest) => ZIO.succeed(digest)
          case Left(_)       => ZIO.fail(LoadError.Io(histos.workspace.IoFailure(api, IoFault.Stat)))

  private def answering(jar: Path): Resolve =
    ShipResolve(dependency =>
      ZIO.succeed(
        Classpath(
          List(
            ResolvedArtifact(
              dependency.organization,
              dependency.name,
              dependency.revision,
              jar,
              Sha256.trusted("aa"),
              histos.resolve.Repository.Central,
            )
          ),
          Nil,
        )
      )
    )

  private val missing: Resolve =
    ShipResolve(dependency =>
      ZIO.fail(ResolveError.NotFound(dependency.organization, dependency.name, dependency.revision))
    )

  private val refused: Resolve =
    ShipResolve(_ => ZIO.fail(ResolveError.NotFound("called", "resolve", "0")))

  private def load(root: Path, build: String, resolve: Resolve) =
    val layer = (ZLayer.succeed(resolve) ++ ZLayer.succeed(ContentStore.in(root.resolve("store")))) >>> Loader.live
    val run   =
      for
        api   <- LoaderApiJar.of(classOf[histos.HistosBuild], root.resolve("histos-api.jar"))
        _     <- Workspace.writeString(root.resolve("histos.scala"), build).mapError(LoadError.Io(_))
        found <- ZIO.serviceWithZIO[Loader](_.load(root, api))
      yield found
    run.provideSomeLayer[Scope](layer)

  private def jarClasses(classes: Path, dest: Path, version: Option[String]): IO[LoadError, Unit] =
    ZIO
      .attemptBlocking:
        val output = new JarOutputStream(Files.newOutputStream(dest))
        val walk   = Files.walk(classes)
        try
          walk.iterator.asScala.foreach: path =>
            if Files.isRegularFile(path) then
              val name = classes.relativize(path).iterator.asScala.mkString("/")
              output.putNextEntry(new JarEntry(name))
              output.write(Files.readAllBytes(path))
              output.closeEntry()
          version match
            case Some(text) =>
              output.putNextEntry(new JarEntry(ApiDigest.resource))
              output.write(text.getBytes(java.nio.charset.StandardCharsets.UTF_8))
              output.closeEntry()
            case None => ()
        finally
          walk.close()
          output.close()
      .mapError(BuildFile.failed(dest, IoFault.Write))
      .unit

  private final class ShipResolve(answer: Dep => IO[ResolveError, Classpath], fetched: Option[Path] = None)
      extends Resolve:
    def resolvePlugin(dependency: Dep, repositories: histos.resolve.Repositories): IO[ResolveError, Classpath] =
      ZIO.succeed(repositories).zipRight(answer(dependency))

    def resolveTool(request: ToolRequest): IO[ResolveError, Classpath] =
      unused(request)

    def resolve(cell: ResolveCell): IO[ResolveError, Classpath] =
      unused(cell)

    def fetch(artifact: LockedArtifact): IO[ResolveError, ResolvedArtifact] =
      fetched match
        case Some(jar) =>
          ZIO.succeed(
            ResolvedArtifact(
              artifact.organization,
              artifact.name,
              artifact.revision,
              jar,
              artifact.sha256,
              artifact.repository,
              artifact.classifier,
            )
          )
        case None => unused(artifact)

    def sources(
        organization: String,
        name: String,
        revision: String,
        repository: histos.resolve.Repository,
    ): IO[ResolveError, ResolvedArtifact] =
      unused((organization, name, revision, repository))

    private def unused[A](seen: Any): IO[ResolveError, A] =
      ZIO.fail(ResolveError.NotFound("unused", seen.getClass.getName, "0"))
  end ShipResolve
end LoaderShipLawSpec
