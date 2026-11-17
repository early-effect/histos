package histos.load

import histos.jdk.Sha256Digest
import histos.plugin.{ApiDigest, ContentHash, LockRevision}
import histos.resolve.{Classpath, LockCodec, Repository, Resolve, ResolvedArtifact}
import histos.workspace.Workspace
import histos.Dep

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.util.zip.{ZipEntry, ZipOutputStream}

import scala.jdk.CollectionConverters.*

import zio.test.*
import zio.{Chunk, Scope, UIO, ZIO, ZLayer, durationInt}

/** A custom lock strategy reads the file revision, the catalog intent, and the plugin diff. */
object PluginLockContextSpec extends ZIOSpecDefault:
  def spec = suite("plugin lock context")(
    test("an added catalog coordinate is a present revision, the catalog intent, and one added entry") {
      observe(seedLock, catalog).map: text =>
        val seen = lines(text)
        val hash = declarations(
          List(
            "com.example::seedplug:1.0.0 binary-or-source",
            "com.example::addedplug:1.0.0 binary-or-source",
          )
        )
        assertTrue(
          seen.contains(s"revision ${revision(seedLock).value}"),
          seen.contains("intent solve"),
          seen.contains(s"drift changed ${hash.value}"),
          seen.contains("entry added com.example::addedplug:1.0.0"),
          seen.count(_.startsWith("entry ")) == 1,
        )
    },
    test("a matching plugin list is the file revision, the catalog intent, and an empty diff") {
      observe(bothLock, catalog).map: text =>
        val seen = lines(text)
        assertTrue(
          seen.contains(s"revision ${revision(bothLock).value}"),
          seen.contains("intent solve"),
          seen.contains("drift same"),
          !seen.exists(_.startsWith("entry ")),
        )
    },
    test("a file that is not a dependency lock keeps its revision and is not Same") {
      observe(garbage, catalog).map: text =>
        val seen = lines(text)
        val hash = declarations(
          List(
            "com.example::seedplug:1.0.0 binary-or-source",
            "com.example::addedplug:1.0.0 binary-or-source",
          )
        )
        assertTrue(
          seen.contains(s"revision ${revision(garbage).value}"),
          seen.contains("intent solve"),
          seen.contains(s"drift changed ${hash.value}"),
          !seen.exists(_.startsWith("entry ")),
        )
    },
    test("a link change is one changed coordinate, not an add and a remove") {
      observe(bothLock, exactCatalog).map: text =>
        val seen = lines(text)
        val hash = declarations(
          List(
            "com.example::seedplug:1.0.0 binary-or-source",
            "com.example::addedplug:1.0.0 exact-binary",
          )
        )
        assertTrue(
          seen.contains(s"revision ${revision(bothLock).value}"),
          seen.contains("intent solve"),
          seen.contains(s"drift changed ${hash.value}"),
          seen.contains("entry changed com.example::addedplug:1.0.0"),
          seen.count(_.startsWith("entry ")) == 1,
        )
    },
  ) @@ TestAspect.withLiveClock @@ TestAspect.timeout(10.minutes)

  private def observe(lock: String, plugins: String): ZIO[Any, LoadError, String] =
    ZIO.scoped:
      for
        root   <- work("histos-m4-lock")
        api    <- apiJar(root.resolve("api.jar"))
        digest <- digestOf(api)
        seed   <- pluginJar(root.resolve("seed.jar"), digest)
        added  <- pluginJar(root.resolve("added.jar"), digest)
        facts = root.resolve("facts.txt")
        _ <- write(root.resolve("dependencies.lock"), lock)
        _ <- write(root.resolve("histos.plugins.scala"), plugins)
        _ <- write(root.resolve("histos.strategies.scala"), strategies(facts))
        _ <- write(root.resolve("histos.scala"), main)
        _ <- BootstrapLoad
          .prepare(jars(seed, added), BootstrapLoad.Prepare(root, api))
          .provideSomeLayer[Scope](BuildCache.ephemeral)
        text <- ZIO.attemptBlocking(Files.readString(facts, StandardCharsets.UTF_8)).orDie
      yield text

  private def lines(text: String): List[String] =
    text.linesIterator.toList

  private def revision(text: String): LockRevision =
    LockCodec.revision(text)

  private def declarations(pins: List[String]): ContentHash =
    LockCodec.content(pins.mkString("\n"))

  private val seedLock: String =
    """|plugin com.example seedplug 1.0.0 binary-or-source
       |""".stripMargin

  private val bothLock: String =
    """|plugin com.example seedplug 1.0.0 binary-or-source
       |plugin com.example addedplug 1.0.0 binary-or-source
       |""".stripMargin

  private val garbage: String = "not a lock\n"

  private val catalog: String =
    """|import histos.*
       |import histos.plugin.*
       |
       |object plugins extends HistosPlugins:
       |  private val seed = PluginSources.ship(dep"com.example::seedplug:1.0.0")
       |  private val added = PluginSources.ship(dep"com.example::addedplug:1.0.0")
       |
       |  override def resolution = Resolution.Solve
       |  def sources = PluginCatalog(seed, added)
       |  override def bootstrap =
       |    BootstrapSource.File(RepoPath("histos.strategies.scala"), required = PluginCatalog(seed))
       |""".stripMargin

  private val exactCatalog: String =
    """|import histos.*
       |import histos.plugin.*
       |
       |object plugins extends HistosPlugins:
       |  private val seed = PluginSources.ship(dep"com.example::seedplug:1.0.0")
       |  private val added =
       |    PluginSources.ship(dep"com.example::addedplug:1.0.0").link(LinkStrategy.ExactBinary)
       |
       |  override def resolution = Resolution.Solve
       |  def sources = PluginCatalog(seed, added)
       |  override def bootstrap =
       |    BootstrapSource.File(RepoPath("histos.strategies.scala"), required = PluginCatalog(seed))
       |""".stripMargin

  private val main: String =
    """|import histos.*
       |
       |object build extends HistosBuild:
       |  def graph =
       |    val app = module("app", Jvm)(scala"3.10.1")
       |    Projects(app)
       |""".stripMargin

  private def strategies(log: Path): String =
    s"""|import histos.*
        |import histos.lock.*
        |import histos.plugin.*
        |import zio.{ZIO, ZLayer}
        |
        |final class Watching extends LockStrategy:
        |  def decide(context: LockContext) =
        |    val revision = context.revision match
        |      case LockRevisionState.Absent           => "absent"
        |      case LockRevisionState.Present(present) => present.value
        |    val intent = context.intent match
        |      case ResolutionIntent.Inherit                  => "inherit"
        |      case ResolutionIntent.Select(Resolution.Solve) => "solve"
        |      case ResolutionIntent.Select(Resolution.Lock)  => "lock"
        |    val drift = context.drift match
        |      case DeclarationDrift.Same             => "same"
        |      case DeclarationDrift.Changed(changed) => "changed " + changed.value
        |    val entries = context.scope.diff.entries.map: entry =>
        |      val change = entry.change match
        |        case LockChange.Added   => "added"
        |        case LockChange.Removed => "removed"
        |        case LockChange.Changed => "changed"
        |      "entry " + change + " " + entry.coordinate.render
        |    val body =
        |      (List("revision " + revision, "intent " + intent, "drift " + drift) ++ entries).mkString("\\n") + "\\n"
        |    ZIO.attemptBlocking {
        |      java.nio.file.Files.writeString(java.nio.file.Path.of("${log.toString}"), body)
        |      LockDecision.OfferRepair
        |    }.orDie
        |
        |object Watching:
        |  def layer: ZLayer[Any, Nothing, LockStrategy] =
        |    ZLayer.succeed[LockStrategy](new Watching)
        |
        |object strategies extends HistosStrategies:
        |  def services = StrategyServices.default.withLocks(Watching.layer)
        |""".stripMargin

  private def jars(seed: Path, added: Path): Resolve =
    new Resolve:
      def resolveTool(request: histos.resolve.ToolRequest)                          = ZIO.dieMessage("resolveTool")
      def resolve(cell: histos.resolve.ResolveCell)                                 = ZIO.dieMessage("resolve")
      def fetch(artifact: histos.resolve.LockedArtifact)                            = ZIO.dieMessage("fetch")
      def resolvePlugin(dependency: Dep, repositories: histos.resolve.Repositories) =
        dependency.name match
          case "seedplug"  => ZIO.succeed(Classpath(List(pinned(dependency, seed)), Nil))
          case "addedplug" => ZIO.succeed(Classpath(List(pinned(dependency, added)), Nil))
          case other       => ZIO.dieMessage(other)
      def sources(organization: String, name: String, revision: String, repository: Repository) =
        ZIO.dieMessage("sources")

  private def pinned(dependency: Dep, path: Path): ResolvedArtifact =
    ResolvedArtifact(
      dependency.organization,
      dependency.name,
      dependency.revision.value,
      path,
      Sha256Digest.of(Chunk.fromArray(path.toString.getBytes(StandardCharsets.UTF_8))),
      Repository.Central,
    )

  private def work(name: String): ZIO[Scope, Nothing, Path] =
    Workspace.work(name).foldZIO(failure => ZIO.dieMessage(failure.message), ZIO.succeed)

  private def write(path: Path, text: String): UIO[Path] =
    ZIO.attemptBlocking {
      Option(path.getParent).foreach(parent => Files.createDirectories(parent))
      Files.writeString(path, text, StandardCharsets.UTF_8)
      path
    }.orDie

  private def apiJar(destination: Path): UIO[Path] =
    ZIO.attemptBlocking {
      val located = Path.of(classOf[histos.HistosBuild].getProtectionDomain.getCodeSource.getLocation.toURI)
      if Files.isRegularFile(located) then located
      else
        Files.createDirectories(destination.getParent)
        val out  = new ZipOutputStream(Files.newOutputStream(destination))
        val walk = Files.walk(located)
        try
          walk.iterator.asScala.foreach: path =>
            if Files.isRegularFile(path) then
              val name = located.relativize(path).toString.replace('\\', '/')
              out.putNextEntry(new ZipEntry(name))
              out.write(Files.readAllBytes(path))
              out.closeEntry()
        finally
          walk.close()
          out.close()
        destination
    }.orDie

  private def digestOf(path: Path): UIO[String] =
    ZIO.attemptBlocking(Sha256Digest.of(Chunk.fromArray(Files.readAllBytes(path))).value).orDie

  private def pluginJar(path: Path, version: String): UIO[Path] =
    ZIO.attemptBlocking {
      Option(path.getParent).foreach(parent => Files.createDirectories(parent))
      val out = new ZipOutputStream(Files.newOutputStream(path))
      try
        val entry = new ZipEntry(ApiDigest.resource)
        out.putNextEntry(entry)
        out.write(version.getBytes(StandardCharsets.UTF_8))
        out.closeEntry()
        val marker = new ZipEntry("marker.txt")
        out.putNextEntry(marker)
        out.write(Array[Byte](1))
        out.closeEntry()
      finally out.close()
      path
    }.orDie
end PluginLockContextSpec
