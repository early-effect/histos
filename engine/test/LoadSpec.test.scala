package histos.load

import dotty.tools.dotc.Driver
import dotty.tools.dotc.reporting.StoreReporter
import histos.cache.ContentStore
import histos.jdk.Sha256
import histos.plugin.*
import histos.resolve.*
import histos.schedule.{ActionCache, RemoteCache}
import histos.{BuildError, Dep, ProjectPath}

import java.nio.file.{Files, Path}
import java.util.jar.{JarEntry, JarOutputStream}

import scala.jdk.CollectionConverters.*

import zio.*
import zio.test.*

object LoadSpec extends ZIOSpecDefault:
  private val apiJar: IO[LoadError, Path] =
    val candidates = rooted("dist/histos-api.jar", "../dist/histos-api.jar")
    ZIO.attempt(candidates.find(Files.isRegularFile(_))).mapError(t => LoadError.Io(LoadIo.Temp)).flatMap:
      case Some(jar) => ZIO.succeed(jar)
      case None      => ZIO.fail(LoadError.MissingFile(Path.of("dist/histos-api.jar")))

  private def rooted(names: String*): List[Path] =
    val root = sys.env.get("HISTOS_REPO").filter(_.nonEmpty).map(Path.of(_))
    root.toList.flatMap(base => names.map(base.resolve)) ++ names.map(Path.of(_))

  private def withRepo(source: String): ZIO[Scope, LoadError, Path] =
    for
      dir <- ZIO.attempt(Files.createTempDirectory("histos-repo")).mapError(t => LoadError.Io(LoadIo.Temp))
      _   <- ZIO.addFinalizer(deleteTree(dir))
      _   <- ZIO.attempt(Files.writeString(dir.resolve("histos.scala"), source)).mapError(t => LoadError.Io(LoadIo.Temp))
    yield dir

  private def deleteTree(root: Path): UIO[Unit] =
    ZIO.attempt {
      if Files.exists(root) then
        val walk = Files.walk(root)
        val paths =
          try walk.iterator.asScala.toList
          finally walk.close()
        paths.sortBy(_.getNameCount).reverse.foreach(path => Files.deleteIfExists(path))
    }.ignore

  private def load(source: String): ZIO[Loader & Scope & ContentStore & Resolve, LoadError, LoadedBuild] =
    for
      jar <- apiJar
      dir <- withRepo(source)
      out <- ZIO.serviceWithZIO[Loader](_.load(dir, jar))
    yield out

  private val happy =
    """import histos.*
      |object build extends HistosBuild:
      |  def graph =
      |    val core = module("core", Jvm)(scala"3.9.0")
      |    Projects(core)
      |""".stripMargin

  private val loads = suite("loading histos.scala")(
    test("evaluates the graph and does not compile the project"):
      ZIO.scoped:
        for loaded <- load(happy)
        yield assertTrue:
          loaded.graph.projects.map(_.name.value) == List("core")
    ,
    test("a duplicate name is a broken graph"):
      val source =
        """import histos.*
          |object build extends HistosBuild:
          |  def graph = Projects(module("core", Jvm)(scala"3.9.0"), module("core", Js)(scala"3.9.0"))
          |""".stripMargin
      ZIO.scoped:
        for loaded <- load(source).either
        yield assertTrue:
          loaded match
            case Left(LoadError.BrokenGraph(BuildError.DuplicateName(name, _, _))) => name.value == "core"
            case _                                                                  => false
    ,
    test("a type error is a compile failure"):
      val source =
        """import histos.*
          |object build extends HistosBuild:
          |  def graph = Projects(module("core", Jvm)(scala"nope"))
          |""".stripMargin
      ZIO.scoped:
        for loaded <- load(source).either
        yield assertTrue:
          loaded match
            case Left(LoadError.CompileFailed(diagnostics)) =>
              val text = diagnostics.map(_.text).mkString("\n")
              text.contains("nope") && !text.contains("histos-build")
            case _ => false
    ,
    test("the build file cannot see the engine"):
      val source =
        """import histos.*
          |object build extends HistosBuild:
          |  def graph =
          |    val loader = histos.load.Loader
          |    Projects()
          |""".stripMargin
      ZIO.scoped:
        for loaded <- load(source).either
        yield assertTrue:
          loaded match
            case Left(LoadError.CompileFailed(_)) => true
            case _                                => false
    ,
    test("a missing build file is its own error"):
      ZIO.scoped:
        for
          jar <- apiJar
          dir <- ZIO.attempt(Files.createTempDirectory("histos-empty")).mapError(t => LoadError.Io(LoadIo.Temp))
          _   <- ZIO.addFinalizer(deleteTree(dir))
          out <- ZIO.serviceWithZIO[Loader](_.load(dir, jar)).either
        yield assertTrue:
          out match
            case Left(LoadError.MissingFile(path)) => path.endsWith("histos.scala")
            case _                                 => false
  )

  private val echo = Dep("dev.example::echo:1.0.0")

  private val echoSource =
    """package dev.example
      |
      |import histos.plugin.*
      |import zio.{Scope, ZIO}
      |
      |object Echo extends HistosPlugin:
      |  val id: PluginId = PluginId("dev.example")
      |
      |  def contribute: Contribution =
      |    Contribution(
      |      List(DeclaredTask(TaskName("echo"), Seal.Open, Set.empty, Nil, Nil, Nil)),
      |      Nil,
      |      Nil,
      |    )
      |
      |  def run(request: RunRequest): ZIO[Scope, PluginRunError, PluginOutput] =
      |    ZIO.succeed(PluginOutput("", Nil))
      |""".stripMargin

  private val header = suite("plugin header")(
    test("a file with no plugin line is unchanged"):
      val text = "import histos.*\nobject build extends HistosBuild"
      assertTrue(HeaderScan.scan(text) == Right(ScannedBuild(Nil, text)))
    ,
    test("comments and blank lines stay, and a plugin line is blanked as binary-or-source"):
      val text = "\n// note\nplugin(dep\"dev.example::echo:1.0.0\")\nimport histos.*\n"
      HeaderScan.scan(text) match
        case Right(scanned) =>
          assertTrue(
            scanned.requests == List(PluginRequest.Dep(echo, LinkStrategy.BinaryOrSource)),
            scanned.source == "\n// note\n\nimport histos.*\n",
            !scanned.source.contains("plugin("),
          )
        case Left(_) => assertTrue(false)
    ,
    test("an explicit ExactBinary link is the same request"):
      HeaderScan.scan("plugin(dep\"dev.example::echo:1.0.0\").link(ExactBinary)") match
        case Right(scanned) =>
          assertTrue(scanned.requests == List(PluginRequest.Dep(echo, LinkStrategy.ExactBinary)), scanned.source == "")
        case Left(_) => assertTrue(false)
    ,
    test("Api is refused at scan, and Source and BinaryOrSource are requests"):
      val api = HeaderScan.scan("plugin(dep\"dev.example::echo:1.0.0\").link(Api)")
      val source = HeaderScan.scan("plugin(dep\"dev.example::echo:1.0.0\").link(Source)")
      val either = HeaderScan.scan("plugin(dep\"dev.example::echo:1.0.0\").link(BinaryOrSource)")
      assertTrue(
        api == Left(PluginLoadError.ApiLink(echo)),
        source == Right(ScannedBuild(List(PluginRequest.Dep(echo, LinkStrategy.Source)), "")),
        either == Right(ScannedBuild(List(PluginRequest.Dep(echo, LinkStrategy.BinaryOrSource)), "")),
      )
    ,
    test("a path plugin is a path request, and a binary link on a path is refused"):
      val path = ProjectPath("tools/docs")
      val bare = HeaderScan.scan("plugin(path\"tools/docs\")")
      val sourced = HeaderScan.scan("plugin(path\"tools/docs\").link(Source)")
      val binary = HeaderScan.scan("plugin(path\"tools/docs\").link(ExactBinary)")
      val either = HeaderScan.scan("plugin(path\"tools/docs\").link(BinaryOrSource)")
      val api = HeaderScan.scan("plugin(path\"tools/docs\").link(Api)")
      assertTrue(
        bare == Right(ScannedBuild(List(PluginRequest.Path(path)), "")),
        sourced == Right(ScannedBuild(List(PluginRequest.Path(path)), "")),
        binary == Left(PluginLoadError.PathIsSource(path)),
        either == Left(PluginLoadError.PathIsSource(path)),
        api == Left(PluginLoadError.PathIsSource(path)),
        HeaderScan.paths("plugin(path\"tools/docs\")\nplugin(dep\"dev.example::echo:1.0.0\")") == List(path),
        HeaderScan.paths("plugin(dep\"dev.example::echo:1.+\")") == Nil,
      )
    ,
    test("a range, a trailing semicolon, and a plugin after the header are refused"):
      val ranged = HeaderScan.scan("plugin(dep\"dev.example::echo:1.+\")")
      val latest = HeaderScan.scan("plugin(dep\"dev.example::echo:latest.release\")")
      val semi = HeaderScan.scan("plugin(dep\"dev.example::echo:1.0.0\");")
      val moved = HeaderScan.scan("import histos.*\nplugin(dep\"dev.example::echo:1.0.0\")")
      assertTrue(
        ranged == Left(PluginLoadError.BadCoordinate("dev.example::echo:1.+")),
        latest == Left(PluginLoadError.BadCoordinate("dev.example::echo:latest.release")),
        semi == Left(PluginLoadError.BadCoordinate("plugin(dep\"dev.example::echo:1.0.0\");")),
        moved == Left(PluginLoadError.Misplaced(2)),
      )
  )

  private val plugins = suite("exact binary plugins")(
    test("an enum build loads before any plugin jar is added"):
      val source =
        """import histos.*
          |
          |enum Color:
          |  case Red, Blue
          |
          |object build extends HistosBuild:
          |  def graph =
          |    val ordinal = Color.Red.ordinal
          |    Projects(module("core", Jvm)(scala"3.9.0"))
          |""".stripMargin
      ZIO.scoped:
        for loaded <- load(source)
        yield assertTrue(loaded.graph.projects.map(_.name.value) == List("core"), loaded.plugins.links.isEmpty)
    ,
    test("a header dependency with no lock is unlocked, and the body is not compiled"):
      val source =
        """plugin(dep"dev.example::echo:1.0.0")
          |import histos.*
          |object build extends HistosBuild:
          |  def graph = Projects(module("core", Jvm)(scala"nope"))
          |""".stripMargin
      ZIO.scoped:
        for
          dir <- withRepo(source)
          jar <- apiJar
          out <- ZIO.serviceWithZIO[Loader](_.load(dir, jar)).either
        yield assertTrue:
          out match
            case Left(LoadError.Plugin(PluginLoadError.Unlocked(coordinate))) =>
              coordinate == PluginCoordinate.Published(echo).render && !Files.exists(Resolve.lockPath(dir))
            case _ => false
    ,
    test("Api writes no lock, and a missing path directory is no entry"):
      val api =
        """plugin(dep"dev.example::echo:1.0.0").link(Api)
          |import histos.*
          |object build extends HistosBuild:
          |  def graph = Projects()
          |""".stripMargin
      val path =
        """plugin(path"tools/docs")
          |import histos.*
          |object build extends HistosBuild:
          |  def graph = Projects()
          |""".stripMargin
      ZIO.scoped:
        for
          apiDir <- withRepo(api)
          pathDir <- withRepo(path)
          jar <- apiJar
          apiOut <- ZIO.serviceWithZIO[Loader](_.load(apiDir, jar)).either
          pathOut <- ZIO.serviceWithZIO[Loader](_.load(pathDir, jar)).either
        yield assertTrue(
          apiOut == Left(LoadError.Plugin(PluginLoadError.ApiLink(echo))),
          pathOut == Left(LoadError.Plugin(PluginLoadError.NoEntry(PluginCoordinate.Path(ProjectPath("tools/docs")).render))),
          !Files.exists(Resolve.lockPath(apiDir)),
          !Files.exists(Resolve.lockPath(pathDir)),
        )
    ,
    test("a digest mismatch names both digests and does not load the class"):
      ZIO.scoped:
        for
          jar <- apiJar
          hex <- apiHex(jar)
          dir <- ZIO.attempt(Files.createTempDirectory("histos-mismatch")).mapError(_ => LoadError.Io(LoadIo.Temp))
          _ <- ZIO.addFinalizer(deleteTree(dir))
          wrong = "ab" * 32
          packed <- textJar(dir.resolve("echo.jar"), List(
                       HistosPlugin.entryResource -> "dev.example.Echo$\n",
                       ApiDigest.resource -> s"$wrong\n",
                       "dev/example/Echo$.class" -> "not a class",
                     ))
          repo <- withRepo(pluginBuild)
          sha <- fileSha(packed)
          _ <- writeLock(repo, sha)
          out <- loadWith(repo, jar, Holding(packed, sha))
        yield assertTrue:
          out match
            case Left(error @ LoadError.Plugin(PluginLoadError.VersionMismatch(dependency, pluginApi, launcher))) =>
              dependency == echo &&
                pluginApi.value == wrong &&
                launcher.value == hex &&
                error.message.contains(wrong) &&
                error.message.contains(hex)
            case _ => false
    ,
    test("a jar that contains histos.schedule is an engine leak"):
      ZIO.scoped:
        for
          jar <- apiJar
          hex <- apiHex(jar)
          dir <- ZIO.attempt(Files.createTempDirectory("histos-leak")).mapError(_ => LoadError.Io(LoadIo.Temp))
          _ <- ZIO.addFinalizer(deleteTree(dir))
          packed <- textJar(dir.resolve("echo.jar"), List(
                       HistosPlugin.entryResource -> "dev.example.Echo$\n",
                       ApiDigest.resource -> s"$hex\n",
                       "histos/schedule/Sendable.class" -> "not a class",
                     ))
          repo <- withRepo(pluginBuild)
          sha <- fileSha(packed)
          _ <- writeLock(repo, sha)
          out <- loadWith(repo, jar, Holding(packed, sha))
        yield assertTrue:
          out match
            case Left(LoadError.Plugin(PluginLoadError.EngineLeak(coordinate, binary))) =>
              coordinate == PluginCoordinate.Published(echo) && binary == "histos.schedule.Sendable"
            case _ => false
    ,
    test("an exact-binary plugin contributes, links from the jar, and run returns its text"):
      ZIO.scoped:
        for
          jar <- apiJar
          hex <- apiHex(jar)
          packed <- compilePlugin(jar, hex)
          repo <- withRepo(usingEcho)
          sha <- fileSha(packed)
          _ <- writeLock(repo, sha)
          loaded <- loadWith(repo, jar, Holding(packed, sha)).flatMap(ZIO.fromEither)
          task = PluginTaskId(PluginId("dev.example"), TaskName("echo"))
          ran <- loaded.plugins.run(task, PluginArgs.empty).provide(ZLayer.succeed(ActionCache.miss), RemoteCache.off)
          bound <- loaded.plugins.bind.provide(ZLayer.succeed(ActionCache.miss), RemoteCache.off)
        yield assertTrue(
          loaded.graph.projects.map(_.name.value) == List("core"),
          loaded.graph.plugins match
            case plugin :: Nil =>
              plugin.id == PluginId("dev.example") &&
                plugin.entry == EntryName("dev.example.Echo$") &&
                plugin.contribution.tasks.map(_.name) == List(TaskName("echo"))
            case _ => false
          ,
          loaded.plugins.links match
            case PluginLink(id, PluginLinked.FromBinary(digest), artifacts) :: Nil =>
              id == PluginId("dev.example") && digest.value == hex && artifacts == List(packed.toString)
            case _ => false
          ,
          bound.keys.map(_.value).toList == List(task.render),
          ran == PluginOutput("", Nil),
        )
    ,
    test("a plugin object defined by the build file is a foreign entry"):
      ZIO.scoped:
        for
          jar <- apiJar
          hex <- apiHex(jar)
          packed <- compilePlugin(jar, hex)
          repo <- withRepo(usingLocal)
          sha <- fileSha(packed)
          _ <- writeLock(repo, sha)
          out <- loadWith(repo, jar, Holding(packed, sha))
        yield assertTrue:
          out match
            case Left(LoadError.Plugin(PluginLoadError.ForeignEntry(id, entry))) =>
              id == PluginId("dev.local") && entry == EntryName("Local$")
            case _ => false
    ,
    test("a plugin jar cannot define histos.schedule, scala, or the build trait"):
      ZIO.scoped:
        for
          jar <- apiJar
          boundary <- Boundary.jars(jar)
          dir <- ZIO.attempt(Files.createTempDirectory("histos-boundary")).mapError(_ => LoadError.Io(LoadIo.Temp))
          _ <- ZIO.addFinalizer(deleteTree(dir))
          vendor <- textJar(dir.resolve("vendor.jar"), List(
                       "histos/schedule/Sendable.class" -> "not a class",
                       "scala/collection/immutable/List.class" -> "not a class",
                       "histos/HistosBuild.class" -> "not a class",
                     ))
          probed <- ZIO.attempt(probe(boundary, vendor))
        yield assertTrue(probed == Refused(Some("histos.schedule.Sendable"), true, true))
    ,
    test("a matching binary-or-source cell loads the jar and does not compile sources"):
      ZIO.scoped:
        for
          jar <- apiJar
          hex <- apiHex(jar)
          packed <- compilePlugin(jar, hex)
          repo <- withRepo(usingEcho)
          sha <- fileSha(packed)
          _ <- writeCell(repo, "binary-or-source", sha, None)
          loaded <- loadWith(repo, jar, Holding(packed, sha)).flatMap(ZIO.fromEither)
          task = PluginTaskId(PluginId("dev.example"), TaskName("echo"))
          ran <- loaded.plugins.run(task, PluginArgs.empty).provide(ZLayer.succeed(ActionCache.miss), RemoteCache.off)
        yield assertTrue(
          loaded.plugins.links match
            case PluginLink(_, PluginLinked.FromBinary(digest), artifacts) :: Nil =>
              digest.value == hex && artifacts == List(packed.toString)
            case _ => false
          ,
          ran == PluginOutput("", Nil),
        )
    ,
    test("binary-or-source compiles the locked sources jar when the api digest differs"):
      ZIO.scoped:
        for
          jar <- apiJar
          hex <- apiHex(jar)
          dir <- ZIO.attempt(Files.createTempDirectory("histos-fallback")).mapError(_ => LoadError.Io(LoadIo.Temp))
          _ <- ZIO.addFinalizer(deleteTree(dir))
          wrong = "ab" * 32
          packed <- textJar(dir.resolve("echo.jar"), List(
                       HistosPlugin.entryResource -> "dev.example.Echo$\n",
                       ApiDigest.resource -> s"$wrong\n",
                     ))
          sources <- textJar(dir.resolve("echo-sources.jar"), List("Echo.scala" -> echoSource))
          repo <- withRepo(usingEcho)
          binarySha <- fileSha(packed)
          sourceSha <- fileSha(sources)
          _ <- writeCell(repo, "binary-or-source", binarySha, Some(sourceSha))
          loaded <- loadWith(repo, jar, Sourced(packed, binarySha, sources, sourceSha, allowSources = true)).flatMap(ZIO.fromEither)
          task = PluginTaskId(PluginId("dev.example"), TaskName("echo"))
          ran <- loaded.plugins.run(task, PluginArgs.empty).provide(ZLayer.succeed(ActionCache.miss), RemoteCache.off)
        yield assertTrue(
          loaded.plugins.links match
            case PluginLink(_, PluginLinked.FromSource(published, running), artifacts) :: Nil =>
              published.value == wrong && running.value == hex && artifacts.forall(underPlugins) && !artifacts.contains(packed.toString)
            case _ => false
          ,
          ran == PluginOutput("", Nil),
        )
    ,
    test("exact-binary does not fall back when a source line is present"):
      ZIO.scoped:
        for
          jar <- apiJar
          dir <- ZIO.attempt(Files.createTempDirectory("histos-exact")).mapError(_ => LoadError.Io(LoadIo.Temp))
          _ <- ZIO.addFinalizer(deleteTree(dir))
          wrong = "ab" * 32
          packed <- textJar(dir.resolve("echo.jar"), List(
                       HistosPlugin.entryResource -> "dev.example.Echo$\n",
                       ApiDigest.resource -> s"$wrong\n",
                     ))
          sources <- textJar(dir.resolve("echo-sources.jar"), List("Echo.scala" -> echoSource))
          repo <- withRepo(pluginBuild)
          binarySha <- fileSha(packed)
          sourceSha <- fileSha(sources)
          _ <- writeCell(repo, "exact-binary", binarySha, Some(sourceSha))
          out <- loadWith(repo, jar, Sourced(packed, binarySha, sources, sourceSha, allowSources = false))
        yield assertTrue:
          out match
            case Left(LoadError.Plugin(PluginLoadError.VersionMismatch(dependency, pluginApi, _))) =>
              dependency == echo && pluginApi.value == wrong
            case _ => false
    ,
    test("binary-or-source with no source line is NoSource and does not fetch"):
      ZIO.scoped:
        for
          jar <- apiJar
          dir <- ZIO.attempt(Files.createTempDirectory("histos-nosource")).mapError(_ => LoadError.Io(LoadIo.Temp))
          _ <- ZIO.addFinalizer(deleteTree(dir))
          wrong = "cd" * 32
          packed <- textJar(dir.resolve("echo.jar"), List(
                       HistosPlugin.entryResource -> "dev.example.Echo$\n",
                       ApiDigest.resource -> s"$wrong\n",
                     ))
          repo <- withRepo(pluginBuild)
          binarySha <- fileSha(packed)
          _ <- writeCell(repo, "binary-or-source", binarySha, None)
          out <- loadWith(repo, jar, Sourced(packed, binarySha, packed, binarySha, allowSources = false))
        yield assertTrue:
          out match
            case Left(error @ LoadError.Plugin(PluginLoadError.NoSource(coordinate))) =>
              coordinate == PluginCoordinate.Published(echo) &&
                error.message == s"plugin ${coordinate.render} needs sources, and this loader does not have them"
            case _ => false
    ,
    test("link Source compiles even when the api digests match"):
      ZIO.scoped:
        for
          jar <- apiJar
          hex <- apiHex(jar)
          packed <- compilePlugin(jar, hex)
          dir <- ZIO.attempt(Files.createTempDirectory("histos-source-link")).mapError(_ => LoadError.Io(LoadIo.Temp))
          _ <- ZIO.addFinalizer(deleteTree(dir))
          sources <- textJar(dir.resolve("echo-sources.jar"), List("Echo.scala" -> echoSource))
          repo <- withRepo(usingEcho)
          binarySha <- fileSha(packed)
          sourceSha <- fileSha(sources)
          _ <- writeCell(repo, "source", binarySha, Some(sourceSha))
          loaded <- loadWith(repo, jar, Sourced(packed, binarySha, sources, sourceSha, allowSources = true)).flatMap(ZIO.fromEither)
        yield assertTrue:
          loaded.plugins.links match
            case PluginLink(_, PluginLinked.FromSource(published, running), artifacts) :: Nil =>
              published.value == hex && running.value == hex && artifacts.forall(underPlugins) && !artifacts.contains(packed.toString)
            case _ => false
    ,
    test("a path plugin compiles from the tree and does not write a lock"):
      ZIO.scoped:
        for
          jar <- apiJar
          hex <- apiHex(jar)
          repo <- withRepo(usingPath)
          _ <- ZIO.attempt {
                 val root = repo.resolve("tools").resolve("docs")
                 Files.createDirectories(root.resolve("META-INF").resolve("histos"))
                 Files.writeString(root.resolve("Echo.scala"), echoSource)
                 Files.writeString(root.resolve("META-INF").resolve("histos").resolve("plugin"), "dev.example.Echo$\n")
               }.mapError(_ => LoadError.Io(LoadIo.Temp))
          loaded <- ZIO.serviceWithZIO[Loader](_.load(repo, jar))
          task = PluginTaskId(PluginId("dev.example"), TaskName("echo"))
          ran <- loaded.plugins.run(task, PluginArgs.empty).provide(ZLayer.succeed(ActionCache.miss), RemoteCache.off)
        yield assertTrue(
          !Files.exists(Resolve.lockPath(repo)),
          loaded.plugins.links match
            case PluginLink(id, PluginLinked.FromSource(published, running), artifacts) :: Nil =>
              id == PluginId("dev.example") && published.value == hex && running.value == hex && artifacts.forall(underPlugins)
            case _ => false
          ,
          ran == PluginOutput("", Nil),
        )
  )

  private val pluginBuild =
    """plugin(dep"dev.example::echo:1.0.0")
      |import histos.*
      |object build extends HistosBuild:
      |  def graph = Projects(module("core", Jvm)(scala"3.9.0"))
      |""".stripMargin

  private val usingEcho =
    """plugin(dep"dev.example::echo:1.0.0")
      |import histos.*
      |import dev.example.Echo
      |object build extends HistosBuild:
      |  def graph = Projects(use(Echo), module("core", Jvm)(scala"3.9.0"))
      |""".stripMargin

  private val usingLocal =
    """plugin(dep"dev.example::echo:1.0.0")
      |import histos.*
      |import histos.plugin.*
      |import zio.{Scope, ZIO}
      |
      |object Local extends HistosPlugin:
      |  val id: PluginId = PluginId("dev.local")
      |
      |  def contribute: Contribution =
      |    Contribution(Nil, Nil, Nil)
      |
      |  def run(request: RunRequest): ZIO[Scope, PluginRunError, PluginOutput] =
      |    ZIO.succeed(PluginOutput("", Nil))
      |
      |object build extends HistosBuild:
      |  def graph = Projects(use(Local), module("core", Jvm)(scala"3.9.0"))
      |""".stripMargin

  private val usingPath =
    """plugin(path"tools/docs")
      |import histos.*
      |import dev.example.Echo
      |object build extends HistosBuild:
      |  def graph = Projects(use(Echo), module("core", Jvm)(scala"3.9.0"))
      |""".stripMargin

  private object IdleResolve extends Resolve:
    def resolve(cell: ResolveCell): IO[ResolveError, Classpath] =
      ZIO.fail(ResolveError.Repository(RepositoryIo.Fetch))

    def fetch(artifact: LockedArtifact): IO[ResolveError, ResolvedArtifact] =
      ZIO.fail(ResolveError.Repository(RepositoryIo.Fetch))

    def resolvePlugin(dependency: Dep, repositories: Repositories): IO[ResolveError, Classpath] =
      ZIO.fail(ResolveError.Repository(RepositoryIo.Fetch))

    def sources(organization: String, name: String, revision: String, repository: Repository): IO[ResolveError, ResolvedArtifact] =
      ZIO.fail(ResolveError.Repository(RepositoryIo.Fetch))

  private final class Holding(jar: Path, sha: Sha256) extends Resolve:
    def resolve(cell: ResolveCell): IO[ResolveError, Classpath] =
      ZIO.fail(ResolveError.Repository(RepositoryIo.Fetch))

    def fetch(artifact: LockedArtifact): IO[ResolveError, ResolvedArtifact] =
      if artifact.sha256 == sha then
        ZIO.succeed(ResolvedArtifact(artifact.organization, artifact.name, artifact.revision, jar, sha, artifact.repository))
      else ZIO.fail(ResolveError.Checksum(jar, artifact.sha256, sha))

    def resolvePlugin(dependency: Dep, repositories: Repositories): IO[ResolveError, Classpath] =
      ZIO.fail(ResolveError.Repository(RepositoryIo.Fetch))

    def sources(organization: String, name: String, revision: String, repository: Repository): IO[ResolveError, ResolvedArtifact] =
      ZIO.fail(ResolveError.Repository(RepositoryIo.Fetch))

  private final class Sourced(
      binary: Path,
      binarySha: Sha256,
      sources: Path,
      sourcesSha: Sha256,
      allowSources: Boolean,
  ) extends Resolve:
    def resolve(cell: ResolveCell): IO[ResolveError, Classpath] =
      ZIO.fail(ResolveError.Repository(RepositoryIo.Fetch))

    def fetch(artifact: LockedArtifact): IO[ResolveError, ResolvedArtifact] =
      if artifact.sha256 == binarySha then
        ZIO.succeed(ResolvedArtifact(artifact.organization, artifact.name, artifact.revision, binary, binarySha, artifact.repository))
      else ZIO.fail(ResolveError.Checksum(binary, artifact.sha256, binarySha))

    def resolvePlugin(dependency: Dep, repositories: Repositories): IO[ResolveError, Classpath] =
      ZIO.fail(ResolveError.Repository(RepositoryIo.Fetch))

    def sources(organization: String, name: String, revision: String, repository: Repository): IO[ResolveError, ResolvedArtifact] =
      if allowSources then ZIO.succeed(ResolvedArtifact(organization, name, revision, sources, sourcesSha, repository))
      else ZIO.fail(ResolveError.Repository(RepositoryIo.Fetch))

  private final case class Refused(name: Option[String], listFromParent: Boolean, buildFromParent: Boolean)

  private def probe(boundary: List[Path], vendor: Path): Refused =
    val platform = ClassLoader.getPlatformClassLoader
    val api = new java.net.URLClassLoader(boundary.map(_.toUri.toURL).toArray, platform)
    val parent = new ApiBoundary(api)
    val build = new BuildLoader(Array(vendor.toUri.toURL), parent)
    try
      val refused =
        try
          build.loadClass("histos.schedule.Sendable")
          None
        catch case _: ClassNotFoundException => build.takeRefused
      val list = build.loadClass("scala.collection.immutable.List")
      val buildTrait = build.loadClass("histos.HistosBuild")
      Refused(refused, list.getClassLoader != build, buildTrait.getClassLoader != build)
    finally
      build.close()
      api.close()

  private def loadWith(repo: Path, jar: Path, resolve: Resolve): ZIO[Loader & Scope & ContentStore, LoadError, Either[LoadError, LoadedBuild]] =
    ZIO.serviceWithZIO[Loader](_.load(repo, jar)).either.provideSome[Loader & Scope & ContentStore](ZLayer.succeed(resolve))

  private def apiHex(jar: Path): IO[LoadError, String] =
    ZIO.attempt(Sha256.of(Chunk.fromArray(Files.readAllBytes(jar))).value).mapError(_ => LoadError.Io(LoadIo.Stat))

  private def fileSha(path: Path): IO[LoadError, Sha256] =
    ZIO.attempt(Sha256.of(Chunk.fromArray(Files.readAllBytes(path)))).mapError(_ => LoadError.Io(LoadIo.Stat))

  private def writeLock(repo: Path, sha: Sha256): IO[LoadError, Unit] =
    writeCell(repo, "exact-binary", sha, None)

  private def writeCell(repo: Path, link: String, binary: Sha256, source: Option[Sha256]): IO[LoadError, Unit] =
    val sourceLine = source match
      case None      => ""
      case Some(sha) => s"source dev.example echo_3 1.0.0 ${sha.value} central\n"
    val text =
      s"plugin dev.example echo 1.0.0 $link\n" +
        "direct dev.example echo 1.0.0 binary\n" +
        s"artifact dev.example echo_3 1.0.0 ${binary.value} central\n" +
        sourceLine
    ZIO.attempt {
      Files.createDirectories(repo.resolve(".histos"))
      Files.writeString(Resolve.lockPath(repo), text)
      ()
    }.mapError(_ => LoadError.Io(LoadIo.Write))

  private def underPlugins(path: String): Boolean =
    Option(Path.of(path).getParent).flatMap(parent => Option(parent.getFileName)).exists(_.toString == "plugins")

  private def compilePlugin(api: Path, hex: String): ZIO[Scope, LoadError, Path] =
    for
      boundary <- Boundary.jars(api)
      dir <- ZIO.attempt(Files.createTempDirectory("histos-echo")).mapError(_ => LoadError.Io(LoadIo.Temp))
      _ <- ZIO.addFinalizer(deleteTree(dir))
      source = dir.resolve("Echo.scala")
      classes = dir.resolve("classes")
      _ <- ZIO.attempt {
             Files.writeString(source, echoSource)
             Files.createDirectories(classes)
           }.mapError(_ => LoadError.Io(LoadIo.Temp))
      _ <- compileSource(source, boundary, classes)
      packed <- textJar(dir.resolve("echo.jar"), List(
                   HistosPlugin.entryResource -> "dev.example.Echo$\n",
                   ApiDigest.resource -> s"$hex\n",
                 ), Some(classes))
    yield packed

  private def compileSource(source: Path, classpath: List[Path], out: Path): IO[LoadError, Unit] =
    ZIO.attempt {
      val reporter = new StoreReporter(null)
      new Driver().process(
        Array(
          "-classpath",
          classpath.map(_.toString).mkString(java.io.File.pathSeparator),
          "-d",
          out.toString,
          "-color:never",
          source.toString,
        ),
        reporter,
      )
      if reporter.hasErrors then Left(reporter.allErrors.map(_.message).mkString("\n"))
      else Right(())
    }.mapError(_ => LoadError.Io(LoadIo.Compile)).flatMap:
      case Left(text) => ZIO.fail(LoadError.CompileFailed(List(CompilerMessage.fromCompiler(text))))
      case Right(_)   => ZIO.unit

  private def textJar(path: Path, entries: List[(String, String)], classes: Option[Path] = None): IO[LoadError, Path] =
    ZIO.attempt {
      val output = new JarOutputStream(Files.newOutputStream(path))
      try
        classes.foreach(root => writeTree(output, root, root))
        entries.foreach: (name, text) =>
          output.putNextEntry(new JarEntry(name))
          output.write(text.getBytes(java.nio.charset.StandardCharsets.UTF_8))
          output.closeEntry()
      finally output.close()
      path
    }.mapError(_ => LoadError.Io(LoadIo.Temp))

  private def writeTree(output: JarOutputStream, root: Path, dir: Path): Unit =
    val walk = Files.walk(dir)
    try
      walk.iterator.asScala.foreach: path =>
        if Files.isRegularFile(path) then
          val name = root.relativize(path).iterator.asScala.map(_.toString).mkString("/")
          output.putNextEntry(new JarEntry(name))
          output.write(Files.readAllBytes(path))
          output.closeEntry()
    finally walk.close()

  private val pluginStore: ZLayer[Any, Nothing, ContentStore] =
    ZLayer.succeed(ContentStore.in(Files.createTempDirectory("histos-plugin-cache")))

  def spec = suite("load")(loads, header, plugins).provideShared(Loader.live ++ ZLayer.succeed(IdleResolve) ++ pluginStore) @@ TestAspect.sequential
