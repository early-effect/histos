package histos.resolve

import histos.*
import histos.jdk.Sha256
import histos.plugin.LinkStrategy
import histos.workspace.{IoFailure, IoFault, Workspace}
import java.nio.file.{Files, Path}
import zio.{Chunk, IO, Ref, ZIO, ZLayer}
import zio.test.*

object ToolCellsSpec extends ZIOSpecDefault:
  private val version = ScalaVersion("3.10.0")
  private val request =
    histos.zinc.CompilerRequests.compiler(version, Repositories.central)
  private val digest = Sha256.of(Chunk.fromArray(Array[Byte](1, 2, 3)))
  private val artifact = LockedArtifact(
    "org.scala-lang",
    "scala3-compiler_3",
    version.value,
    digest,
    Repository.Central
  )
  private val locked = LockedTool(
    request.id,
    request.roots,
    List(Repository.Central),
    request.roots.map(root =>
      artifact.copy(
        organization = root.organization,
        name = root.name,
        revision = root.revision
      )
    ),
    Nil
  )

  private final class Stub(
      solve: ToolRequest => IO[ResolveError, Classpath],
      get: LockedArtifact => IO[ResolveError, ResolvedArtifact]
  ) extends Resolve:
    override def resolveTool(
        request: ToolRequest
    ): IO[ResolveError, Classpath] = solve(request)
    def resolve(cell: ResolveCell): IO[ResolveError, Classpath] =
      ZIO.succeed(Classpath(Nil, Nil))
    def fetch(artifact: LockedArtifact): IO[ResolveError, ResolvedArtifact] =
      get(artifact)
    def resolvePlugin(
        dependency: Dep,
        repositories: Repositories
    ): IO[ResolveError, Classpath] = ZIO.succeed(Classpath(Nil, Nil))
    def sources(
        organization: String,
        name: String,
        revision: String,
        repository: Repository
    ): IO[ResolveError, ResolvedArtifact] =
      ZIO.fail(ResolveError.NotFound(organization, name, revision))

  private def refused[A]: IO[ResolveError, A] =
    ZIO.fail(ResolveError.NoLock(Path.of("unexpected-solve")))

  private def publish(
      repo: Path,
      name: String,
      dependency: Option[(String, Option[JarClassifier])],
      classifier: Option[JarClassifier] = None
  ): ZIO[Any, IoFailure, Unit] =
    val directory =
      repo.resolve("dev").resolve("fixture").resolve(name).resolve("1.0.0")
    val stem = s"$name-1.0.0${classifier.fold("")(value => s"-${value.value}")}"
    val jar = directory.resolve(s"$stem.jar")
    val comment = classifier.fold(Array.emptyByteArray)(
      _.value.getBytes(java.nio.charset.StandardCharsets.UTF_8)
    )
    val bytes =
      Array[Byte](80, 75, 5, 6) ++ Array.fill[Byte](16)(0) ++ Array[Byte](
        comment.length.toByte,
        0
      ) ++ comment
    val dependencies = dependency.fold(""): (child, selected) =>
      val qualifier =
        selected.fold("")(value => s"<classifier>${value.value}</classifier>")
      s"<dependencies><dependency><groupId>dev.fixture</groupId><artifactId>$child</artifactId><version>1.0.0</version>$qualifier</dependency></dependencies>"
    val pom =
      s"<project><modelVersion>4.0.0</modelVersion><groupId>dev.fixture</groupId><artifactId>$name</artifactId><version>1.0.0</version>$dependencies</project>"
    for
      _ <- ZIO
        .attemptBlocking {
          Files.createDirectories(directory); Files.write(jar, bytes); ()
        }
        .mapError(_ => IoFailure(jar, IoFault.Write))
      _ <- Workspace.writeString(directory.resolve(s"$name-1.0.0.pom"), pom)
      sha <- ZIO
        .attemptBlocking(
          java.security.MessageDigest
            .getInstance("SHA-1")
            .digest(bytes)
            .map(byte => f"${byte & 0xff}%02x")
            .mkString
        )
        .mapError(_ => IoFailure(jar, IoFault.Read))
      _ <- Workspace.writeString(
        directory.resolve(s"$stem.jar.sha1"),
        sha
      )
    yield ()

  def spec = suite("tool cells")(
    test("empty graphs propagate compiler checksums through verify and check"):
      ZIO.scoped:
        for
          root <- Workspace.work("histos-empty-checksum")
          error = ResolveError.Checksum(
            root.resolve("compiler.jar"),
            digest,
            Sha256.of(Chunk.empty)
          )
          _ <- Workspace.writeString(
            Resolve.lockPath(root),
            DependencyLock.render(DependencyLock(Nil, Nil, List(locked)))
          )
          resolver = Stub(_ => refused, _ => ZIO.fail(error))
          verify <- Resolve
            .verify(root, ProjectGraph.trusted(Nil), Repositories.central)
            .provide(ZLayer.succeed(resolver))
            .either
          check <- Resolve
            .check(root, ProjectGraph.trusted(Nil), Repositories.central)
            .provide(ZLayer.succeed(resolver))
            .either
        yield assertTrue(verify == Left(error), check == Left(error))
    ,
    test("a compiler tool cell belongs to the dependency lock"):
      val text = "tool compiler 3.10.0 jvm\n"
      assertTrue(
        DependencyLock.parse(text) == Right(
          DependencyLock(
            Nil,
            Nil,
            List(LockedTool(request.id, Nil, Nil, Nil, Nil))
          )
        )
      )
    ,
    test(
      "tool roots, transitivity, origins, artifacts and evictions round-trip in classpath order"
    ):
      val origin =
        Repository.Maven(RepoUrl.file(Path.of("/tmp/native-repository")))
      val tool = locked.copy(
        roots = request.roots.map(
          _.copy(transitivity = Transitivity.Single, origin = Some(origin))
        ),
        evictions = List(Eviction("org.example", "support", "1.0.0", "2.0.0"))
      )
      val lock = DependencyLock(Nil, Nil, List(tool))
      assertTrue(
        DependencyLock.parse(DependencyLock.render(lock)) == Right(lock)
      )
    ,
    test("duplicate and malformed tool identities are typed lock errors"):
      val header = s"tool ${request.id.token}\n"
      assertTrue(
        DependencyLock.parse(header + header) == Left(
          LockError.DuplicateTool(request.id)
        ),
        DependencyLock.parse("tool unknown 3.10.0 jvm") == Left(
          LockError.BadLine("tool unknown 3.10.0 jvm")
        ),
        DependencyLock.parse(
          header + "root org lib 1.0.0 unknown build"
        ) == Left(LockError.BadLine("root org lib 1.0.0 unknown build"))
      )
    ,
    test("different Js revisions and environments coexist"):
      val catalog = ToolCatalog.default
      val one = histos.js.JsRequests.suite(
        JsVersion("1.22.0"),
        JsEnv.Node,
        catalog,
        Repositories.central
      )
      val two = histos.js.JsRequests.suite(
        JsVersion("1.22.0"),
        JsEnv.Jsdom,
        catalog,
        Repositories.central
      )
      val three = histos.js.JsRequests.suite(
        JsVersion("1.21.0"),
        JsEnv.Node,
        catalog,
        Repositories.central
      )
      val tools = List(one, two, three).map(req =>
        LockedTool(req.id, req.roots, List(Repository.Central), Nil, Nil)
      )
      val lock = DependencyLock(Nil, Nil, tools)
      assertTrue(
        tools.map(_.id).distinct.size == 3,
        DependencyLock.parse(DependencyLock.render(lock)) == Right(lock)
      )
    ,
    test("missing and drifted tools fail before a fetch or solve"):
      for
        fetched <- Ref.make(0)
        resolver = Stub(_ => refused, _ => fetched.update(_ + 1) *> refused)
        tools = LockedTools(
          DependencyLock(Nil, Nil, List(locked)),
          ToolCatalog.default,
          Repositories.central,
          resolver
        )
        missing <- tools
          .copy(lock = DependencyLock(Nil))
          .materialize(request)
          .either
        changed <- tools
          .materialize(
            request.copy(roots = request.roots.map(_.copy(revision = "9.9.9")))
          )
          .either
        used <- fetched.get
      yield assertTrue(
        missing == Left(ResolveError.NoTool(request.id)),
        changed == Left(ResolveError.ToolDrift(request.id)),
        used == 0
      )
    ,
    test("warm materialization fetches only recorded jars and never solves"):
      for
        fetched <- Ref.make(List.empty[LockedArtifact])
        path = Path.of("warm/compiler.jar")
        resolver = Stub(
          _ => refused,
          held =>
            fetched
              .update(_ :+ held)
              .as(
                ResolvedArtifact(
                  held.organization,
                  held.name,
                  held.revision,
                  path,
                  held.sha256,
                  held.repository
                )
              )
        )
        tools = LockedTools(
          DependencyLock(Nil, Nil, List(locked)),
          ToolCatalog.default,
          Repositories.central,
          resolver
        )
        result <- tools.materialize(request)
        used <- fetched.get
      yield assertTrue(
        used == locked.artifacts,
        result.artifacts.map(_.path) == List.fill(locked.artifacts.size)(path)
      )
    ,
    test("a checksum error remains a checksum error"):
      val path = Path.of("warm/compiler.jar")
      val actual = Sha256.of(Chunk.empty)
      val error = ResolveError.Checksum(path, digest, actual)
      val resolver = Stub(_ => refused, _ => ZIO.fail(error))
      LockedTools(
        DependencyLock(Nil, Nil, List(locked)),
        ToolCatalog.default,
        Repositories.central,
        resolver
      ).materialize(request)
        .either
        .map(result => assertTrue(result == Left(error)))
    ,
    test(
      "the real adapter respects single/closure roots and verifies tool checksums"
    ):
      ZIO.scoped:
        for
          root <- Workspace.work("histos-tool-repository")
          repo = root.resolve("maven")
          _ <- publish(repo, "support", None)
          _ <- publish(repo, "tool", Some("support" -> None))
          repositories = Repositories(Repository.Maven(RepoUrl.file(repo)), Nil)
          requested = ToolRequest(
            request.id,
            List(
              ToolRoot("dev.fixture", "tool", "1.0.0", Transitivity.Closure)
            ),
            repositories
          )
          resolver <- ZIO
            .service[Resolve]
            .provide(Resolve.live(root.resolve("cache")))
          closure <- resolver.resolveTool(requested)
          single <- resolver.resolveTool(
            requested.copy(roots =
              requested.roots.map(_.copy(transitivity = Transitivity.Single))
            )
          )
          first <- ZIO
            .fromOption(closure.artifacts.headOption)
            .mapError(_ =>
              ResolveError.NotFound("dev.fixture", "tool", "1.0.0")
            )
          artifacts = closure.artifacts.map(found =>
            LockedArtifact(
              found.organization,
              found.name,
              found.revision,
              found.sha256,
              found.repository
            )
          )
          held = LockedTool(
            requested.id,
            requested.roots,
            repositories.all,
            artifacts,
            closure.evictions
          )
          strict = Stub(_ => refused, resolver.fetch)
          view = LockedTools(
            DependencyLock(Nil, Nil, List(held)),
            ToolCatalog.default,
            repositories,
            strict
          )
          warm <- view.materialize(requested)
          tampered = held.copy(artifacts =
            artifacts.map(_.copy(sha256 = digest))
          )
          rejected <- view
            .copy(lock = DependencyLock(Nil, Nil, List(tampered)))
            .materialize(requested)
            .either
        yield assertTrue(
          closure.artifacts.map(_.name).toSet == Set("tool", "support"),
          single.artifacts.map(_.name) == List("tool"),
          warm == closure,
          closure.artifacts.forall(_.repository == repositories.first),
          rejected == Left(
            ResolveError.Checksum(first.path, digest, first.sha256)
          )
        )
    ,
    test(
      "classified transitives retain their complete identity during locked fetch"
    ):
      ZIO.scoped:
        for
          root <- Workspace.work("histos-tool-classifier")
          repo = root.resolve("maven")
          qualifier = JarClassifier("jdk8")
          _ <- publish(repo, "support", None)
          _ <- publish(repo, "support", None, Some(qualifier))
          _ <- publish(repo, "tool", Some("support" -> Some(qualifier)))
          repositories = Repositories(Repository.Maven(RepoUrl.file(repo)), Nil)
          requested = ToolRequest(
            request.id,
            List(
              ToolRoot("dev.fixture", "tool", "1.0.0", Transitivity.Closure)
            ),
            repositories
          )
          resolver <- ZIO
            .service[Resolve]
            .provide(Resolve.live(root.resolve("cache")))
          solved <- resolver.resolveTool(requested)
          classified <- ZIO
            .fromOption(solved.artifacts.find(_.classifier.contains(qualifier)))
            .mapError(_ =>
              ResolveError.NotFound("dev.fixture", "support", "1.0.0")
            )
          held = LockedArtifact(
            classified.organization,
            classified.name,
            classified.revision,
            classified.sha256,
            classified.repository,
            classified.classifier
          )
          fetched <- resolver.fetch(held)
          cell = LockedTool(
            requested.id,
            requested.roots,
            repositories.all,
            List(held),
            Nil
          )
          lock = DependencyLock(Nil, Nil, List(cell))
        yield assertTrue(
          fetched.classifier == Some(qualifier),
          fetched.sha256 == classified.sha256,
          DependencyLock.parse(DependencyLock.render(lock)) == Right(lock)
        )
    ,
    test("a Native origin is not silently replaced by Central"):
      ZIO.scoped:
        for
          root <- Workspace.work("histos-tool-origin")
          origin = Repository.Maven(RepoUrl.file(root))
          requested = ToolRequest(
            request.id,
            List(
              ToolRoot(
                "org.scala-lang",
                "scala-library",
                version.value,
                Transitivity.Single,
                Some(origin)
              )
            ),
            Repositories.central
          )
          result <- ZIO
            .serviceWithZIO[Resolve](_.resolveTool(requested))
            .provide(Resolve.live(root.resolve("cache")))
            .either
        yield assertTrue(
          result == Left(
            ResolveError.NotFound(
              "org.scala-lang",
              "scala-library",
              version.value
            )
          )
        )
    ,
    test("an update failure preserves the complete previous lock"):
      ZIO.scoped:
        for
          root <- Workspace.work("histos-tool-update")
          path = Resolve.lockPath(root)
          old = DependencyLock(
            Nil,
            List(
              LockedPlugin(
                "dev.example",
                "plugin",
                "1.0.0",
                LinkStrategy.ExactBinary,
                Nil,
                Nil,
                None
              )
            ),
            List(locked)
          )
          _ <- Workspace.writeString(path, DependencyLock.render(old))
          resolver = Stub(
            _ => ZIO.fail(ResolveError.ToolDrift(request.id)),
            _ => refused
          )
          result <- Resolve
            .update(root, ProjectGraph.trusted(Nil), Repositories.central)
            .provide(ZLayer.succeed(resolver))
            .either
          after <- Resolve.readLock(root)
        yield assertTrue(
          result == Left(ResolveError.ToolDrift(request.id)),
          after == old
        )
    ,
    test(
      "one successful update replaces staged plugins and tools in the same generation"
    ):
      ZIO.scoped:
        for
          root <- Workspace.work("histos-tool-generation")
          path = Resolve.lockPath(root)
          prior = DependencyLock(
            Nil,
            List(
              LockedPlugin(
                "dev.example",
                "old",
                "1.0.0",
                LinkStrategy.ExactBinary,
                Nil,
                Nil,
                None
              )
            ),
            List(locked)
          )
          staged = List(
            LockedPlugin(
              "dev.example",
              "new",
              "2.0.0",
              LinkStrategy.ExactBinary,
              Nil,
              Nil,
              None
            )
          )
          _ <- Workspace.writeString(path, DependencyLock.render(prior))
          resolver = Stub(
            req =>
              ZIO.succeed(
                Classpath(
                  req.roots.map(root =>
                    ResolvedArtifact(
                      root.organization,
                      root.name,
                      root.revision,
                      Path.of("fixture.jar"),
                      digest,
                      req.repositories.first
                    )
                  ),
                  Nil
                )
              ),
            _ => refused
          )
          _ <- Resolve
            .update(
              root,
              ProjectGraph.trusted(Nil),
              Repositories.central,
              plugins = PluginUpdate.Replace(staged)
            )
            .provide(ZLayer.succeed(resolver))
          after <- Resolve.readLock(root)
        yield assertTrue(
          after.plugins == staged,
          after.tools.map(_.id) == List(request.id),
          after.tools.flatMap(_.roots) == request.roots
        )
    ,
    test("a plugin update keeps tool cells"):
      ZIO.scoped:
        for
          root <- Workspace.work("histos-tool-plugin")
          path = Resolve.lockPath(root)
          _ <- Workspace.writeString(
            path,
            DependencyLock.render(DependencyLock(Nil, Nil, List(locked)))
          )
          resolver = Stub(_ => refused, _ => refused)
          _ <- Resolve
            .updatePlugins(
              root,
              List(
                PluginRequest.Dep(
                  Dep("dev.example::plugin:1.0.0"),
                  LinkStrategy.ExactBinary
                )
              ),
              Repositories.central
            )
            .provide(ZLayer.succeed(resolver))
          after <- Resolve.readLock(root)
        yield assertTrue(
          after.tools == List(locked),
          after.plugins.map(_.name) == List("plugin")
        )
  )
