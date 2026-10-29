package histos.zinc

import histos.cache.ContentStore
import histos.clean.Products
import histos.jdk.Sha256
import histos.native.NativeOrigin
import histos.resolve.{LockedTools, Resolve}
import histos.schedule.{CacheError, Digest}
import histos.toolchain.BuildCompiler
import histos.workspace.Workspace
import histos.{Jvm, Module, Platform, ProjectGraph, ProjectName, Projects, ScalaVersion, module}

import java.nio.charset.StandardCharsets
import java.time.Instant

import slayer.{Stubbed, any, stub, stubbed, stubbedTraced}

import zio.{Chunk, NonEmptyChunk, Ref, Scope, ZIO, ZLayer, durationInt}
import zio.test.*

/** `GraphCompile` is the service. The compiler is traced. The store is a stub when the law is who gets called, and live
  * when the law is the bytes that come back.
  */
object CompileLawSpec extends histos.SuiteClock:
  private val marker = "kept"
  private val moved  = Sha256.of(Chunk.fromArray("public".getBytes(StandardCharsets.UTF_8)))

  def spec =
    suite("compile laws")(
      comment,
      signature,
      hit,
      named,
      line,
      restored,
      corrupt,
      failed,
    ).provideSomeLayerShared[TestEnvironment & Scope](running) @@ TestAspect.withLiveSystem @@
      TestAspect.timeout(60.seconds)

  private val running =
    ZLayer.scoped:
      BuildCompiler.version.flatMap: version =>
        ZIO.service[LockedTools].provideSomeLayer[Scope](histos.ToolFixture.jvm(version))

  private def comment =
    test("a comment compiles the edited module and not a dependent"):
      BuildCompiler.version.flatMap: version =>
        val core  = module("core", Jvm)(version)
        val app   = module("app", Jvm)(version).dependsOn(core)
        val other = module("other", Jvm)(version)
        tree(version, Projects(core, app, other)).flatMap: (jdk, root, graph) =>
          remembered: _ =>
            for
              _      <- run(jdk, root, graph)
              before <- called
              _      <- Workspace.writeString(source(root, "core"), "object core\n// note\n")
              second <- run(jdk, root, graph)
              after  <- called
            yield assertTrue(
              after.drop(before.length) == List("core"),
              changed(second) == List(ProjectName("core") -> KeyMiss.Changed(List(CompileInput.Sources))),
            )

  private def signature =
    test("a public signature compiles the dependent and not an unrelated module"):
      BuildCompiler.version.flatMap: version =>
        val core  = module("core", Jvm)(version)
        val app   = module("app", Jvm)(version).dependsOn(core)
        val other = module("other", Jvm)(version)
        tree(version, Projects(core, app, other)).flatMap: (jdk, root, graph) =>
          remembered: current =>
            for
              _      <- run(jdk, root, graph)
              before <- called
              _      <- current.set(moved)
              _      <- Workspace.writeString(source(root, "core"), "object core\n// signed\n")
              second <- run(jdk, root, graph)
              after  <- called
            yield assertTrue(
              after.drop(before.length).toSet == Set("core", "app"),
              changed(second).exists((name, change) =>
                name == ProjectName("app") && change == KeyMiss.Changed(List(CompileInput.Apis))
              ),
            )

  private def hit =
    test("a hit does not call the compiler"):
      BuildCompiler.version.flatMap: version =>
        val core = module("core", Jvm)(version)
        tree(version, Projects(core)).flatMap: (jdk, root, graph) =>
          remembered: _ =>
            for
              _      <- run(jdk, root, graph)
              before <- called
              second <- run(jdk, root, graph)
              after  <- called
            yield assertTrue(after == before, changed(second).isEmpty)

  private def named =
    test("a miss names no prior key, then the input that changed"):
      BuildCompiler.version.flatMap: version =>
        val core = module("core", Jvm)(version)
        tree(version, Projects(core)).flatMap: (jdk, root, graph) =>
          remembered: _ =>
            for
              first  <- run(jdk, root, graph)
              _      <- Workspace.writeString(source(root, "core"), "object core\n// note\n")
              second <- run(jdk, root, graph)
            yield assertTrue(
              changed(first) == List(ProjectName("core") -> KeyMiss.NoPrior),
              changed(second) == List(ProjectName("core") -> KeyMiss.Changed(List(CompileInput.Sources))),
            )

  private def line =
    test("a miss line names no prior key, or the inputs that changed"):
      val name    = ProjectName("core")
      val version = ScalaVersion("3.10.0")
      val id      = s"${name.value} ${version.value} ${Platform.Jvm.token}"
      check(Gen.listOfBounded(0, 4)(inputs)): changed =>
        val text =
          if changed.isEmpty then id
          else s"$id ${changed.map(_.token).mkString(" ")}"
        assertTrue(
          KeyMiss.line(name, version, Platform.Jvm, KeyMiss.NoPrior) == s"$id no prior key",
          KeyMiss.line(name, version, Platform.Jvm, KeyMiss.Changed(Nil)) == id,
          KeyMiss.line(name, version, Platform.Jvm, KeyMiss.Changed(changed)) == text,
        )

  private def restored =
    test("clean leaves the store and the next compile restores the class file"):
      BuildCompiler.version.flatMap: version =>
        val core = module("core", Jvm)(version)
        tree(version, Projects(core)).flatMap: (jdk, root, graph) =>
          live(mark = true): store =>
            run(jdk, root, graph).flatMap: first =>
              done(first) match
                case Some(cell) =>
                  val kept = cell.classes.resolve("Kept.class")
                  cell.key.flatMap(key => Digest.from(key.value)) match
                    case Some(id) =>
                      for
                        before <- Workspace.readString(kept)
                        _      <- Products.wipe(root)
                        gone   <- Workspace.readIfPresent(kept)
                        blob   <- store.read(id)
                        mark   <- called
                        second <- run(jdk, root, graph)
                        back   <- Workspace.readString(kept)
                        after  <- called
                      yield assertTrue(
                        before == marker,
                        gone.isEmpty,
                        blob.nonEmpty,
                        after == mark,
                        back == marker,
                      )
                    case None =>
                      ZIO.succeed(assertTrue(cell.key.map(_.value) == Some(marker)))
                case None =>
                  ZIO.succeed(assertTrue(done(first).map(_.module.value) == Some("core")))

  private def corrupt =
    test("a corrupt stored compile is a stored failure and does not call the compiler"):
      BuildCompiler.version.flatMap: version =>
        val core = module("core", Jvm)(version)
        tree(version, Projects(core)).flatMap: (jdk, root, graph) =>
          live(mark = false): store =>
            run(jdk, root, graph).flatMap: first =>
              done(first).flatMap(cell => cell.key.flatMap(key => Digest.from(key.value))) match
                case Some(id) =>
                  val refused = GraphCompileError.Failed(
                    ProjectName("core"),
                    version,
                    Platform.Jvm,
                    CompileError.Stored(CacheError.Corrupt(id)),
                  )
                  for
                    _     <- store.put(id, Chunk.fromArray("nope".getBytes(StandardCharsets.UTF_8)), Instant.EPOCH)
                    mark  <- called
                    again <- run(jdk, root, graph)
                    after <- called
                  yield assertTrue(
                    after == mark,
                    again.collect { case CompileFeed.Failed(error) => error }.toList == List(refused),
                  )
                case None =>
                  ZIO.succeed(assertTrue(done(first).map(_.module.value) == Some("core")))

  private def failed =
    test("a compile error is the cell failure"):
      BuildCompiler.version.flatMap: version =>
        val core = module("core", Jvm)(version)
        tree(version, Projects(core)).flatMap: (jdk, root, graph) =>
          Ref
            .make(Map.empty[Digest, Chunk[Byte]])
            .flatMap: blobs =>
              (for
                _      <- installStore(blobs)
                _      <- stub[Compiler](_.compile(any[CompileCall]))(ZIO.fail(CompileError.NoSources))
                result <- GraphCompile.runCounted(root, graph, Nil, jdk, NativeOrigin.Central, None).either
                calls  <- called
              yield assertTrue(calls == List("core"), result == Left(failure(version, CompileError.NoSources))))
                .provideSomeLayer[LockedTools](memoryLayer)

  private val resolveLayer =
    ZLayer.fromZIO(ZIO.serviceWith[LockedTools](_.resolve))

  private val memoryLayer =
    stubbedTraced[Compiler] ++ stubbed[ContentStore] ++ resolveLayer

  private def remembered[A](
      use: Ref[Sha256] => ZIO[
        LockedTools & Stubbed[Compiler] & Stubbed[ContentStore] & Resolve & ContentStore & Compiler,
        Any,
        A,
      ]
  ) =
    for
      signature <- Ref.make(ApiDigest.empty)
      blobs     <- Ref.make(Map.empty[Digest, Chunk[Byte]])
      out       <-
        (for
          _   <- installStore(blobs)
          _   <- install(signature, mark = false)
          got <- use(signature)
        yield got).provideSomeLayer[LockedTools](memoryLayer)
    yield out

  private def live[A](
      mark: Boolean
  )(
      use: ContentStore => ZIO[
        LockedTools & Stubbed[Compiler] & Resolve & ContentStore & Compiler,
        Any,
        A,
      ]
  ) =
    for
      cas <- Workspace.work("histos-compile-law-cas")
      store = ContentStore.in(cas)
      signature <- Ref.make(ApiDigest.empty)
      out       <-
        (for
          _   <- install(signature, mark)
          got <- use(store)
        yield got).provideSomeLayer[LockedTools](
          stubbedTraced[Compiler] ++ ZLayer.succeed(store) ++ resolveLayer
        )
    yield out

  private def install(signature: Ref[Sha256], mark: Boolean) =
    stub[Compiler](_.compile(any[CompileCall]))((call: CompileCall) => retain(signature, mark)(call))

  private def installStore(blobs: Ref[Map[Digest, Chunk[Byte]]]) =
    stub[ContentStore](_.read(any[Digest]))((digest: Digest) => blobs.get.map(_.get(digest))) *>
      stub[ContentStore](_.put(any[Digest], any[Chunk[Byte]], any[Instant]))(
        (digest: Digest, bytes: Chunk[Byte], _: Instant) => blobs.update(_.updated(digest, bytes))
      )

  private def retain(signature: Ref[Sha256], mark: Boolean)(call: CompileCall) =
    for
      store  <- ZIO.service[ContentStore]
      digest <- signature.get
      _      <- CompileReplay.seed(store, call)
      _      <- ZIO.when(mark)(
        Workspace.writeString(call.request.classes.resolve("Kept.class"), marker).mapError(CompileError.Io(_))
      )
      _ <- Workspace
        .writeString(call.request.analysis.resolve(ApiDigest.fileName), digest.value + "\n")
        .mapError(CompileError.Io(_))
      _ <- CompileReplay.save(store, call.key, call.request)
      _ <- CompileInputs.remember(store, call.request.analysis, call.rows)
    yield CompileOutput(
      call.request.classes,
      0,
      call.key,
      changed = true,
      sources = Some(SourceCount(0, call.request.sources.size)),
      classesWritten = Some(0),
      miss = None,
    )

  private def tree(version: ScalaVersion, built: Either[histos.BuildError, ProjectGraph]) =
    for
      jdk   <- GraphCompile.jdkHere
      root  <- Workspace.work("histos-compile-law")
      graph <- ZIO.fromEither(built)
      _     <- ZIO.foreachDiscard(modulesOf(graph)): item =>
        Workspace.writeString(source(root, item.name.value), s"object ${item.name.value}\n")
      _ <- Workspace.writeString(Resolve.lockPath(root), lockText(graph))
      _ <- histos.ToolFixture.seed(root, graph)
    yield (jdk, root, graph)

  private def run(jdk: histos.jdk.JdkRuntime, root: java.nio.file.Path, graph: ProjectGraph) =
    GraphCompile.consoleCounted(root, graph, Nil, jdk, NativeOrigin.Central, None).runCollect

  private def called: ZIO[Stubbed[Compiler], Nothing, List[String]] =
    ZIO.serviceWith[Stubbed[Compiler]](_.calls.flatMap(moduleOf).toList)

  private def moduleOf(call: slayer.Call): Option[String] =
    call.args match
      case (found: CompileCall) :: _ =>
        Option(found.request.analysis.getParent)
          .flatMap(version => Option(version.getParent))
          .flatMap(module => Option(module.getFileName).map(_.toString))
      case _ => None

  private def changed(feeds: Chunk[CompileFeed]): List[(ProjectName, KeyMiss)] =
    feeds.collect { case CompileFeed.Miss(name, _, _, change) => name -> change }.toList

  private def done(feeds: Chunk[CompileFeed]): Option[CompiledCell] =
    feeds.collect { case CompileFeed.Done(cell) => cell }.toList match
      case cell :: Nil => Some(cell)
      case _           => None

  private def failure(version: ScalaVersion, error: CompileError): GraphCompileError =
    GraphCompileError.Cells(
      NonEmptyChunk(GraphCompileError.Failed(ProjectName("core"), version, Platform.Jvm, error))
    )

  private def source(root: java.nio.file.Path, name: String): java.nio.file.Path =
    Sources.mainFile(root.resolve(name), s"$name.scala")

  private def modulesOf(graph: ProjectGraph): List[Module] =
    graph.projects.collect { case item: Module => item }

  private def lockText(graph: ProjectGraph): String =
    val cells = modulesOf(graph).flatMap: item =>
      item.versions.all.flatMap: version =>
        item.platforms.all.map: platform =>
          histos.resolve.LockedCell(item.name, version, platform, Nil, Nil, Nil)
    histos.resolve.DependencyLock.render(histos.resolve.DependencyLock(cells))

  private val inputs =
    Gen.elements(
      CompileInput.Scala,
      CompileInput.Platform,
      CompileInput.Jdk,
      CompileInput.Runtime,
      CompileInput.Flags,
      CompileInput.Sources,
      CompileInput.Artifacts,
      CompileInput.Apis,
      CompileInput.Tools,
      CompileInput.Compiler,
    )
end CompileLawSpec
