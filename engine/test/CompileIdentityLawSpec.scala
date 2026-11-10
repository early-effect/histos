package histos.zinc

import histos.cache.ContentStore
import histos.jdk.{JdkMajor, JdkRuntime, JdkVersion, Sha256, Sha256Digest}
import histos.resolve.{DependencyLock, LockedTools, Repositories, Resolve, ResolveError}
import histos.toolchain.RuntimeIdentity
import histos.workspace.{IoFailure, IoFault, Workspace}
import histos.schedule.Digest
import histos.{Module, Platform, ProjectName, ScalaVersion, ScalacFlag, ScalacFlags, SourcePos, ToolCatalog}

import java.nio.file.{Files, Path}
import java.time.Instant

import slayer.{any, stub, stubbed}

import zio.{Chunk, Scope, UIO, ZIO, ZLayer}
import zio.test.*

object CompileIdentityLawSpec extends ZIOSpecDefault:
  def spec = suite("compile identity laws")(
    suite("the key is the declared rows")(
      test("equal requests keep one key, and that key is the encoded rows"):
        val sources = Sha256.trusted("src")
        val key     = CompileKey.of(asked, sources)
        assertTrue(
          key == CompileKey.of(asked, sources),
          key == Sha256Digest.of(CompileInputs.encode(CompileInputs.declared(asked, sources))),
          key.value.length == 64,
        )
      ,
      test("a platform, a flag, a coverage text, or a scala version is a different key"):
        check(Gen.fromIterable(Platform.values)): platform =>
          val moved = asked.copy(toolchain = asked.toolchain.copy(platform = platform))
          assertTrue(
            (CompileKey.of(moved, sourceHash) == baseKey) == (platform == Platform.Jvm),
            CompileKey.of(flagged, sourceHash) != baseKey,
            CompileKey.of(asked.copy(coverage = Some("on")), sourceHash) != baseKey,
            CompileKey.of(asked.copy(coverage = Some("off")), sourceHash) !=
              CompileKey.of(asked.copy(coverage = Some("on")), sourceHash),
            CompileKey.of(versioned("2.13.18"), sourceHash) != baseKey,
          )
      ,
      test("reversing api digests or artifact checksums changes the key"):
        val apis      = List(Sha256.trusted("aa"), Sha256.trusted("bb"))
        val artifacts = List(Sha256.trusted("cc"), Sha256.trusted("dd"))
        val forward   = asked.copy(upstreamApis = apis, artifactHashes = artifacts)
        val backward  = asked.copy(upstreamApis = apis.reverse, artifactHashes = artifacts.reverse)
        assertTrue(
          CompileKey.of(forward, sourceHash) != CompileKey.of(backward, sourceHash),
          CompileKey.of(forward, sourceHash) != CompileKey.of(asked, sourceHash),
        )
      ,
      test("the jdk version and the runtime salt are different rows of the key"):
        val jdk = asked.copy(toolchain =
          asked.toolchain.copy(jdk = runtime(JdkVersion.trusted("17", JdkMajor.trusted(17)), Sha256.trusted("ab")))
        )
        val salt = asked.copy(toolchain = asked.toolchain.copy(jdk = runtime(jdk21, Sha256.trusted("cd"))))
        assertTrue(CompileKey.of(jdk, sourceHash) != baseKey, CompileKey.of(salt, sourceHash) != baseKey)
      ,
      test("a different source digest is a miss, and the same digest is not"):
        val other = Sha256.trusted("next")
        assertTrue(
          CompileKey.of(asked, other) != baseKey,
          CompileInputs.diff(CompileInputs.declared(asked, sourceHash), CompileInputs.declared(asked, other)) ==
            List(CompileInput.Sources),
        )
      ,
      test("compiler fingerprints are order-sensitive, and an empty list still changes the key"):
        val first  = Sha256.trusted("aa")
        val second = Sha256.trusted("bb")
        assertTrue(
          CompileKey.withTools(baseKey, List(first, second)) != CompileKey.withTools(baseKey, List(second, first)),
          CompileKey.withTools(baseKey, Nil) != baseKey,
          CompileInputs.withCompiler(Nil, List(first, second)) != CompileInputs.withCompiler(Nil, List(second, first)),
        )
      ,
      test("the input slot is the analysis path, not the compile key"):
        val left  = CompileInputs.slot(Path.of("/repo/a"))
        val right = CompileInputs.slot(Path.of("/repo/b"))
        assertTrue(left != right, left == CompileInputs.slot(Path.of("/repo/a")))
      ,
      test("a changed row is listed before an added row and a removed row"):
        val previous = List(
          CompileRow(CompileInput.Apis, "old"),
          CompileRow(CompileInput.Sources, "src"),
        )
        val current = List(
          CompileRow(CompileInput.Apis, "new"),
          CompileRow(CompileInput.Coverage, "on"),
        )
        assertTrue(
          CompileInputs.diff(previous, current) == List(
            CompileInput.Apis,
            CompileInput.Coverage,
            CompileInput.Sources,
          ),
          CompileInputs.diff(previous, previous) == Nil,
        )
      ,
      test("every compile input token parses, and an unknown token does not"):
        check(Gen.fromIterable(CompileInput.values)): input =>
          assertTrue(CompileInput.from(input.token).contains(input))
        && assertTrue(CompileInput.from("classes").isEmpty, CompileInput.from("").isEmpty),
    ),
    suite("a stored map is a miss until replay says the products are this key")(
      test("no map, garbage bytes, and the same rows are three misses, and none of them is a hit"):
        Workspace
          .work("histos-inputs")
          .flatMap: dir =>
            val store    = ContentStore.in(dir.resolve("cas"))
            val analysis = dir.resolve("analysis")
            val rows     = CompileInputs.declared(asked, sourceHash)
            val slot     = CompileInputs.slot(analysis)
            for
              absent <- CompileInputs.explain(store, analysis, rows)
              _      <- store.put(slot, Chunk.fromArray("nope".getBytes), Instant.EPOCH)
              junk   <- CompileInputs.explain(store, analysis, rows)
              _      <- CompileInputs.remember(store, analysis, rows)
              same   <- CompileInputs.explain(store, analysis, rows)
              edited <- CompileInputs.explain(store, analysis, CompileInputs.declared(flagged, sourceHash))
            yield assertTrue(
              absent == KeyMiss.NoPrior,
              junk == KeyMiss.NoPrior,
              same == KeyMiss.Changed(Nil),
              edited == KeyMiss.Changed(List(CompileInput.Flags)),
              KeyMiss.line(ProjectName.trusted("core"), scala3, Platform.Jvm, same) == "core 3.10.0 jvm",
              KeyMiss.line(ProjectName.trusted("core"), scala3, Platform.Jvm, absent) == "core 3.10.0 jvm no prior key",
            )
      ,
      test("replay is the marker, the analysis zip, and an occupied classes directory"):
        Workspace
          .work("histos-replay")
          .flatMap: dir =>
            val classes  = dir.resolve("classes")
            val analysis = dir.resolve("analysis")
            val request  = cell(classes, analysis, List(dir.resolve("A.scala")), Nil)
            val key      = CompileKey.of(request, sourceHash)
            val marker   = analysis.resolve("replay.key")
            val zip      = analysis.resolve("inc.zip")
            for
              missing <- CompileReplay.recall(request, key)
              _       <- Workspace.createDirectories(classes)
              _       <- Workspace.writeString(marker, key.value + "\n")
              _       <- Workspace.writeString(zip, "not-zinc")
              empty   <- CompileReplay.recall(request, key)
              _       <- Workspace.createDirectories(classes.resolve("nested"))
              hit     <- CompileReplay.recall(request, key)
              _       <- Workspace.writeString(marker, "  " + key.value + " \n")
              padded  <- CompileReplay.recall(request, key)
              _       <- Workspace.writeString(marker, CompileKey.of(flagged, sourceHash).value + "\n")
              wrong   <- CompileReplay.recall(request, key)
              _       <- Workspace.delete(zip)
              noZip   <- CompileReplay.recall(request, key)
            yield assertTrue(
              missing.isEmpty,
              empty.isEmpty,
              hit.contains(replayed(request, key)),
              padded.contains(replayed(request, key)),
              wrong.isEmpty,
              noZip.isEmpty,
            )
      ,
      test("a missing blob is a miss, and a blob that is not an analysis is corrupt"):
        Workspace
          .work("histos-load")
          .flatMap: dir =>
            val store   = ContentStore.in(dir.resolve("cas"))
            val request = cell(dir.resolve("classes"), dir.resolve("analysis"), Nil, Nil)
            val key     = CompileKey.of(request, sourceHash)
            Digest.from(key.value) match
              case None =>
                ZIO.succeed(assertTrue(key.value == "64 lowercase hex characters"))
              case Some(id) =>
                for
                  missing <- CompileReplay.load(store, key, request)
                  _       <- store.put(id, Chunk.fromArray("nope".getBytes), Instant.EPOCH)
                  corrupt <- CompileReplay.load(store, key, request).either
                  unsaved <- CompileReplay.save(store, key, request).either
                yield assertTrue(
                  missing.isEmpty,
                  corrupt == Left(CompileError.Stored(histos.schedule.CacheError.Corrupt(id))),
                  unsaved == Left(CompileError.Io(IoFailure(request.analysis.resolve("inc.zip"), IoFault.Read))),
                ),
    ),
    suite("upstream, sources, and product paths")(
      test("an exact cell wins, and a newer minor is not a fallback"):
        val upstream = mod("core", scala38, scala39, scala310)
        val old      = mod("core", scala21316, scala21317, scala21318)
        assertTrue(
          UpstreamCells.chosen(scala39, upstream, Platform.Jvm, List(upstream)).contains(scala39),
          UpstreamCells
            .chosen(scala39, upstream, Platform.Jvm, List(mod("core", scala38, scala310)))
            .contains(scala38),
          UpstreamCells.chosen(scala37, upstream, Platform.Jvm, List(upstream)).isEmpty,
          UpstreamCells.chosen(scala21317, old, Platform.Jvm, List(old)).contains(scala21317),
          UpstreamCells
            .chosen(
              scala21316,
              mod("core", scala21317, scala21318),
              Platform.Jvm,
              List(mod("core", scala21317, scala21318)),
            )
            .isEmpty,
          UpstreamCells.chosen(scala21318, old, Platform.Jvm, List(old)).contains(scala21318),
          UpstreamCells.chosen(scala212, old, Platform.Jvm, List(old)).isEmpty,
          UpstreamCells.chosen(scala310, old, Platform.Jvm, List(old)).isEmpty,
          UpstreamCells.chosen(scala310, upstream, Platform.Jvm, Nil).isEmpty,
        )
      ,
      test("semanticdb flags follow the series, and an unknown series adds none"):
        val repo = Path.of("/repo")
        assertTrue(
          SemanticDb.argv(repo, scala310) == List(
            "-Xsemanticdb",
            "-sourceroot",
            repo.toAbsolutePath.normalize.toString,
          ),
          SemanticDb.argv(repo, scala21318) == List(
            "-Yrangepos",
            s"-P:semanticdb:sourceroot:${repo.toAbsolutePath.normalize}",
          ),
          SemanticDb.argv(repo, scala212) == SemanticDb.argv(repo, scala21318),
          SemanticDb.argv(repo, ScalaVersion.trusted("1.2.3")) == Nil,
        )
      ,
      test("a standard library jar is the scala-library or scala3-library file name"):
        assertTrue(
          JvmCompile.isStandardLibrary(Path.of("scala-library-2.13.18.jar")),
          JvmCompile.isStandardLibrary(Path.of("lib/scala3-library_3-3.10.0.jar")),
          !JvmCompile.isStandardLibrary(Path.of("scala-library.jar")),
          !JvmCompile.isStandardLibrary(Path.of("zio.jar")),
        )
      ,
      test("classes and test classes map to their analysis directories, and another path does not"):
        val repo    = Path.of("/repo")
        val name    = ProjectName.trusted("core")
        val classes = GraphCompile.classesDir(repo, name, scala310, Platform.Js)
        val tests   = repo.resolve(".histos/test-classes/core/3.10.0/jvm")
        val covered = repo.resolve(".histos/coverage/classes/core/3.10.0/jvm")
        assertTrue(
          classes.endsWith(Path.of(".histos/classes/core/3.10.0/js")),
          GraphCompile.analysisDirOf(classes).contains(repo.resolve(".histos/analysis/core/3.10.0/js")),
          GraphCompile.analysisDirOf(tests).contains(repo.resolve(".histos/test-analysis/core/3.10.0/jvm")),
          GraphCompile.analysisDirOf(covered).contains(repo.resolve(".histos/coverage/analysis/core/3.10.0/jvm")),
          GraphCompile.analysisDirOf(repo.resolve("target/classes")).isEmpty,
        )
      ,
      test("the module src wins, and the repository src is only the single-module layout"):
        Workspace
          .work("histos-src")
          .flatMap: repo =>
            val one   = mod("core", scala310)
            val two   = mod("app", scala310)
            val lone  = repo.resolve("src")
            val named = repo.resolve("core/src")
            for
              absent <- GraphCompile.sourceBase(repo, one, List(one))
              _      <- Workspace.createDirectories(lone)
              shared <- GraphCompile.sourceBase(repo, one, List(one))
              hidden <- GraphCompile.sourceBase(repo, one, List(one, two))
              _      <- Workspace.createDirectories(named)
              own    <- GraphCompile.sourceBase(repo, one, List(one, two))
            yield assertTrue(absent.isEmpty, shared.contains(lone), hidden.isEmpty, own.contains(named))
      ,
      test("a platform directory name allows only that platform"):
        check(Gen.fromIterable(Platform.values)): platform =>
          val name = platform match
            case Platform.Jvm    => "jvm"
            case Platform.Js     => "scalajs"
            case Platform.Native => "scalanative"
          SourceFilter.parse(name) match
            case Some(filter) =>
              assertTrue(
                SourceFilter.allows(filter, scala310, platform),
                !SourceFilter.allows(filter, scala310, other(platform)),
              )
            case None => assertTrue(name == "parsed")
      ,
      test("source filters keep the series, and a package named test stays main code"):
        Workspace
          .work("histos-sources")
          .flatMap: root =>
            val main           = root.resolve("src/main/scala/demo/A.scala")
            val pack           = root.resolve("src/main/scala/demo/test/Hidden.scala")
            val packagedJs     = root.resolve("src/main/scala/js/S.scala")
            val packagedSeries = root.resolve("src/main/scala/scala-3/Only.scala")
            val jvm            = root.resolve("src/main/jvm/scala/J.scala")
            val js             = root.resolve("src/main/js/scala/S.scala")
            val java           = root.resolve("src/main/java/A.java")
            val only           = root.resolve("src/main/scala-3/Only.scala")
            val spec           = root.resolve("src/test/scala/demo/ATest.scala")
            for
              _     <- Workspace.writeString(main, "object A\n")
              _     <- Workspace.writeString(pack, "package demo.test\n")
              _     <- Workspace.writeString(packagedJs, "package js\n")
              _     <- Workspace.writeString(packagedSeries, "package `scala-3`\n")
              _     <- Workspace.writeString(jvm, "object J\n")
              _     <- Workspace.writeString(js, "object S\n")
              _     <- Workspace.writeString(java, "class A {}\n")
              _     <- Workspace.writeString(only, "object Only\n")
              _     <- Workspace.writeString(spec, "object ATest\n")
              found <- Sources.select(root, scala310, Platform.Jvm)
              tests <- Sources.tests(root, scala310, Platform.Jvm)
              third <- Sources.select(root, scala310, Platform.Js)
              old   <- Sources.select(root, scala21318, Platform.Jvm)
              gone  <- Sources.select(root.resolve("missing"), scala310, Platform.Jvm)
            yield assertTrue(
              found.contains(main),
              found.contains(pack),
              found.contains(packagedJs),
              found.contains(packagedSeries),
              found.contains(jvm),
              found.contains(java),
              found.contains(only),
              !found.contains(js),
              !found.contains(spec),
              old.contains(main),
              old.contains(packagedJs),
              old.contains(packagedSeries),
              !old.contains(only),
              !old.contains(js),
              tests == List(spec),
              third.contains(js),
              third.contains(only),
              third.contains(packagedJs),
              !third.contains(jvm),
              !third.contains(java),
              gone == Nil,
            )
      ,
      test("a missing analysis is initial, and an unreadable zip is unknown"):
        Workspace
          .work("histos-stale")
          .flatMap: repo =>
            val sources = List(repo.resolve("A.scala"))
            val dir     = SourceStale.directory(
              repo,
              ProjectName.trusted("core"),
              scala310,
              Platform.Js,
              AnalysisKind.Test,
            )
            for
              initial <- SourceStale.read(dir, sources)
              _       <- Workspace.writeString(dir.resolve("inc.zip"), "nope")
              unknown <- SourceStale.read(dir, sources)
              empty   <- SourceStale.read(repo.resolve("absent"), Nil)
            yield assertTrue(
              dir.endsWith(Path.of(".histos/test-analysis/core/3.10.0/js")),
              initial == FileStaleness.Initial(sources),
              unknown == FileStaleness.Unknown,
              empty == FileStaleness.Initial(Nil),
            ),
    ),
    (suite("the compiler is not asked when the request is already refused")(
      test("javascript and native fail before an empty source list or a banned jar"):
        check(Gen.fromIterable(List(Platform.Js, Platform.Native))): platform =>
          refused(cell(Path.of("classes"), Path.of("analysis"), Nil, Nil, "3.10.0", platform)).map: (error, ticks) =>
            assertTrue(error == Left(CompileError.UnsupportedPlatform(platform)), ticks == 0)
      ,
      test("scala 2.12 fails before the empty source check, and 2.13 with no sources stops there"):
        for
          old   <- refused(cell(Path.of("classes"), Path.of("analysis"), Nil, Nil, "2.12.20", Platform.Jvm))
          empty <- refused(cell(Path.of("classes"), Path.of("analysis"), Nil, Nil, "2.13.18", Platform.Jvm))
        yield assertTrue(
          old._1 == Left(CompileError.UnsupportedScala(ScalaVersion.trusted("2.12.20"))),
          old._2 == 0,
          empty._1 == Left(CompileError.NoSources),
          empty._2 == 0,
        )
      ,
      test("an empty source list is not a classpath ban"):
        Workspace
          .work("histos-ban-empty")
          .flatMap: dir =>
            jar(dir.resolve("api.jar"), ClassMarker.HistosBuild.entry).flatMap: banned =>
              refused(cell(dir.resolve("classes"), dir.resolve("analysis"), Nil, List(banned))).map: (error, ticks) =>
                assertTrue(error == Left(CompileError.NoSources), ticks == 0)
      ,
      test("a compiler class is banned, and permission does not waive that ban"):
        Workspace
          .work("histos-ban-compiler")
          .flatMap: dir =>
            val source = dir.resolve("A.scala")
            val dotty  = dir.resolve("dotty")
            val nsc    = dir.resolve("nsc")
            for
              _       <- Workspace.writeString(source, "object A\n")
              _       <- Workspace.writeString(ClassMarker.DottyDriver.under(dotty), "")
              _       <- Workspace.writeString(ClassMarker.NscGlobal.under(nsc), "")
              third   <- refused(cell(dir.resolve("c1"), dir.resolve("a1"), List(source), List(dotty)))
              allowed <- refused(
                cell(dir.resolve("c2"), dir.resolve("a2"), List(source), List(dotty), permitted = List(dotty))
              )
              second <- refused(cell(dir.resolve("c3"), dir.resolve("a3"), List(source), List(nsc)))
            yield assertTrue(
              third._1 == Left(CompileError.Banned(dotty, ClasspathBan.Scala3Compiler)),
              allowed._1 == Left(CompileError.Banned(dotty, ClasspathBan.Scala3Compiler)),
              second._1 == Left(CompileError.Banned(nsc, ClasspathBan.Scala2Compiler)),
              third._2 == 0 && allowed._2 == 0 && second._2 == 0,
            )
      ,
      test(
        "histos in a jar is banned unless that jar is the permitted api, and a classes tree is not"
      ):
        Workspace
          .work("histos-ban-api")
          .flatMap: dir =>
            val source = dir.resolve("A.scala")
            val tree   = dir.resolve("classes-in")
            val plugin = dir.resolve("plugin-dir")
            for
              _      <- Workspace.writeString(source, "object A\n")
              banned <- jar(dir.resolve("api.jar"), ClassMarker.HistosBuild.entry)
              _      <- Workspace.writeString(ClassMarker.HistosBuild.under(tree), "")
              _      <- Workspace.writeString(plugin.resolve("META-INF").resolve("histos").resolve("plugin"), "")
              hidden <- refused(cell(dir.resolve("c1"), dir.resolve("a1"), List(source), List(banned)))
              waived <- refused(
                cell(dir.resolve("c2"), dir.resolve("a2"), List(source), List(banned), permitted = List(banned))
              )
              passed <- refused(cell(dir.resolve("c3"), dir.resolve("a3"), List(source), List(tree)))
              marked <- refused(cell(dir.resolve("c4"), dir.resolve("a4"), List(source), List(plugin)))
            yield assertTrue(
              hidden._1 == Left(CompileError.Banned(banned, ClasspathBan.Histos)),
              kind(waived._1) == "no-tool",
              kind(passed._1) == "no-tool",
              kind(marked._1) == "no-tool",
              hidden._2 == 0 && waived._2 == 0 && passed._2 == 0 && marked._2 == 0,
            ),
    ) @@ TestAspect.sequential).provideSomeLayerShared[TestEnvironment & Scope](gate),
  )

  private val scala3     = ScalaVersion.trusted("3.10.0")
  private val scala37    = ScalaVersion.trusted("3.7.0")
  private val scala38    = ScalaVersion.trusted("3.8.0")
  private val scala39    = ScalaVersion.trusted("3.9.0")
  private val scala310   = scala3
  private val scala212   = ScalaVersion.trusted("2.12.20")
  private val scala21316 = ScalaVersion.trusted("2.13.16")
  private val scala21317 = ScalaVersion.trusted("2.13.17")
  private val scala21318 = ScalaVersion.trusted("2.13.18")
  private val jdk21      = JdkVersion.trusted("21", JdkMajor.trusted(21))
  private val sourceHash = Sha256.trusted("src")

  private val gate =
    (stubbed[Resolve] ++ stubbed[ContentStore] ++ stubbed[Compilers] ++ CompilerStarts.live) >+>
      ZLayer.fromFunction: (resolve: Resolve) =>
        LockedTools(DependencyLock(Nil), ToolCatalog.default, Repositories.central, resolve)

  private val asked: CompileRequest =
    cell(Path.of("classes"), Path.of("analysis"), Nil, Nil)

  private val baseKey = CompileKey.of(asked, sourceHash)

  private val flagged: CompileRequest =
    asked.copy(toolchain = asked.toolchain.copy(flags = ScalacFlags.of(ScalacFlag.Deprecation)))

  private def versioned(raw: String): CompileRequest =
    asked.copy(toolchain = asked.toolchain.copy(version = ScalaVersion.trusted(raw)))

  private def runtime(version: JdkVersion, salt: Sha256): JdkRuntime =
    JdkRuntime(Path.of("/jdk"), version, RuntimeIdentity.Local(Path.of("/jdk"), salt))

  private def cell(
      classes: Path,
      analysis: Path,
      sources: List[Path],
      classpath: List[Path],
      version: String = "3.10.0",
      platform: Platform = Platform.Jvm,
      permitted: List[Path] = Nil,
  ): CompileRequest =
    CompileRequest(
      CompileToolchain(
        ScalaVersion.trusted(version),
        platform,
        runtime(jdk21, Sha256.trusted("ab")),
        ScalacFlags.empty,
      ),
      sources,
      classpath,
      classes,
      analysis,
      Nil,
      Nil,
      permitted = permitted,
    )

  private def replayed(request: CompileRequest, key: Sha256): CompileOutput =
    CompileOutput(
      request.classes,
      0,
      key,
      changed = false,
      sources = Some(SourceCount(0, request.sources.size)),
      classesWritten = Some(0),
      miss = None,
    )

  private def mod(name: String, version: ScalaVersion, rest: ScalaVersion*): Module =
    Module(
      ProjectName.trusted(name),
      histos.Platforms(Platform.Jvm, Nil),
      histos.ScalaVersions(version, rest.toList),
      Nil,
      Nil,
      SourcePos("law.scala", 1, 1),
    )

  private def other(platform: Platform): Platform =
    if platform == Platform.Jvm then Platform.Js else Platform.Jvm

  private def kind(error: Either[CompileError, CompileOutput]): String =
    error match
      case Left(CompileError.Resolved(ResolveError.NoTool(_))) => "no-tool"
      case Left(CompileError.Banned(_, ban))                   => s"banned ${ban.label}"
      case Left(other)                                         => other.productPrefix
      case Right(_)                                            => "compiled"

  private def jar(path: Path, entry: String): ZIO[Any, IoFailure, Path] =
    ZIO
      .attemptBlocking {
        Option(path.getParent).foreach(parent => Files.createDirectories(parent))
        val out = new java.util.zip.ZipOutputStream(Files.newOutputStream(path))
        try
          out.putNextEntry(new java.util.zip.ZipEntry(entry))
          out.write(Array.emptyByteArray)
          out.closeEntry()
        finally out.close()
        path
      }
      .mapError(_ => IoFailure(path, IoFault.Write))

  private def refused(request: CompileRequest) =
    for
      _ <- stub[Resolve](_.fetch(any[histos.resolve.LockedArtifact]))(
        ZIO.die(new IllegalStateException("fetched"))
      )
      _ <- stub[ContentStore](_.read(any[histos.schedule.Digest]))(ZIO.die(new IllegalStateException("read")))
      _ <- stub[Compilers](
        _.compile(
          any[CompileRequest],
          any[CompilerArtifacts],
          any[List[String]],
          any[CompileProblem => Unit],
          any[Sha256],
          any[UIO[Unit]],
        )
      )(ZIO.die(new IllegalStateException("compiled")))
      before <- ZIO.serviceWithZIO[CompilerStarts](_.get)
      error  <- JvmCompile.compilePooled(request, Nil, _ => ()).either
      after  <- ZIO.serviceWithZIO[CompilerStarts](_.get)
    yield (error, after - before)
end CompileIdentityLawSpec
