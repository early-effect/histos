package histos.testrun

import histos.jdk.{Sha256, Sha256Error}
import histos.native.{ClassShapes, NativeOrigin}
import histos.resolve.{DependencyLock, LockedArtifact, LockedCell, LockedTool, Repository, Repositories}
import histos.toolchain.RuntimeIdentity
import histos.watch.SourceWatch
import histos.workspace.{IoFailure, IoFault, Workspace}
import histos.zinc.{ApiDigest, CompiledCell, CompilerRequests, GraphCompile}
import histos.{
  Jvm,
  Module,
  Platforms,
  ProjectDep,
  ProjectName,
  ScalaVersion,
  ScalaVersions,
  ScalacFlag,
  ScalacFlags,
  SourcePos,
  TestStore,
  ToolCatalog,
}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.nio.file.attribute.PosixFilePermissions

import org.objectweb.asm.{ClassWriter, Opcodes}

import zio.{Chunk, Scope, ZIO}
import zio.stream.ZStream
import zio.test.*

object QuickSpec extends histos.SuiteClock:
  private val jdkToken = "test-jdk"
  private val plain    = SuiteStamp.Cover(Nil, Nil, jdkToken, Nil, Nil)

  private def jars(names: List[String]): SuiteStamp.Cover =
    plain.copy(jars = names)

  private val stamps = suite("suite stamp")(
    test(
      "a referenced class is in the digest and an unreferenced class is not"
    ):
      ZIO.scoped:
        for
          root <- temp
          _    <- ZIO.attempt {
            writeClass(root, "demo/Foo", List("demo/Lib"))
            writeClass(root, "demo/Lib", Nil)
            writeClass(root, "demo/Other", Nil)
          }
          first <- SuiteStamp.digest(
            "demo.Foo",
            List(root),
            jars(List("jar-a")),
          )
          _     <- ZIO.attempt(writeClass(root, "demo/Other", List("demo/Nowhere")))
          other <- SuiteStamp.digest(
            "demo.Foo",
            List(root),
            jars(List("jar-a")),
          )
          _   <- ZIO.attempt(writeClass(root, "demo/Lib", List("demo/Other")))
          lib <- SuiteStamp.digest("demo.Foo", List(root), jars(List("jar-a")))
          jar <- SuiteStamp.digest("demo.Foo", List(root), jars(List("jar-b")))
        yield assertTrue(
          first.isDefined && first == other && first != lib && lib != jar
        )
    ,
    test("one class index stamps a suite the same way"):
      ZIO.scoped:
        for
          root <- temp
          _    <- ZIO.attempt {
            writeClass(root, "demo/Foo", List("demo/Lib"))
            writeClass(root, "demo/Lib", Nil)
          }
          direct <- SuiteStamp.digest(
            "demo.Foo",
            List(root),
            jars(List("jar-a")),
          )
          tree    <- SuiteStamp.index(List(root))
          indexed <- ZIO.fromEither(
            SuiteStamp.digest("demo.Foo", tree, jars(List("jar-a")))
          )
        yield assertTrue(direct.isDefined && direct == indexed)
    ,
    test("a stored success matches only that digest"):
      ZIO.scoped:
        for
          root   <- temp
          module <- ZIO.fromEither(ProjectName.from("app"))
          _      <- QuickRecord.save(
            root,
            module,
            ScalaVersion("3.10.0"),
            Jvm,
            suiteName,
            digestA,
          )
          hit <- QuickRecord.hit(
            root,
            module,
            ScalaVersion("3.10.0"),
            Jvm,
            suiteName,
            digestA,
          )
          miss <- QuickRecord.hit(
            root,
            module,
            ScalaVersion("3.10.0"),
            Jvm,
            suiteName,
            digestB,
          )
          _ <- QuickRecord.clear(
            root,
            module,
            ScalaVersion("3.10.0"),
            Jvm,
            suiteName,
          )
          gone <- QuickRecord.hit(
            root,
            module,
            ScalaVersion("3.10.0"),
            Jvm,
            suiteName,
            digestA,
          )
        yield assertTrue(hit && !miss && !gone)
    ,
    test("a zio spec is chosen and a helper is not"):
      val local = List(
        ClassShapes.Shape(
          "demo.FooA$",
          module = true,
          parents = List("zio.test.ZIOSpecDefault"),
        ),
        ClassShapes.Shape(
          "demo.Helper",
          module = false,
          parents = List("java.lang.Object"),
        ),
      )
      val world = (local ++ List(
        ClassShapes.Shape(
          "zio.test.ZIOSpecDefault",
          module = false,
          parents = List("zio.test.ZIOSpecAbstract"),
        )
      )).map(shape => shape.name -> shape).toMap
      val marks =
        List(SuiteNames.Mark("zio.test.ZIOSpecAbstract", module = true))
      assertTrue(SuiteNames.choose(local, world, marks) == List("demo.FooA"))
    ,
    test("upstream jars, the jdk, flags, and an API digest change the digest"):
      ZIO.scoped:
        for
          root     <- temp
          _        <- ZIO.attempt(writeClass(root, "demo/Foo", Nil))
          same     <- SuiteStamp.digest("demo.Foo", List(root), plain)
          upstream <- SuiteStamp.digest(
            "demo.Foo",
            List(root),
            plain.copy(upstream = List("up-1")),
          )
          jdk <- SuiteStamp.digest(
            "demo.Foo",
            List(root),
            plain.copy(jdk = "other-jdk"),
          )
          flags <- SuiteStamp.digest(
            "demo.Foo",
            List(root),
            plain.copy(flags = List("-encoding", "UTF-8")),
          )
          api <- SuiteStamp.digest(
            "demo.Foo",
            List(root),
            plain.copy(apis = List(s"core ${"ab" * 32}")),
          )
        yield assertTrue(
          same.isDefined && upstream != same && jdk != same && flags != same && api != same
        )
    ,
    test("a class file that cannot be parsed is a stamp error"):
      ZIO.scoped:
        for
          root <- temp
          path = root.resolve("demo").resolve("Foo.class")
          _ <- ZIO.attempt {
            Files.createDirectories(path.getParent)
            Files.write(path, Array[Byte](1, 2, 3, 4))
          }
          scanned <- SuiteStamp.index(List(root)).either
        yield assertTrue(scanned == Left(StampError.Class(path)))
    ,
    test("an unreadable class directory is a stamp error"):
      ZIO.scoped:
        for
          root <- temp
          dir = root.resolve("classes")
          _       <- ZIO.attempt(Files.createDirectories(dir))
          _       <- unreadable(dir)
          scanned <- SuiteStamp.index(List(dir)).either
        yield assertTrue(
          scanned == Left(StampError.Io(IoFailure(dir, IoFault.Walk)))
        )
    ,
    test(
      "a failed save is the write error and a failed clear is the delete error"
    ):
      ZIO.scoped:
        for
          root   <- temp
          module <- named
          record = QuickRecord.file(root, module, scala3, Jvm, suiteName)
          parent = root
            .resolve(".histos")
            .resolve("test-quick")
            .resolve(module.value)
            .resolve(scala3.value)
            .resolve(Jvm.token)
            .resolve("demo")
          _ <- ZIO.attempt(Files.createDirectories(parent))
          _ <- ZIO.acquireRelease(
            ZIO.attempt(
              Files.setPosixFilePermissions(
                parent,
                PosixFilePermissions.fromString("r-x------"),
              )
            )
          )(_ =>
            ZIO
              .attempt(
                Files.setPosixFilePermissions(
                  parent,
                  PosixFilePermissions.fromString("rwx------"),
                )
              )
              .ignore
          )
          saved <- QuickRecord
            .save(root, module, scala3, Jvm, suiteName, digestA)
            .either
          kept = root.resolve("clear-me")
          path = QuickRecord.file(kept, module, scala3, Jvm, suiteName)
          _ <- ZIO.attempt {
            Files.createDirectories(path)
            Files.writeString(path.resolve("kept"), "x")
          }
          cleared <- QuickRecord
            .clear(kept, module, scala3, Jvm, suiteName)
            .either
        yield assertTrue(
          saved == Left(QuickError.Save(IoFailure(record, IoFault.Write))),
          cleared == Left(QuickError.Clear(IoFailure(path, IoFault.Delete))),
        )
    ,
    test("an empty suite name is not a suite"):
      assertTrue(SuiteName.from("") == Left(SuiteNameError.Empty))
    ,
    test("a quick record that is not a stored success is an error"):
      ZIO.scoped:
        for
          root   <- temp
          module <- named
          path = QuickRecord.file(root, module, scala3, Jvm, suiteName)
          _ <- ZIO.attempt {
            Option(path.getParent).foreach(parent => Files.createDirectories(parent))
            Files.writeString(path, "nope\n")
          }
          read <- QuickRecord
            .hit(root, module, scala3, Jvm, suiteName, digestA)
            .either
        yield assertTrue(read == Left(QuickError.Shape(path)))
    ,
    test("a quick record whose digest is not a sha256 is an error"):
      ZIO.scoped:
        for
          root   <- temp
          module <- named
          path = QuickRecord.file(root, module, scala3, Jvm, suiteName)
          _ <- ZIO.attempt {
            Option(path.getParent).foreach(parent => Files.createDirectories(parent))
            Files.writeString(path, "success nope\n")
          }
          read <- QuickRecord
            .hit(root, module, scala3, Jvm, suiteName, digestA)
            .either
        yield assertTrue(read == Left(QuickError.Hash(path, Sha256Error.Bad("nope")))),
  )

  private val runs = suite("testQuick run")(
    test("an absent quick record runs the suite"):
      ZIO.scoped:
        for
          root <- temp
          classes = root.resolve("classes")
          _        <- ZIO.attempt(writeClass(classes, "demo/Foo", Nil))
          module   <- named
          digest   <- requireDigest(root, classes)
          recorded <- QuickRecord.hit(
            root,
            module,
            scala3,
            Jvm,
            suiteName,
            digest,
          )
          feeds <- quick(root, module, classes, TestStatus.Passed, base)
        yield assertTrue(!recorded, feeds == ran(module, TestStatus.Passed))
    ,
    test("a digest mismatch runs the suite"):
      ZIO.scoped:
        for
          root <- temp
          classes = root.resolve("classes")
          _      <- ZIO.attempt(writeClass(classes, "demo/Foo", Nil))
          module <- named
          digest <- requireDigest(root, classes)
          _      <- QuickRecord.save(
            root,
            module,
            scala3,
            Jvm,
            suiteName,
            digestB,
          )
          recorded <- QuickRecord.hit(
            root,
            module,
            scala3,
            Jvm,
            suiteName,
            digest,
          )
          stored <- QuickRecord.hit(
            root,
            module,
            scala3,
            Jvm,
            suiteName,
            digestB,
          )
          feeds <- quick(root, module, classes, TestStatus.Passed, base)
        yield assertTrue(
          digest != digestB,
          !recorded,
          stored,
          feeds == ran(module, TestStatus.Passed),
        )
    ,
    test("an unreadable quick record fails the run"):
      ZIO.scoped:
        for
          root <- temp
          classes = root.resolve("classes")
          _      <- ZIO.attempt(writeClass(classes, "demo/Foo", Nil))
          module <- named
          digest <- requireDigest(root, classes)
          _      <- QuickRecord.save(
            root,
            module,
            scala3,
            Jvm,
            suiteName,
            digest,
          )
          readable <- QuickRecord.hit(
            root,
            module,
            scala3,
            Jvm,
            suiteName,
            digest,
          )
          path = QuickRecord.file(root, module, scala3, Jvm, suiteName)
          _      <- unreadable(path)
          hidden <- QuickRecord
            .hit(root, module, scala3, Jvm, suiteName, digest)
            .either
          failed <- quick(root, module, classes, TestStatus.Passed, base).either
          refused = QuickError.Read(IoFailure(path, IoFault.Read))
        yield assertTrue(
          readable,
          hidden == Left(refused),
          failed == Left(ModuleTestError.Record(refused)),
        )
    ,
    test("a suite with no digest runs"):
      ZIO.scoped:
        for
          root <- temp
          classes = root.resolve("classes")
          _      <- ZIO.attempt(Files.createDirectories(classes))
          module <- named
          _      <- QuickRecord.save(
            root,
            module,
            scala3,
            Jvm,
            suiteName,
            digestA,
          )
          missing <- SuiteStamp.digest(suiteName.value, List(classes), plain)
          feeds   <- quick(root, module, classes, TestStatus.Passed, base)
        yield assertTrue(
          missing.isEmpty,
          feeds == ran(module, TestStatus.Passed),
        )
    ,
    test("a record cleared after a failure runs the suite"):
      ZIO.scoped:
        for
          root <- temp
          classes = root.resolve("classes")
          _      <- ZIO.attempt(writeClass(classes, "demo/Foo", Nil))
          module <- named
          digest <- requireDigest(root, classes)
          _      <- QuickRecord.save(
            root,
            module,
            scala3,
            Jvm,
            suiteName,
            digest,
          )
          _ <- ZIO.attempt(
            writeClass(classes, "demo/Foo", List("demo/Other"))
          )
          changed <- requireDigest(root, classes)
          failed  <- quick(root, module, classes, TestStatus.Failed(1), base)
          cleared <- QuickRecord.hit(
            root,
            module,
            scala3,
            Jvm,
            suiteName,
            digest,
          )
          replaced <- QuickRecord.hit(
            root,
            module,
            scala3,
            Jvm,
            suiteName,
            changed,
          )
          _        <- ZIO.attempt(writeClass(classes, "demo/Foo", Nil))
          restored <- requireDigest(root, classes)
          again    <- quick(root, module, classes, TestStatus.Passed, base)
        yield assertTrue(
          digest != changed,
          restored == digest,
          failed == ran(module, TestStatus.Failed(1)),
          !cleared,
          !replaced,
          again == ran(module, TestStatus.Passed),
        )
    ,
    test("a quick hit is cached and the suite does not run"):
      ZIO.scoped:
        for
          root <- temp
          classes = root.resolve("classes")
          _      <- ZIO.attempt(writeClass(classes, "demo/Foo", Nil))
          module <- named
          digest <- requireDigest(root, classes)
          _      <- QuickRecord.save(
            root,
            module,
            scala3,
            Jvm,
            suiteName,
            digest,
          )
          feeds <- quick(root, module, classes, TestStatus.Passed, base)
        yield assertTrue(
          feeds == Chunk(
            TestFeed.Cached(module, scala3, Jvm, suiteName.value)
          )
        )
    ,
    test("a stored success misses when the upstream jar, jdk, or flags differ"):
      ZIO.scoped:
        for
          root <- temp
          classes = root.resolve("classes")
          _      <- ZIO.attempt(writeClass(classes, "demo/Foo", Nil))
          module <- named
          digest <- requireDigest(root, classes)
          _      <- QuickRecord.save(
            root,
            module,
            scala3,
            Jvm,
            suiteName,
            digest,
          )
          same     <- quick(root, module, classes, TestStatus.Passed, base)
          upstream <- quick(root, module, classes, TestStatus.Passed, upstreamAsk)
          _        <- QuickRecord.save(
            root,
            module,
            scala3,
            Jvm,
            suiteName,
            digest,
          )
          jdk <- quick(root, module, classes, TestStatus.Passed, jdkAsk)
          _   <- QuickRecord.save(
            root,
            module,
            scala3,
            Jvm,
            suiteName,
            digest,
          )
          flags <- quick(root, module, classes, TestStatus.Passed, flagsAsk)
        yield assertTrue(
          same == Chunk(TestFeed.Cached(module, scala3, Jvm, suiteName.value)),
          upstream == ran(module, TestStatus.Passed),
          jdk == ran(module, TestStatus.Passed),
          flags == ran(module, TestStatus.Passed),
        )
    ,
    test("a missing upstream API file adds no line and an unreadable file is a gap"):
      ZIO.scoped:
        for
          root <- temp
          classes = root.resolve("classes")
          _      <- ZIO.attempt(writeClass(classes, "demo/Foo", Nil))
          absent <- ZIO.fromEither(SuiteCover.stored(root, upstreamAsk))
          quiet  <- SuiteStamp.digest(suiteName.value, List(classes), absent)
          path   <- ZIO.fromEither(apiFile(root, ProjectName("core")))
          _      <- ZIO.attempt {
            Files.createDirectories(path.getParent)
            Files.writeString(path, "nope\n")
          }
          bad = SuiteCover.stored(root, upstreamAsk)
          sha = Sha256.of(Chunk.fromArray("core-api".getBytes(StandardCharsets.UTF_8)))
          _       <- ZIO.attempt(Files.writeString(path, sha.value + "\n"))
          present <- ZIO.fromEither(SuiteCover.stored(root, upstreamAsk))
          moved   <- SuiteStamp.digest(suiteName.value, List(classes), present)
        yield assertTrue(
          absent.apis.isEmpty,
          quiet.isDefined,
          bad == Left(CoverGap.NoApi(ProjectName("core"), scala3, Jvm)),
          present.apis == List(s"core ${sha.value}"),
          moved.isDefined && moved != quiet,
        ),
  )

  private val sources = suite("source watch")(
    test("a saved file changes the snapshot and an untouched tree does not"):
      ZIO.scoped:
        for
          root <- temp
          file = root.resolve("src").resolve("demo").resolve("Lib.scala")
          _ <- ZIO.attempt {
            Files.createDirectories(file.getParent)
            Files.writeString(file, "one")
          }
          before <- SourceWatch.take(
            List(root.resolve("src"), root.resolve("histos.scala"))
          )
          same <- SourceWatch.take(
            List(root.resolve("src"), root.resolve("histos.scala"))
          )
          _     <- ZIO.attempt(Files.writeString(file, "two"))
          after <- SourceWatch.take(
            List(root.resolve("src"), root.resolve("histos.scala"))
          )
        yield assertTrue(before == same && before != after)
  )

  private def writeClass(
      root: Path,
      internal: String,
      calls: List[String],
  ): Unit =
    val writer = new ClassWriter(ClassWriter.COMPUTE_MAXS)
    writer.visit(
      Opcodes.V1_8,
      Opcodes.ACC_PUBLIC | Opcodes.ACC_SUPER,
      internal,
      null,
      "java/lang/Object",
      null,
    )
    val method = writer.visitMethod(
      Opcodes.ACC_PUBLIC | Opcodes.ACC_STATIC,
      "run",
      "()V",
      null,
      null,
    )
    calls.foreach { owner =>
      method.visitMethodInsn(Opcodes.INVOKESTATIC, owner, "run", "()V", false)
    }
    method.visitInsn(Opcodes.RETURN)
    method.visitMaxs(0, 0)
    method.visitEnd()
    writer.visitEnd()
    val path = root.resolve(internal + ".class")
    Files.createDirectories(path.getParent)
    Files.write(path, writer.toByteArray)
    ()

  private def temp: ZIO[Scope, Throwable, Path] =
    Workspace
      .work("histos-quick")
      .mapError(error => new java.io.IOException(error.message))

  private val scala3    = ScalaVersion("3.10.0")
  private val suiteName = SuiteName("demo.Foo")
  private val digestA   =
    Sha256.of(Chunk.fromArray("a".getBytes(StandardCharsets.UTF_8)))
  private val digestB =
    Sha256.of(Chunk.fromArray("b".getBytes(StandardCharsets.UTF_8)))
  private val repos = Repositories.central
  private val jdkA  = RuntimeIdentity.Local(
    Path.of("/jdk-a"),
    Sha256.of(Chunk.fromArray("jdk-a".getBytes(StandardCharsets.UTF_8))),
  )
  private val jdkB = RuntimeIdentity.Local(
    Path.of("/jdk-b"),
    Sha256.of(Chunk.fromArray("jdk-b".getBytes(StandardCharsets.UTF_8))),
  )
  private val compilerSha =
    Sha256.of(Chunk.fromArray("compiler-tool".getBytes(StandardCharsets.UTF_8)))

  private def named: ZIO[Any, histos.ProjectNameError, ProjectName] =
    ZIO.fromEither(ProjectName.from("app"))

  private def appModule(dependsOn: List[ProjectDep], flags: ScalacFlags): Module =
    Module(
      ProjectName("app"),
      Platforms(Jvm, Nil),
      ScalaVersions(scala3, Nil),
      Nil,
      dependsOn,
      SourcePos("histos.scala", 1, 1),
      flags = flags,
    )

  private def coreModule: Module =
    Module(
      ProjectName("core"),
      Platforms(Jvm, Nil),
      ScalaVersions(scala3, Nil),
      Nil,
      Nil,
      SourcePos("histos.scala", 1, 1),
    )

  private def compilerTool(request: histos.resolve.ToolRequest, sha: Sha256): LockedTool =
    LockedTool(
      request.id,
      request.roots,
      Repositories.ordered(request.repositories),
      request.roots.map(root => LockedArtifact(root.organization, root.name, root.revision, sha, Repository.Central)),
      Nil,
    )

  private def toolLock: DependencyLock =
    DependencyLock(Nil, Nil, List(compilerTool(CompilerRequests.compiler(scala3, repos), compilerSha)))

  private def asked(module: Module, modules: List[Module], lock: DependencyLock, jdk: RuntimeIdentity): SuiteAsk =
    SuiteAsk(
      module,
      modules,
      scala3,
      Jvm,
      lock,
      repos,
      ToolCatalog.default,
      NativeOrigin.Central,
      jdk,
      None,
    )

  private def base: SuiteAsk =
    val module = appModule(Nil, ScalacFlags.empty)
    asked(module, List(module), toolLock, jdkA)

  private def upstreamAsk: SuiteAsk =
    val linked = appModule(List(ProjectDep(ProjectName("core"), None)), ScalacFlags.empty)
    val sha    = Sha256.of(Chunk.fromArray("upstream-jar".getBytes(StandardCharsets.UTF_8)))
    val cell   = LockedCell(
      ProjectName("core"),
      scala3,
      Jvm,
      Nil,
      List(LockedArtifact("org.example", "core-lib", "1", sha, Repository.Central)),
      Nil,
    )
    asked(linked, List(coreModule, linked), toolLock.copy(cells = List(cell)), jdkA)

  private def jdkAsk: SuiteAsk =
    val module = appModule(Nil, ScalacFlags.empty)
    asked(module, List(module), toolLock, jdkB)

  private def flagsAsk: SuiteAsk =
    val module = appModule(Nil, ScalacFlags.of(ScalacFlag.EncodingUtf8))
    asked(module, List(module), toolLock, jdkA)

  private def apiFile(root: Path, name: ProjectName): Either[CoverGap, Path] =
    GraphCompile.analysisDirOf(GraphCompile.classesDir(root, name, scala3, Jvm)) match
      case Some(dir) => Right(dir.resolve(ApiDigest.fileName))
      case None      => Left(CoverGap.NoApi(name, scala3, Jvm))

  private def requireDigest(root: Path, classes: Path): ZIO[Any, CoverGap | StampError, Sha256] =
    ZIO
      .fromEither(SuiteCover.stored(root, base))
      .flatMap: cover =>
        SuiteStamp
          .digest(suiteName.value, List(classes), cover)
          .flatMap:
            case Some(value) => ZIO.succeed(value)
            case None        => ZIO.fail(StampError.Missing(suiteName.value))

  private def ran(module: ProjectName, status: TestStatus): Chunk[TestFeed] =
    Chunk(
      TestFeed.Begin(module, scala3, Jvm),
      TestFeed.Out(suiteName.value),
      TestFeed.Done(TestedCell(module, scala3, Jvm, status)),
    )

  private def quick(
      repo: Path,
      module: ProjectName,
      classes: Path,
      status: TestStatus,
      ask: SuiteAsk,
  ): ZIO[Any, ModuleTestError, Chunk[TestFeed]] =
    SuiteLaunch
      .remember(
        repo,
        CompiledCell(
          module,
          scala3,
          Jvm,
          classes,
          1,
          key = None,
          changed = true,
          sources = None,
          classesWritten = None,
        ),
        List(suiteName.value),
        List(classes),
        ask,
        TestMode.Quick,
        TestStore.Off,
        names =>
          ZIO.succeed(
            ZStream.fromIterable(
              List(
                TestFeed.Begin(module, scala3, Jvm),
                TestFeed.Out(names.mkString(" ")),
                TestFeed.Done(TestedCell(module, scala3, Jvm, status)),
              )
            )
          ),
      )
      .flatMap(_.runCollect)

  private def unreadable(path: Path): ZIO[Scope, Throwable, Unit] =
    ZIO
      .acquireRelease(
        ZIO.attempt(
          Files.setPosixFilePermissions(
            path,
            PosixFilePermissions.fromString("---------"),
          )
        )
      )(_ =>
        ZIO
          .attempt(
            Files.setPosixFilePermissions(
              path,
              PosixFilePermissions.fromString("rw-------"),
            )
          )
          .ignore
      )
      .unit

  def spec = suite("quick")(stamps, sources, runs)
end QuickSpec
