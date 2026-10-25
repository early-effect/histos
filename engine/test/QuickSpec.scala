package histos.testrun

import histos.native.ClassShapes
import histos.watch.SourceWatch
import histos.workspace.{IoFailure, IoFault, Workspace}
import histos.zinc.CompiledCell
import histos.{Jvm, ProjectName, ScalaVersion}

import java.nio.file.{Files, Path}
import java.nio.file.attribute.PosixFilePermissions

import org.objectweb.asm.{ClassWriter, Opcodes}

import zio.{Chunk, Scope, ZIO, ZLayer}
import zio.stream.ZStream
import zio.test.*

object QuickSpec extends ZIOSpecDefault:
  private val jdkToken = "test-jdk"
  private val plain = SuiteStamp.Cover(Nil, Nil, jdkToken, Nil)

  private def jars(names: List[String]): SuiteStamp.Cover =
    plain.copy(jars = names)

  private val stamps = suite("suite stamp")(
    test(
      "a referenced class is in the digest and an unreferenced class is not"
    ):
      ZIO.scoped:
        for
          root <- temp
          _ <- ZIO.attempt {
            writeClass(root, "demo/Foo", List("demo/Lib"))
            writeClass(root, "demo/Lib", Nil)
            writeClass(root, "demo/Other", Nil)
          }
          first <- SuiteStamp.digest(
            "demo.Foo",
            List(root),
            jars(List("jar-a"))
          )
          _ <- ZIO.attempt(writeClass(root, "demo/Other", List("demo/Nowhere")))
          other <- SuiteStamp.digest(
            "demo.Foo",
            List(root),
            jars(List("jar-a"))
          )
          _ <- ZIO.attempt(writeClass(root, "demo/Lib", List("demo/Other")))
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
          _ <- ZIO.attempt {
            writeClass(root, "demo/Foo", List("demo/Lib"))
            writeClass(root, "demo/Lib", Nil)
          }
          direct <- SuiteStamp.digest(
            "demo.Foo",
            List(root),
            jars(List("jar-a"))
          )
          tree <- SuiteStamp.index(List(root))
          indexed <- ZIO.fromEither(
            SuiteStamp.digest("demo.Foo", tree, jars(List("jar-a")))
          )
        yield assertTrue(direct.isDefined && direct == indexed)
    ,
    test("a stored success matches only that digest"):
      ZIO.scoped:
        for
          root <- temp
          module <- ZIO.fromEither(ProjectName.from("app"))
          _ <- QuickRecord.save(
            root,
            module,
            ScalaVersion("3.10.0"),
            Jvm,
            "demo.Foo",
            "aaa"
          )
          hit <- QuickRecord.hit(
            root,
            module,
            ScalaVersion("3.10.0"),
            Jvm,
            "demo.Foo",
            "aaa"
          )
          miss <- QuickRecord.hit(
            root,
            module,
            ScalaVersion("3.10.0"),
            Jvm,
            "demo.Foo",
            "bbb"
          )
          _ <- QuickRecord.clear(
            root,
            module,
            ScalaVersion("3.10.0"),
            Jvm,
            "demo.Foo"
          )
          gone <- QuickRecord.hit(
            root,
            module,
            ScalaVersion("3.10.0"),
            Jvm,
            "demo.Foo",
            "aaa"
          )
        yield assertTrue(hit && !miss && !gone)
    ,
    test("a zio spec is chosen and a helper is not"):
      val local = List(
        ClassShapes.Shape(
          "demo.FooA$",
          module = true,
          parents = List("zio.test.ZIOSpecDefault")
        ),
        ClassShapes.Shape(
          "demo.Helper",
          module = false,
          parents = List("java.lang.Object")
        )
      )
      val world = (local ++ List(
        ClassShapes.Shape(
          "zio.test.ZIOSpecDefault",
          module = false,
          parents = List("zio.test.ZIOSpecAbstract")
        )
      )).map(shape => shape.name -> shape).toMap
      val marks =
        List(SuiteNames.Mark("zio.test.ZIOSpecAbstract", module = true))
      assertTrue(SuiteNames.choose(local, world, marks) == List("demo.FooA"))
    ,
    test("upstream jars, the jdk, and flags change the digest"):
      ZIO.scoped:
        for
          root <- temp
          _ <- ZIO.attempt(writeClass(root, "demo/Foo", Nil))
          same <- SuiteStamp.digest("demo.Foo", List(root), plain)
          upstream <- SuiteStamp.digest(
            "demo.Foo",
            List(root),
            plain.copy(upstream = List("up-1"))
          )
          jdk <- SuiteStamp.digest(
            "demo.Foo",
            List(root),
            plain.copy(jdk = "other-jdk")
          )
          flags <- SuiteStamp.digest(
            "demo.Foo",
            List(root),
            plain.copy(flags = List("-encoding", "UTF-8"))
          )
        yield assertTrue(
          same.isDefined && upstream != same && jdk != same && flags != same
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
          _ <- ZIO.attempt(Files.createDirectories(dir))
          _ <- unreadable(dir)
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
          root <- temp
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
                PosixFilePermissions.fromString("r-x------")
              )
            )
          )(_ =>
            ZIO
              .attempt(
                Files.setPosixFilePermissions(
                  parent,
                  PosixFilePermissions.fromString("rwx------")
                )
              )
              .ignore
          )
          saved <- QuickRecord
            .save(root, module, scala3, Jvm, suiteName, "aaa")
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
          cleared == Left(QuickError.Clear(IoFailure(path, IoFault.Delete)))
        )
  )

  private val runs = suite("testQuick run")(
    test("an absent quick record runs the suite"):
      ZIO.scoped:
        for
          root <- temp
          classes = root.resolve("classes")
          _ <- ZIO.attempt(writeClass(classes, "demo/Foo", Nil))
          module <- named
          digest <- requireDigest(classes)
          recorded <- QuickRecord.hit(
            root,
            module,
            scala3,
            Jvm,
            suiteName,
            digest
          )
          feeds <- quick(root, module, classes, TestStatus.Passed, plain)
        yield assertTrue(!recorded, feeds == ran(module, TestStatus.Passed))
    ,
    test("a digest mismatch runs the suite"):
      ZIO.scoped:
        for
          root <- temp
          classes = root.resolve("classes")
          _ <- ZIO.attempt(writeClass(classes, "demo/Foo", Nil))
          module <- named
          digest <- requireDigest(classes)
          _ <- QuickRecord.save(
            root,
            module,
            scala3,
            Jvm,
            suiteName,
            "not-this-digest"
          )
          recorded <- QuickRecord.hit(
            root,
            module,
            scala3,
            Jvm,
            suiteName,
            digest
          )
          stored <- QuickRecord.hit(
            root,
            module,
            scala3,
            Jvm,
            suiteName,
            "not-this-digest"
          )
          feeds <- quick(root, module, classes, TestStatus.Passed, plain)
        yield assertTrue(
          digest != "not-this-digest",
          !recorded,
          stored,
          feeds == ran(module, TestStatus.Passed)
        )
    ,
    test("an unreadable quick record runs the suite"):
      ZIO.scoped:
        for
          root <- temp
          classes = root.resolve("classes")
          _ <- ZIO.attempt(writeClass(classes, "demo/Foo", Nil))
          module <- named
          digest <- requireDigest(classes)
          _ <- QuickRecord.save(
            root,
            module,
            scala3,
            Jvm,
            suiteName,
            digest
          )
          readable <- QuickRecord.hit(
            root,
            module,
            scala3,
            Jvm,
            suiteName,
            digest
          )
          _ <- unreadable(
            QuickRecord.file(root, module, scala3, Jvm, suiteName)
          )
          hidden <- QuickRecord.hit(
            root,
            module,
            scala3,
            Jvm,
            suiteName,
            digest
          )
          feeds <- quick(root, module, classes, TestStatus.Passed, plain)
        yield assertTrue(
          readable,
          !hidden,
          feeds == ran(module, TestStatus.Passed)
        )
    ,
    test("a suite with no digest runs"):
      ZIO.scoped:
        for
          root <- temp
          classes = root.resolve("classes")
          _ <- ZIO.attempt(Files.createDirectories(classes))
          module <- named
          _ <- QuickRecord.save(
            root,
            module,
            scala3,
            Jvm,
            suiteName,
            "recorded"
          )
          missing <- SuiteStamp.digest(suiteName, List(classes), plain)
          feeds <- quick(root, module, classes, TestStatus.Passed, plain)
        yield assertTrue(
          missing.isEmpty,
          feeds == ran(module, TestStatus.Passed)
        )
    ,
    test("a record cleared after a failure runs the suite"):
      ZIO.scoped:
        for
          root <- temp
          classes = root.resolve("classes")
          _ <- ZIO.attempt(writeClass(classes, "demo/Foo", Nil))
          module <- named
          digest <- requireDigest(classes)
          _ <- QuickRecord.save(
            root,
            module,
            scala3,
            Jvm,
            suiteName,
            digest
          )
          _ <- ZIO.attempt(
            writeClass(classes, "demo/Foo", List("demo/Other"))
          )
          changed <- requireDigest(classes)
          failed <- quick(root, module, classes, TestStatus.Failed(1), plain)
          cleared <- QuickRecord.hit(
            root,
            module,
            scala3,
            Jvm,
            suiteName,
            digest
          )
          replaced <- QuickRecord.hit(
            root,
            module,
            scala3,
            Jvm,
            suiteName,
            changed
          )
          _ <- ZIO.attempt(writeClass(classes, "demo/Foo", Nil))
          restored <- requireDigest(classes)
          again <- quick(root, module, classes, TestStatus.Passed, plain)
        yield assertTrue(
          digest != changed,
          restored == digest,
          failed == ran(module, TestStatus.Failed(1)),
          !cleared,
          !replaced,
          again == ran(module, TestStatus.Passed)
        )
    ,
    test("a quick hit is cached and the suite does not run"):
      ZIO.scoped:
        for
          root <- temp
          classes = root.resolve("classes")
          _ <- ZIO.attempt(writeClass(classes, "demo/Foo", Nil))
          module <- named
          digest <- requireDigest(classes)
          _ <- QuickRecord.save(
            root,
            module,
            scala3,
            Jvm,
            suiteName,
            digest
          )
          feeds <- quick(root, module, classes, TestStatus.Passed, plain)
        yield assertTrue(
          feeds == Chunk(
            TestFeed.Cached(module, scala3, Jvm, suiteName)
          )
        )
    ,
    test("a stored success misses when the upstream jar, jdk, or flags differ"):
      ZIO.scoped:
        for
          root <- temp
          classes = root.resolve("classes")
          _ <- ZIO.attempt(writeClass(classes, "demo/Foo", Nil))
          module <- named
          digest <- requireDigest(classes)
          _ <- QuickRecord.save(
            root,
            module,
            scala3,
            Jvm,
            suiteName,
            digest
          )
          same <- quick(root, module, classes, TestStatus.Passed, plain)
          upstream <- quick(
            root,
            module,
            classes,
            TestStatus.Passed,
            plain.copy(upstream = List("up-1"))
          )
          _ <- QuickRecord.save(
            root,
            module,
            scala3,
            Jvm,
            suiteName,
            digest
          )
          jdk <- quick(
            root,
            module,
            classes,
            TestStatus.Passed,
            plain.copy(jdk = "other-jdk")
          )
          _ <- QuickRecord.save(
            root,
            module,
            scala3,
            Jvm,
            suiteName,
            digest
          )
          flags <- quick(
            root,
            module,
            classes,
            TestStatus.Passed,
            plain.copy(flags = List("-encoding", "UTF-8"))
          )
        yield assertTrue(
          same == Chunk(TestFeed.Cached(module, scala3, Jvm, suiteName)),
          upstream == ran(module, TestStatus.Passed),
          jdk == ran(module, TestStatus.Passed),
          flags == ran(module, TestStatus.Passed)
        )
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
          _ <- ZIO.attempt(Files.writeString(file, "two"))
          after <- SourceWatch.take(
            List(root.resolve("src"), root.resolve("histos.scala"))
          )
        yield assertTrue(before == same && before != after)
  )

  private def writeClass(
      root: Path,
      internal: String,
      calls: List[String]
  ): Unit =
    val writer = new ClassWriter(ClassWriter.COMPUTE_MAXS)
    writer.visit(
      Opcodes.V1_8,
      Opcodes.ACC_PUBLIC | Opcodes.ACC_SUPER,
      internal,
      null,
      "java/lang/Object",
      null
    )
    val method = writer.visitMethod(
      Opcodes.ACC_PUBLIC | Opcodes.ACC_STATIC,
      "run",
      "()V",
      null,
      null
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

  private val scala3 = ScalaVersion("3.10.0")
  private val suiteName = "demo.Foo"

  private def named: ZIO[Any, histos.ProjectNameError, ProjectName] =
    ZIO.fromEither(ProjectName.from("app"))

  private def requireDigest(classes: Path): ZIO[Any, StampError, String] =
    SuiteStamp
      .digest(suiteName, List(classes), plain)
      .flatMap:
        case Some(value) => ZIO.succeed(value)
        case None        => ZIO.fail(StampError.Missing(suiteName))

  private def ran(module: ProjectName, status: TestStatus): Chunk[TestFeed] =
    Chunk(
      TestFeed.Begin(module, scala3, Jvm),
      TestFeed.Out(suiteName),
      TestFeed.Done(TestedCell(module, scala3, Jvm, status))
    )

  private def quick(
      repo: Path,
      module: ProjectName,
      classes: Path,
      status: TestStatus,
      cover: SuiteStamp.Cover
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
          classesWritten = None
        ),
        List(suiteName),
        List(classes),
        cover,
        quick = true,
        names =>
          ZIO.succeed(
            ZStream.fromIterable(
              List(
                TestFeed.Begin(module, scala3, Jvm),
                TestFeed.Out(names.mkString(" ")),
                TestFeed.Done(TestedCell(module, scala3, Jvm, status))
              )
            )
          )
      )
      .flatMap(_.runCollect)
      .provide(ZLayer.succeed(TestContext.empty))

  private def unreadable(path: Path): ZIO[Scope, Throwable, Unit] =
    ZIO
      .acquireRelease(
        ZIO.attempt(
          Files.setPosixFilePermissions(
            path,
            PosixFilePermissions.fromString("---------")
          )
        )
      )(_ =>
        ZIO
          .attempt(
            Files.setPosixFilePermissions(
              path,
              PosixFilePermissions.fromString("rw-------")
            )
          )
          .ignore
      )
      .unit

  def spec = suite("quick")(stamps, sources, runs)
