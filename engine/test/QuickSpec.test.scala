package histos.testrun

import histos.native.ClassShapes
import histos.watch.SourceWatch
import histos.{Jvm, ProjectName, ScalaVersion}

import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import org.objectweb.asm.{ClassWriter, Opcodes}

import zio.{Scope, ZIO}
import zio.test.*

object QuickSpec extends ZIOSpecDefault:
  private val stamps = suite("suite stamp")(
    test("a referenced class is in the digest and an unreferenced class is not"):
      ZIO.scoped:
        for
          root <- temp
          _ <- ZIO.attempt {
                 writeClass(root, "demo/Foo", List("demo/Lib"))
                 writeClass(root, "demo/Lib", Nil)
                 writeClass(root, "demo/Other", Nil)
               }
          first <- ZIO.attempt(SuiteStamp.digest("demo.Foo", List(root), List("jar-a")))
          _ <- ZIO.attempt(writeClass(root, "demo/Other", List("demo/Nowhere")))
          other <- ZIO.attempt(SuiteStamp.digest("demo.Foo", List(root), List("jar-a")))
          _ <- ZIO.attempt(writeClass(root, "demo/Lib", List("demo/Other")))
          lib <- ZIO.attempt(SuiteStamp.digest("demo.Foo", List(root), List("jar-a")))
          jar <- ZIO.attempt(SuiteStamp.digest("demo.Foo", List(root), List("jar-b")))
        yield assertTrue(
          first.isDefined && first == other && first != lib && lib != jar,
        )
    ,
    test("a stored success matches only that digest"):
      ZIO.scoped:
        for
          root <- temp
          module <- ZIO.fromEither(ProjectName.from("app")).mapError(error => new IllegalStateException(error.message))
          _ <- QuickRecord.save(root, module, ScalaVersion("3.10.0"), Jvm, "demo.Foo", "aaa")
          hit <- QuickRecord.hit(root, module, ScalaVersion("3.10.0"), Jvm, "demo.Foo", "aaa")
          miss <- QuickRecord.hit(root, module, ScalaVersion("3.10.0"), Jvm, "demo.Foo", "bbb")
          _ <- QuickRecord.clear(root, module, ScalaVersion("3.10.0"), Jvm, "demo.Foo")
          gone <- QuickRecord.hit(root, module, ScalaVersion("3.10.0"), Jvm, "demo.Foo", "aaa")
        yield assertTrue(hit && !miss && !gone)
    ,
    test("a zio spec is chosen and a helper is not"):
      val local = List(
        ClassShapes.Shape("demo.FooA$", module = true, parents = List("zio.test.ZIOSpecDefault")),
        ClassShapes.Shape("demo.Helper", module = false, parents = List("java.lang.Object")),
      )
      val world = (local ++ List(
        ClassShapes.Shape("zio.test.ZIOSpecDefault", module = false, parents = List("zio.test.ZIOSpecAbstract")),
      )).map(shape => shape.name -> shape).toMap
      val marks = List(SuiteNames.Mark("zio.test.ZIOSpecAbstract", module = true))
      assertTrue(SuiteNames.choose(local, world, marks) == List("demo.FooA"))
    ,
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
          before <- SourceWatch.take(List(root.resolve("src"), root.resolve("histos.scala")))
          same <- SourceWatch.take(List(root.resolve("src"), root.resolve("histos.scala")))
          _ <- ZIO.attempt(Files.writeString(file, "two"))
          after <- SourceWatch.take(List(root.resolve("src"), root.resolve("histos.scala")))
        yield assertTrue(before == same && before != after)
    ,
  )

  private def writeClass(root: Path, internal: String, calls: List[String]): Unit =
    val writer = new ClassWriter(ClassWriter.COMPUTE_MAXS)
    writer.visit(Opcodes.V1_8, Opcodes.ACC_PUBLIC | Opcodes.ACC_SUPER, internal, null, "java/lang/Object", null)
    val method = writer.visitMethod(Opcodes.ACC_PUBLIC | Opcodes.ACC_STATIC, "run", "()V", null, null)
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
    ZIO.acquireRelease(ZIO.attempt(Files.createTempDirectory("histos-quick"))): dir =>
      ZIO.attempt {
        if Files.exists(dir) then
          val walk = Files.walk(dir)
          val paths =
            try walk.iterator.asScala.toList
            finally walk.close()
          paths.sortBy(_.getNameCount).reverse.foreach(path => Files.deleteIfExists(path))
      }.ignore

  def spec = suite("quick")(stamps, sources)
