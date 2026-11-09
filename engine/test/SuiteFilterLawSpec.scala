package histos.testrun

import histos.workspace.Workspace
import histos.{
  ClangMajor,
  ClangPin,
  Dep,
  DepKind,
  Module,
  Platform,
  Platforms,
  ProjectName,
  ScalaVersion,
  ScalaVersions,
  SourcePos,
  TestProvider,
}

import java.nio.file.{Files, Path}

import zio.Chunk
import zio.test.*

object SuiteFilterLawSpec extends ZIOSpecDefault:
  private val name    = ProjectName.trusted("core")
  private val version = ScalaVersion.trusted("3.10.0")

  def spec = suite("suite filter laws")(
    test("a clang major is that pin, and an unused pin is a link failure"):
      val pinned = module(ClangPin.Major(ClangMajor.trusted(17)), Nil)
      val bare   = module(ClangPin.Unused, Nil)
      for
        major <- SuiteLaunch.nativeMajor(pinned)
        none  <- SuiteLaunch.nativeMajor(bare).either
      yield assertTrue(
        major == ClangMajor.trusted(17),
        none == Left(
          ModuleTestError.LinkFailed(name, version, Platform.Native, "the module does not name a clang major")
        ),
      )
    ,
    test("a blank pattern is dropped, and a finished cell is that status"):
      val cell = histos.zinc.CompiledCell(name, version, Platform.Jvm, Path.of("classes"), 0, None, false, None, None)
      for done <- SuiteLaunch.finished(cell, TestStatus.Failed(3)).runCollect
      yield assertTrue(
        SuiteLaunch.exact(List("demo.*", "", "has space", "A")).map(_.value) == List("demo.*", "A"),
        SuiteLaunch.linkFailed(cell, "clang") ==
          ModuleTestError.LinkFailed(name, version, Platform.Jvm, "clang"),
        done == Chunk(TestFeed.Done(TestedCell(name, version, Platform.Jvm, TestStatus.Failed(3)))),
      )
    ,
    test("anon classes are not suite names, a missing binary does not match, and an empty pattern list does"):
      (SuitePattern.from("demo.Hello"), SuitePattern.from("demo.Other"), SuitePattern.from("nope")) match
        case (Right(hello), Right(other), Right(nope)) =>
          Workspace
            .work("histos-suite-filter")
            .flatMap: root =>
              val classes   = root.resolve("classes")
              val helloFile = classes.resolve("demo").resolve("Hello.class")
              val anon      = classes.resolve("anon").resolve("Hello$$anon.class")
              val note      = classes.resolve("demo").resolve("readme.txt")
              val dot       = classes.resolve(".class")
              for
                _    <- write(helloFile, Array(1))
                _    <- write(anon, Array(2))
                _    <- write(note, Array(3))
                _    <- write(dot, Array(4))
                open <- SuiteLaunch.narrowed(classes, Nil)
                file <- SuiteLaunch.narrowed(note, List(hello))
                hit  <- SuiteLaunch.narrowed(classes, List(hello))
                miss <- SuiteLaunch.narrowed(classes, List(other))
                only <- SuiteLaunch.narrowed(classes.resolve("anon"), List(nope))
              yield
                val names    = SuiteLaunch.classBinaries(classes)
                val anonName = SuiteLaunch.classBinary(classes, anon)
                val text     = SuiteLaunch.classBinary(classes, note)
                val blank    = SuiteLaunch.classBinary(classes, dot)
                assertTrue(
                  names == List("demo.Hello"),
                  anonName == None,
                  text == None,
                  blank == None,
                  open,
                  file,
                  hit,
                  !miss,
                  only,
                )
        case _ => zio.ZIO.succeed(assertTrue(false))
    ,
    test("a provider is satisfied only by its own library"):
      val kit   = Dep.trusted("dev.zio", "zio-test-sbt", "2.1.27", DepKind.Java)
      val other = Dep.trusted("dev.zio", "zio-test", "2.1.27", DepKind.Java)
      assertTrue(
        SuiteLaunch.satisfied(module(ClangPin.Unused, List(kit)), TestProvider.Zio),
        !SuiteLaunch.satisfied(module(ClangPin.Unused, List(other)), TestProvider.Zio),
        !SuiteLaunch.satisfied(module(ClangPin.Unused, Nil), TestProvider.Zio),
      ),
  )

  private def module(clang: ClangPin, tests: List[Dep]): Module =
    Module(
      name,
      Platforms(Platform.Native, Nil),
      ScalaVersions(version, Nil),
      Nil,
      Nil,
      SourcePos("law.scala", 1, 1),
      clang = clang,
      testDependencies = tests,
    )

  private def write(path: Path, bytes: Array[Byte]): zio.UIO[Unit] =
    zio.ZIO
      .attemptBlocking {
        Option(path.getParent).foreach(parent => Files.createDirectories(parent))
        Files.write(path, bytes)
        ()
      }
      .catchAll(error => zio.ZIO.dieMessage(error.getClass.getName))
end SuiteFilterLawSpec
