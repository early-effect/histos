package histos.native

import histos.*
import histos.build.BuildNode
import histos.cache.LinkerStarts
import histos.pack.MainClass
import histos.plugin.SourceSet
import histos.process.{ChildEvent, ChildProcess}
import histos.testrun.{TestExecutionFixtures, SuiteStamp, SuiteName, QuickRecord}
import histos.workspace.{IoFailure, IoFault, Workspace}

import java.nio.file.{Files, Path}
import java.util.zip.{ZipEntry, ZipOutputStream}

import _root_.scala.jdk.CollectionConverters.*

import zio.{ZIO, durationInt}
import zio.test.*

object NativeInputsSpec extends ZIOSpecDefault:
  private val version    = TestExecutionFixtures.version
  private val appName    = ProjectName("app")
  private val memberName = ProjectName("member")
  private val appStage   = CompilationStage(Cell(appName, version, Native), SourceSet.Main)

  private val tests = suite("Native physical link inputs")(
    test("a classpath resource is embedded and a resource-only change cannot restore the old binary") {
      val graph = Projects(
        module("app", Platform.Native)(version).clang(ClangMajor(21)).nativeVersion(TestExecutionFixtures.nightly)
      )
      ZIO
        .fromEither(graph)
        .flatMap: graph =>
          TestExecutionFixtures.withGraph(graph): fixture =>
            for
              _ <- Workspace.writeString(fixture.repo.resolve("app/src/demo/App.scala"), resourceMain)
              resource = fixture.repo.resolve("app/src/resources/member.txt")
              _        <- Workspace.writeString(resource, "one")
              products <- fixture.ensure(List(BuildNode.Compile(appStage)))
              app      <- ZIO.fromEither(products.require(appStage))
              first    <- link(fixture, app.classpath, List(ResourceName("member.txt")))
              before   <- output(fixture, first)
              _        <- Workspace.writeString(resource, "two")
              updated  <- fixture.ensure(List(BuildNode.Compile(appStage)))
              next     <- ZIO.fromEither(updated.require(appStage))
              second   <- link(fixture, next.classpath, List(ResourceName("member.txt")))
              after    <- output(fixture, second)
              third    <- link(fixture, next.classpath, List(ResourceName("member.txt")))
              warm     <- output(fixture, third)
              starts   <- ZIO.serviceWithZIO[LinkerStarts](_.get)
            yield assertTrue(before == List("one"), after == List("two"), warm == List("two"), starts == 2)
    },
    test("same-path member jar body replacement invalidates a binary even when the app ABI and product are unchanged") {
      val member =
        module("member", Platform.Native)(version).clang(ClangMajor(21)).nativeVersion(TestExecutionFixtures.nightly)
      val graph = Projects(
        member,
        module("app", Platform.Native)(version)
          .clang(ClangMajor(21))
          .nativeVersion(TestExecutionFixtures.nightly)
          .dependsOn(member),
      )
      ZIO
        .fromEither(graph)
        .flatMap: graph =>
          TestExecutionFixtures.withGraph(graph): fixture =>
            for
              _ <- Workspace.writeString(
                fixture.repo.resolve("app/src/demo/App.scala"),
                "package demo\nobject App:\n  def main(args: Array[String]): Unit = println(Member.text)\n",
              )
              source = fixture.repo.resolve("member/src/demo/Member.scala")
              _       <- Workspace.writeString(source, memberSource("one"))
              initial <- fixture.ensure(List(BuildNode.Compile(appStage)))
              app     <- ZIO.fromEither(initial.require(appStage))
              member  <- ZIO
                .fromEither(initial.require(CompilationStage(Cell(memberName, version, Native), SourceSet.Main)))
              jar = fixture.repo.resolve("member.jar")
              _ <- archive(member.classes, jar)
              classpath = app.classpath.map(path => if path == member.classes then jar else path)
              first    <- link(fixture, classpath)
              before   <- output(fixture, first)
              previous <- quickDigest(app.classes, classpath)
              _ <- QuickRecord.save(fixture.repo, appName, version, Native, SuiteName.trusted("demo.App"), previous)
              _ <- Workspace.writeString(source, memberSource("two"))
              updated    <- fixture.ensure(List(BuildNode.Compile(appStage)))
              next       <- ZIO.fromEither(updated.require(appStage))
              nextMember <- ZIO.fromEither(updated.require(member.stage))
              _          <- archive(nextMember.classes, jar)
              second     <- link(fixture, classpath)
              after      <- output(fixture, second)
              current    <- quickDigest(next.classes, classpath)
              quick  <- QuickRecord.hit(fixture.repo, appName, version, Native, SuiteName.trusted("demo.App"), current)
              _      <- link(fixture, classpath)
              starts <- ZIO.serviceWithZIO[LinkerStarts](_.get)
            yield assertTrue(
              before == List("one"),
              after == List("two"),
              starts == 2,
              app.api == next.api,
              previous != current,
              !quick,
            )
    },
    test("declared resources must exist before any actual Native link starts") {
      val graph =
        Projects(module("app", Native)(version).clang(ClangMajor(21)).nativeVersion(TestExecutionFixtures.nightly))
      ZIO
        .fromEither(graph)
        .flatMap: graph =>
          TestExecutionFixtures.withGraph(graph): fixture =>
            for
              _ <- Workspace.writeString(
                fixture.repo.resolve("app/src/demo/App.scala"),
                "package demo\nobject App:\n  def main(args: Array[String]): Unit = println(1)\n",
              )
              products <- fixture.ensure(List(BuildNode.Compile(appStage)))
              app      <- ZIO.fromEither(products.require(appStage))
              refused  <- link(fixture, app.classpath, List(ResourceName("missing.txt"))).either
              starts   <- ZIO.serviceWithZIO[LinkerStarts](_.get)
            yield assertTrue(refused == Left(NativeError.MissingResource(ResourceName("missing.txt"))), starts == 0)
    },
    test("physical input hashing refuses embedded symbolic links") {
      ZIO.scoped:
        for
          root    <- Workspace.work("histos-native-input-links")
          classes <- Workspace.createDirectories(root.resolve("classes"))
          file = root.resolve("outside.txt")
          _ <- Workspace.writeString(file, "outside")
          link = classes.resolve("linked.txt")
          _ <- ZIO.attemptBlocking(Files.createSymbolicLink(link, file)).mapError(_ => IoFailure(link, IoFault.Create))
          refused <- NativeLink.inputDigests(List(classes)).either
        yield assertTrue(refused == Left(NativeError.InputLink(link)))
    },
  )

  def spec = tests @@ TestAspect.withLiveClock @@ TestAspect.timeout(10.minutes) @@ TestAspect.sequential

  private def quickDigest(classes: Path, classpath: List[Path]) =
    for
      hashes <- NativeLink.inputDigests(classpath)
      digest <- SuiteStamp.digest(
        "demo.App",
        List(classes),
        SuiteStamp.Cover(hashes.map(_.value), Nil, "fixture", Nil, Nil),
      )
      found <- ZIO.fromOption(digest).orElseFail(IoFailure(classes, IoFault.Read))
    yield found

  private def memberSource(value: String): String = s"package demo\nobject Member:\n  def text: String = \"$value\"\n"

  private val resourceMain =
    """package demo
      |object App:
      |  def main(args: Array[String]): Unit =
      |    val text = Option(getClass.getResourceAsStream("/member.txt")).fold("missing"): input =>
      |      try new String(input.readAllBytes(), java.nio.charset.StandardCharsets.UTF_8)
      |      finally input.close()
      |    println(text)
      |""".stripMargin

  private def link(fixture: TestExecutionFixtures.Fixture, classpath: List[Path], resources: List[ResourceName] = Nil) =
    for
      toolchain <- ZIO.fromEither(NativeToolchain.of(TestExecutionFixtures.nightly, fixture.runtimes.native))
      binary    <- NativeLink.linkClasspath(
        NativeLinkRequest(
          ClangMajor(21),
          toolchain,
          fixture.runtimes.env,
          fixture.repo.resolve("link"),
          fixture.session.jdk,
          MainClass.trusted("demo.App"),
          version,
          resources = resources,
        ),
        classpath,
      )
    yield binary

  private def output(fixture: TestExecutionFixtures.Fixture, binary: Path) =
    Workspace
      .recreate(fixture.repo.resolve("run"))
      .flatMap: work =>
        ChildProcess
          .command(
            List(binary.toString),
            work,
            fixture.runtimes.env ++ NativeStack.environment(ThreadStack.default),
            histos.process.RunRelay.Closed,
          )
          .runCollect
          .map(_.collect { case ChildEvent.Out(text) => text }.toList)

  private def archive(classes: Path, jar: Path): ZIO[Any, IoFailure, Unit] =
    ZIO
      .attemptBlocking:
        val entries = Files.walk(classes)
        val stream  = new ZipOutputStream(Files.newOutputStream(jar))
        try
          entries.iterator.asScala
            .filter(Files.isRegularFile(_))
            .toList
            .sortBy(_.toString)
            .foreach: path =>
              stream.putNextEntry(new ZipEntry(classes.relativize(path).toString.replace('\\', '/')))
              stream.write(Files.readAllBytes(path))
              stream.closeEntry()
        finally
          stream.close()
          entries.close()
      .mapError(_ => IoFailure(jar, IoFault.Write))
end NativeInputsSpec
