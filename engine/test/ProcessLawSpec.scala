package histos.process

import histos.pack.MainClass
import histos.workspace.{IoFailure, IoFault, Workspace}
import histos.Platform

import java.nio.file.{Files, Path}

import zio.ZIO
import zio.test.*

object ProcessLawSpec extends ZIOSpecDefault:
  def spec = suite("child process laws")(
    test("javascript and native fail before a program is required"):
      check(Gen.fromIterable(List(Platform.Js, Platform.Native))): platform =>
        ChildProcess
          .events(request(Path.of("/missing-java"), Path.of("/classes"), Path.of("/work"), platform))
          .runCollect
          .either
          .map(error => assertTrue(error == Left(ChildError.UnsupportedPlatform(platform))))
    ,
    test("a command with no program fails before the work directory is read"):
      ChildProcess
        .command(Nil, Path.of("/missing-work"), Map.empty)
        .runCollect
        .either
        .map(error => assertTrue(error == Left(ChildError.NoProgram)))
    ,
    test("java, javac, classes, artifacts, and a noisy work directory fail in that order"):
      Workspace
        .work("histos-child")
        .flatMap: dir =>
          val java     = dir.resolve("bin/java")
          val javac    = dir.resolve("bin/javac")
          val classes  = dir.resolve("classes")
          val artifact = dir.resolve("lib.jar")
          val work     = dir.resolve("work")
          val orphan   = Path.of("java")
          for
            noParent <- ChildProcess.events(request(orphan, classes, work)).runCollect.either
            absent   <- ChildProcess.events(request(dir.resolve("missing-java"), classes, work)).runCollect.either
            _        <- writeFile(java)
            noJavac  <- ChildProcess.events(request(java, classes, work)).runCollect.either
            _        <- writeFile(javac)
            noClass  <- ChildProcess.events(request(java, classes, work)).runCollect.either
            _        <- Workspace.createDirectories(classes)
            noJar    <- ChildProcess.events(request(java, classes, work, artifacts = List(artifact))).runCollect.either
            _        <- writeFile(artifact)
            _        <- Workspace.createDirectories(work)
            _        <- writeFile(work.resolve("noise"))
            noisy    <- ChildProcess.events(request(java, classes, work, artifacts = List(artifact))).runCollect.either
          yield assertTrue(
            noParent == Left(ChildError.Java(orphan)),
            absent == Left(ChildError.Java(dir.resolve("missing-java"))),
            noJavac == Left(ChildError.Java(javac)),
            noClass == Left(ChildError.Classes(classes)),
            noJar == Left(ChildError.Artifact(artifact)),
            noisy == Left(ChildError.Work(work)),
          )
    ,
    test("a zero status and a non-zero status are both outcomes, and a failed start stays that error"):
      val failed = ChildError.NoProgram
      for
        passed    <- Child.run(start("/usr/bin/true"), failed)
        failedRun <- Child.run(start("/usr/bin/false"), failed)
        refused   <- Child.run(ZIO.fail(failed), ChildError.Status).either
      yield assertTrue(
        passed.status == 0 && passed.output.isEmpty,
        failedRun.status != 0 && failedRun.output.isEmpty,
        refused == Left(failed),
      ),
  )

  private def request(
      java: Path,
      classes: Path,
      work: Path,
      platform: Platform = Platform.Jvm,
      artifacts: List[Path] = Nil,
  ): ChildRequest =
    ChildRequest(platform, java, ChildClasspath(classes, artifacts), MainClass.trusted("demo.Main"), work)

  private def writeFile(path: Path): ZIO[Any, IoFailure, Unit] =
    Workspace.writeString(path, "")

  private def start(program: String): ZIO[Any, ChildError, Process] =
    ZIO
      .attemptBlocking(new ProcessBuilder(program).start())
      .mapError(_ => ChildError.Start(program))
end ProcessLawSpec
