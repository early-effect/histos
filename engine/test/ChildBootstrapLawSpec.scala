package histos.process

import histos.pack.MainClass
import histos.workspace.Workspace
import histos.Platform

import java.nio.file.{Files, Path}

import zio.{System, ZIO}
import zio.test.{TestAspect, ZIOSpecDefault, assertTrue, suite, test}

/** The JVM bootstrap compiles its entry, becomes a process group, and then runs the named main. */
object ChildBootstrapLawSpec extends ZIOSpecDefault:
  def spec = suite("child bootstrap laws")(
    test("setpgid runs before the named main, and a zero group is a zero status"):
      Workspace
        .work("histos-child-boot")
        .flatMap: dir =>
          val classes = dir.resolve("classes")
          val work    = dir.resolve("work")
          val source  = dir.resolve("src").resolve("law").resolve("Main.java")
          for
            home <- System.property("java.home").orDie
            java <- home match
              case Some(value) if value.nonEmpty => ZIO.succeed(Path.of(value, "bin", "java"))
              case _                             => ZIO.dieMessage("java.home is absent")
            javac <- Option(java.getParent) match
              case Some(bin) => ZIO.succeed(bin.resolve("javac"))
              case None      => ZIO.dieMessage("java has no parent directory")
            _        <- Workspace.createDirectories(classes)
            _        <- Workspace.createDirectories(work)
            _        <- Workspace.createDirectories(parentOf(source))
            _        <- Workspace.writeString(source, mainSource)
            compiled <- compileJava(javac, source, classes, dir)
            _        <- ZIO.when(compiled.nonEmpty)(ZIO.dieMessage(compiled))
            ran      <- ChildProcess
              .run(
                ChildRequest(
                  Platform.Jvm,
                  java,
                  ChildClasspath(classes, Nil),
                  MainClass.trusted("law.Main"),
                  work,
                )
              )
              .catchAll(error => ZIO.dieMessage(error.message))
            group  <- ZIO.attemptBlocking(Files.readString(work.resolve("group"))).orDie
            marker <- ZIO.attemptBlocking(Files.readString(work.resolve("ran"))).orDie
          yield assertTrue(ran.exit == ProcessExit(0), group.trim == "0", marker.trim == "ok")
  ) @@ TestAspect.withLiveSystem

  private val mainSource =
    """package law;
      |public final class Main {
      |  public static void main(String[] args) throws Exception {
      |    java.nio.file.Files.writeString(java.nio.file.Path.of("ran"), "ok");
      |  }
      |}
      |""".stripMargin

  private def compileJava(compiler: Path, source: Path, classes: Path, directory: Path): ZIO[Any, Nothing, String] =
    ZIO.attemptBlocking {
      val process = new ProcessBuilder(compiler.toString, "-d", classes.toString, source.toString)
        .directory(directory.toFile)
        .redirectErrorStream(true)
        .start()
      val text = new String(process.getInputStream.readAllBytes(), java.nio.charset.StandardCharsets.UTF_8)
      val code = process.waitFor()
      if code == 0 then "" else text
    }.orDie

  private def parentOf(path: Path): Path =
    Option(path.getParent).getOrElse(path)
end ChildBootstrapLawSpec
