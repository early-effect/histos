package histos.load

import histos.cache.ContentStore
import histos.suite.HistosSpec
import histos.workspace.Workspace
import histos.zinc.CompilerStarts

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import scala.util.Try

import zio.test.*
import zio.{Ref, Scope, ZIO, ZLayer}

object BuildCacheLawSpec extends HistosSpec:
  private val Source =
    """|object Quiet:
       |  def token: Int = 1
       |""".stripMargin

  def spec = suite("a durable build cache")(
    test("a second process unpacks, starts no compiler, and prints nothing") {
      ZIO.scoped {
        for
          root <- Workspace
            .work("histos-build-cache-law")
            .foldZIO(failure => ZIO.dieMessage(failure.message), ZIO.succeed)
          classpath <- ZIO.fromOption(compileClasspath).orElse(ZIO.dieMessage("scala library is not on this classpath"))
          source    <- write(root.resolve("src").resolve("Quiet.scala"), Source)
          disk      <- directory(root.resolve("store"))
          out1      <- directory(root.resolve("out-1"))
          out2      <- directory(root.resolve("out-2"))
          before    <- TestConsole.outputErr
          firstStart  <- Ref.make(0)
          first       <- compile(disk, firstStart, source, classpath, out1).either
          starts1     <- firstStart.get
          said        <- TestConsole.outputErr
          secondStart <- Ref.make(0)
          second      <- compile(disk, secondStart, source, classpath, out2).either
          starts2     <- secondStart.get
          quiet       <- TestConsole.outputErr
        yield assertTrue(
          first == Right(()),
          starts1 == 1,
          said.drop(before.length) == Vector("compiling Quiet.scala\n"),
          second == Right(()),
          starts2 == 0,
          quiet == said,
        )
      }
    }
  )

  private def compile(
      disk: Path,
      starts: Ref[Int],
      source: Path,
      classpath: List[Path],
      out: Path,
  ): ZIO[Scope, LoadError, Unit] =
    BuildCache
      .compile(List(source), classpath, out, "law")
      .provideSomeLayer[Scope](
        ZLayer.succeed(ContentStore.in(disk)) ++ ZLayer.succeed(CompilerStarts(starts))
      )

  private def directory(path: Path): ZIO[Any, Nothing, Path] =
    ZIO.attemptBlocking(Files.createDirectories(path)).orDie

  private def write(path: Path, text: String): ZIO[Any, Nothing, Path] =
    ZIO.attemptBlocking {
      Files.createDirectories(path.getParent)
      Files.writeString(path, text, StandardCharsets.UTF_8)
      path
    }.orDie

  private def compileClasspath: Option[List[Path]] =
    val library = codeSource(classOf[scala.runtime.BoxesRunTime])
    val scala3  = codeSource(classOf[scala.deriving.Mirror])
    (library, scala3) match
      case (Some(first), Some(second)) if first == second => Some(List(first))
      case (Some(first), Some(second))                    => Some(List(first, second))
      case _                                              => None

  private def codeSource(cls: Class[?]): Option[Path] =
    Option(cls.getProtectionDomain)
      .flatMap(domain => Option(domain.getCodeSource))
      .flatMap(source => Option(source.getLocation))
      .flatMap(url => Try(Path.of(url.toURI)).toOption)
      .filter(Files.exists(_))
end BuildCacheLawSpec
