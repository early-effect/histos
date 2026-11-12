package histos.native

import histos.ClangMajor
import histos.workspace.{IoFailure, Workspace}

import java.nio.file.attribute.PosixFilePermission
import java.nio.file.{Files, Path}

import zio.ZIO
import zio.test.{ZIOSpecDefault, assertTrue, suite, test}

/** clang and clang++ are one compiler. A missing pair, a bad exit, and the wrong major are refused. Every clang here is
  * a script in the law's directory, never the host's.
  */
object ClangLawSpec extends ZIOSpecDefault:
  def spec = suite("clang laws")(
    test("a scripted pair is found and resolves at its major, and a pair that does not agree is refused"):
      Workspace
        .work("histos-clang")
        .flatMap: dir =>
          val pair = dir.resolve("pair")
          val skew = dir.resolve("skew")
          val dead = dir.resolve("dead")
          val mute = dir.resolve("mute")
          val lone = dir.resolve("lone")
          val on   = Map("PATH" -> pair.toString)
          for
            _       <- script(pair.resolve("clang"), version("16.0.0"))
            _       <- script(pair.resolve("clang++"), version("16.0.0"))
            paired  <- ClangProbe.found(on)
            agreed  <- ClangProbe.resolve(ClangMajor(16), on)
            wrong   <- ClangProbe.resolve(ClangMajor(17), on).either
            nowhere <- ClangProbe.resolve(ClangMajor(16), Map.empty).either
            absent  <- ClangProbe.found(Map.empty).either
            bad     <- ClangProbe.found(Map("PATH" -> "\u0000")).either
            _       <- script(skew.resolve("clang"), version("16.0.0"))
            _       <- script(skew.resolve("clang++"), version("17.0.0"))
            skewed  <- ClangProbe.found(Map("PATH" -> skew.toString)).either
            _       <- script(dead.resolve("clang"), "#!/bin/sh\nexit 2\n")
            _       <- script(dead.resolve("clang++"), "#!/bin/sh\nexit 2\n")
            failed  <- ClangProbe.found(Map("PATH" -> dead.toString)).either
            _       <- script(mute.resolve("clang"), "#!/bin/sh\nprintf '%s\\n' nope\n")
            _       <- script(mute.resolve("clang++"), "#!/bin/sh\nprintf '%s\\n' nope\n")
            quiet   <- ClangProbe.found(Map("PATH" -> mute.toString)).either
            _       <- script(lone.resolve("clang"), version("16.0.0"))
            alone   <- ClangProbe.found(Map("PATH" -> lone.toString)).either
          yield assertTrue(
            paired.clang == pair.resolve("clang"),
            paired.clangpp == pair.resolve("clang++"),
            agreed == paired,
            wrong == Left(ClangMiss.WrongMajor(paired.version, ClangMajor(17), pair.resolve("clang"))),
            nowhere == Left(ClangMiss.NotOnPath(ClangMajor(16))),
            absent == Left(ClangMiss.Absent),
            refused(bad)(isBadPath),
            paired.version.value == "16.0.0",
            paired.target.value == "arm64-apple-darwin",
            refused(skewed)(isMismatch),
            refused(failed)(isStatus(2)),
            refused(quiet)(isNoVersion),
            refused(alone)(isNoCxx),
            ClangReport.parse("") == Left(ClangText.Empty),
            ClangReport.parse("Target: arm64-apple-darwin") == Left(ClangText.NoVersion),
            ClangReport.parse("clang version 16.0.0") == Left(ClangText.NoTarget),
            ClangReport.parse("clang version nope\nTarget: arm64-apple-darwin") == Left(ClangText.BadVersion("nope")),
            ClangReport.parse("clang version 16.0.0\nTarget: arm64-apple-darwin") == Right(
              ClangReport(paired.version, paired.target)
            ),
          )
  )

  private def refused(found: Either[ClangMiss, Clang])(accept: ClangMiss => Boolean): Boolean =
    found match
      case Left(error) => accept(error)
      case Right(_)    => false

  private def isBadPath(error: ClangMiss): Boolean =
    error match
      case ClangMiss.BadPath(entry) => entry == "\u0000"
      case _                        => false

  private def isMismatch(error: ClangMiss): Boolean =
    error match
      case ClangMiss.Mismatched(clang, clangpp) =>
        clang.value == "16.0.0" && clangpp.value == "17.0.0"
      case _ => false

  private def isStatus(code: Int)(error: ClangMiss): Boolean =
    error match
      case ClangMiss.Status(_, status) => status == code
      case _                           => false

  private def isNoVersion(error: ClangMiss): Boolean =
    error match
      case ClangMiss.Text(_, ClangText.NoVersion) => true
      case _                                      => false

  private def isNoCxx(error: ClangMiss): Boolean =
    error match
      case ClangMiss.NoClangPP(_) => true
      case _                      => false

  private def version(token: String): String =
    s"#!/bin/sh\nprintf '%s\\n' 'clang version $token' 'Target: arm64-apple-darwin'\n"

  private def script(path: Path, text: String): ZIO[Any, Nothing, Unit] =
    (Workspace.createDirectories(parentOf(path)) *> Workspace.writeString(path, text) *> ZIO.attemptBlocking {
      Files.setPosixFilePermissions(
        path,
        java.util.Set.of(
          PosixFilePermission.OWNER_READ,
          PosixFilePermission.OWNER_WRITE,
          PosixFilePermission.OWNER_EXECUTE,
        ),
      )
      ()
    }).catchAll(die)

  private def die(error: IoFailure | Throwable): ZIO[Any, Nothing, Nothing] =
    error match
      case failure: IoFailure => ZIO.dieMessage(failure.message)
      case thrown: Throwable  => ZIO.die(thrown)

  private def parentOf(path: Path): Path =
    Option(path.getParent).getOrElse(path)
end ClangLawSpec
