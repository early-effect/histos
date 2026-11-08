package histos.workflow

import histos.workspace.Workspace

import java.nio.file.{Files, Path}

import zio.ZIO
import zio.test.{ZIOSpecDefault, assertTrue, suite, test}

/** The default branch is the remote HEAD this checkout recorded. A directory that is not a repository has none. */
object CiBranchLawSpec extends ZIOSpecDefault:
  def spec = suite("ci branch laws")(
    test("origin HEAD is main, a non-repository has none, and a symref line is the branch"):
      Workspace.work("histos-ci-branch").flatMap: dir =>
        val repo = repository
        for
          found   <- CiBranch.read(repo).either
          missing <- CiBranch.read(dir).either
        yield assertTrue(
          found == Right(CiBranch("main")),
          missing == Left(CiError.NoDefaultBranch),
          CiBranch.headFromLsRemote("ref: refs/heads/main\tHEAD\n") == Some("main"),
          CiBranch.headFromLsRemote("ref: refs/heads/HEAD\tHEAD") == None,
          CiBranch.headFromLsRemote("no ref here") == None,
        )
  )

  private def repository: Path =
    def walk(path: Path, left: Int): Option[Path] =
      if left == 0 then None
      else if Files.isRegularFile(path.resolve("dependencies.lock")) && Files.isRegularFile(
          path.resolve("histos.scala")
        )
      then Some(path)
      else
        Option(path.getParent) match
          case Some(parent) => walk(parent, left - 1)
          case None         => None
    walk(Path.of("").toAbsolutePath.normalize, 8).getOrElse(Path.of("").toAbsolutePath.normalize)
end CiBranchLawSpec
