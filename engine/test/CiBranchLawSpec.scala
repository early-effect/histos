package histos.workflow

import histos.publish.LawGit
import histos.workspace.Workspace

import zio.ZIO
import zio.test.{ZIOSpecDefault, assertTrue, suite, test}

/** The default branch is the remote HEAD. A clone recorded it, a bare remote is asked, and a non-repository has none.
  */
object CiBranchLawSpec extends ZIOSpecDefault:
  def spec = suite("ci branch laws")(
    test("a clone reads the HEAD it recorded, a remote it never fetched is asked, and a non-repository has none"):
      ZIO.scoped:
        Workspace
          .work("histos-ci-branch")
          .flatMap: root =>
            val cloned = root.resolve("cloned")
            val added  = root.resolve("added")
            val plain  = root.resolve("plain")
            for
              origin   <- LawGit.origin(root, "trunk")
              _        <- LawGit.git(root, "clone", origin.toString, cloned.toString)
              _        <- Workspace.createDirectories(added)
              _        <- LawGit.git(added, "init", "-b", "other")
              _        <- LawGit.git(added, "remote", "add", "origin", origin.toString)
              _        <- Workspace.createDirectories(plain)
              recorded <- CiBranch.read(cloned).either
              asked    <- CiBranch.read(added).either
              missing  <- CiBranch.read(plain).either
            yield assertTrue(
              recorded == Right(CiBranch("trunk")),
              asked == Right(CiBranch("trunk")),
              missing == Left(CiError.NoDefaultBranch),
            )
    ,
    test("a symref line names the branch, and HEAD or no ref is none"):
      assertTrue(
        CiBranch.headFromLsRemote("ref: refs/heads/main\tHEAD\n") == Some("main"),
        CiBranch.headFromLsRemote("ref: refs/heads/HEAD\tHEAD") == None,
        CiBranch.headFromLsRemote("no ref here") == None,
      ),
  )
