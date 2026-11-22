package histos.supply

import histos.version.CommitSha

import java.nio.file.Files

import zio.{IO, Ref, ZIO}
import zio.test.*

object PullRequestSpec extends ZIOSpecDefault:
  private val main     = CommitSha.trusted("f" * 40)
  private val weekly   = BranchName.trusted("histos/deps")
  private val security = BranchName.trusted("histos/security")
  private val repo     = RepoName.trusted("early-effect/histos")
  private val commit   = CommitSha.trusted("a" * 40)

  def spec = suite("deps pull request")(
    test("a missing weekly branch is created, then the commit lands on that commit") {
      exercise(weekly, created(weekly)).map: (result, sent) =>
        val text = result.fold(_.message, identity)
        assertTrue(
          result.isRight,
          sent.map(_.method) == List("POST", "POST", "GET", "POST"),
          sent.head.url.endsWith("/git/refs"),
          sent.head.body.contains("refs/heads/histos/deps"),
          !sent.exists(_.url.contains("/git/refs/heads/")),
          sent(1).body.contains(""""expectedHeadOid":"""" + main.value),
          sent(2).url.contains("head=early-effect:histos%2Fdeps"),
          sent(3).body.contains(""""head":"histos/deps""""),
          text ==
            s"POST https://api.github.com/repos/early-effect/histos/git/refs\ncommit ${commit.value}\nopened\n",
        )
    },
    test("a missing security branch is created, then its pull request names that branch") {
      exercise(security, created(security)).map: (result, sent) =>
        val text = result.fold(_.message, identity)
        assertTrue(
          result.isRight,
          sent.head.body.contains("refs/heads/histos/security"),
          sent(1).body.contains(""""branchName":"histos/security""""),
          sent(2).url.contains("head=early-effect:histos%2Fsecurity"),
          sent(3).body.contains(""""head":"histos/security""""),
          text.contains("opened"),
        )
    },
    test("a path or a longer remote name is not the branch tip") {
      ZIO.scoped:
        for
          repo <- ZIO.acquireRelease(ZIO.attemptBlocking(Files.createTempDirectory("histos-tip")).orDie)(path =>
            ZIO.attemptBlocking(deleteTree(path)).orDie
          )
          _ <- command(repo, "git", "init", "-q") *>
            command(repo, "git", "config", "user.email", "deps@example.com") *>
            command(repo, "git", "config", "user.name", "deps") *>
            ZIO.attemptBlocking(Files.createDirectories(repo.resolve("histos"))).orDie *>
            command(repo, "git", "add", "histos") *>
            command(repo, "git", "commit", "-q", "--allow-empty", "-m", "path")
          head <- command(repo, "git", "rev-parse", "HEAD")
          _    <- command(
            repo,
            "git",
            "update-ref",
            "refs/remotes/origin/histos/deps-longer",
            head.trim,
          )
          found <- ZIO.serviceWithZIO[GitRead](_.tip(repo, weekly))
        yield assertTrue(found.isEmpty, head.trim.nonEmpty)
    }.provide(GitRead.live).mapError(message => SupplyFault.Refused(message, 1)),
    test("a refused branch create fails before a commit") {
      exercise(weekly, List(Reply(422, """{"message":"Reference already exists"}"""))).map: (result, sent) =>
        assertTrue(
          result == Left(
            SupplyFault.Refused(
              "POST https://api.github.com/repos/early-effect/histos/git/refs",
              422,
            )
          ),
          sent.length == 1,
        )
    },
  )

  private def created(branch: BranchName): List[Reply] =
    List(
      Reply(201, s"""{"ref":"refs/heads/${branch.value}"}"""),
      Reply(200, s"""{"data":{"createCommitOnBranch":{"commit":{"oid":"${commit.value}"}}}}"""),
      Reply(200, """[]"""),
      Reply(201, """{"number":7}"""),
    )

  private def exercise(
      branch: BranchName,
      replies: List[Reply],
  ): ZIO[Any, Nothing, (Either[SupplyFault, String], List[Exchange])] =
    for
      rest <- Ref.make(replies)
      sent <- Ref.make(List.empty[Exchange])
      net = script(rest, sent)
      text <- GhRun
        .execute(
          net,
          repo,
          "token",
          List(
            GhCall.UpdateRef(branch, main, create = true),
            GhCall.CommitOnBranch(branch, main, "deps: Libraries", List("dependencies.lock" -> "lock")),
            GhCall.OpenOrUpdate(branch, PrFlow.title, "Libraries"),
          ),
          dryRun = false,
        )
        .either
      calls <- sent.get
    yield (text, calls)

  private def command(repo: java.nio.file.Path, args: String*): ZIO[Any, String, String] =
    ZIO
      .attemptBlocking:
        val process = new ProcessBuilder(args*).directory(repo.toFile).redirectErrorStream(true).start()
        val text    = String(process.getInputStream.readAllBytes, java.nio.charset.StandardCharsets.UTF_8)
        (process.waitFor(), text)
      .mapError(_.getMessage)
      .flatMap: (code, text) =>
        if code == 0 then ZIO.succeed(text) else ZIO.fail(s"${args.mkString(" ")} exited $code")

  private def deleteTree(root: java.nio.file.Path): Unit =
    if Files.exists(root) then Files.walk(root).sorted(java.util.Comparator.reverseOrder()).forEach(Files.delete)

  private def script(rest: Ref[List[Reply]], sent: Ref[List[Exchange]]): Net =
    new Net:
      def send(
          method: String,
          url: String,
          body: String,
          headers: List[(String, String)],
      ): IO[SupplyFault, Reply] =
        sent.update(_ :+ Exchange(method, url, body)) *>
          rest
            .modify:
              case reply :: tail => (Some(reply), tail)
              case Nil           => (None, Nil)
            .flatMap:
              case Some(reply) => ZIO.succeed(reply)
              case None        =>
                ZIO.fail(SupplyFault.ServiceDown(SourceName.trusted("script"), OutageStrategy.FailClosed))
end PullRequestSpec
