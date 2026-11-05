package histos.supply

import histos.version.CommitSha

import java.util.Base64
import java.nio.charset.StandardCharsets

import zio.{IO, ZIO}

/** Runs a pull-request plan. Each `createCommitOnBranch` expects the oid the previous call returned. */
object GhRun:
  final case class State(head: Option[CommitSha], notes: List[String])

  val graphql: String = "https://api.github.com/graphql"

  def execute(
      net: Net,
      repo: RepoName,
      token: String,
      calls: List[GhCall],
      dryRun: Boolean,
      base: String = "main",
  ): IO[SupplyFault, String] =
    ZIO
      .foldLeft(calls)(State(None, Nil)): (state, call) =>
        step(net, repo, token, state, call, dryRun, base)
      .map(state => state.notes.mkString("\n") + "\n")

  /** The first commit expects the ref we just updated. Every later commit expects the previous commit. */
  def expected(state: State, planned: CommitSha): CommitSha =
    state.head.getOrElse(planned)

  def alert(
      net: Net,
      repo: RepoName,
      token: String,
      policy: SupplyChain,
      findings: List[Finding],
      dryRun: Boolean,
  ): IO[SupplyFault, String] =
    val found = findings.map(_.id).toSet
    for
      issues <-
        policy.advisories.onMain match
          case MainAlert.CheckOnly    => ZIO.succeed("")
          case MainAlert.Issue(label) =>
            issueMoves(net, repo, token, label.value, found, dryRun)
      security <- securityPull(net, repo, token, policy, findings, dryRun)
    yield issues + security

  def snapshot(net: Net, repo: RepoName, token: String, body: String, dryRun: Boolean): IO[SupplyFault, String] =
    val url = s"https://api.github.com/repos/${repo.value}/dependency-graph/snapshots"
    if dryRun then ZIO.succeed(s"dry-run POST $url\n")
    else
      send(net, "POST", url, body, token).map: reply =>
        if reply.status == 200 || reply.status == 201 then s"submitted ${repo.value}\n"
        else reply.body

  private def step(
      net: Net,
      repo: RepoName,
      token: String,
      state: State,
      call: GhCall,
      dryRun: Boolean,
      base: String,
  ): IO[SupplyFault, State] =
    call match
      case GhCall.UpdateRef(branch, sha, create) =>
        val url =
          if create then s"https://api.github.com/repos/${repo.value}/git/refs"
          else s"https://api.github.com/repos/${repo.value}/git/refs/heads/${branch.value}"
        val body =
          if create then s"""{"ref":"refs/heads/${branch.value}","sha":"${sha.value}"}"""
          else s"""{"sha":"${sha.value}","force":true}"""
        val method = if create then "POST" else "PATCH"
        record(net, method, url, body, token, dryRun, state.copy(head = Some(sha)), s"$method $url")
      case GhCall.CommitOnBranch(branch, planned, headline, files) =>
        val expected = GhRun.expected(state, planned)
        val body     = commitBody(repo, branch, expected, headline, files)
        if dryRun then ZIO.succeed(state.copy(notes = state.notes :+ s"commit $headline expected ${expected.value}"))
        else
          send(net, "POST", graphql, body, token).flatMap: reply =>
            if reply.status >= 400 || reply.body.contains(""""errors"""") then
              ZIO.fail(SupplyFault.HeadMoved(branch, expected))
            else
              Scan.strings(reply.body, "oid").headOption.flatMap(oid => CommitSha.from(oid).toOption) match
                case None      => ZIO.fail(SupplyFault.HeadMoved(branch, expected))
                case Some(oid) =>
                  ZIO.succeed(state.copy(head = Some(oid), notes = state.notes :+ s"commit ${oid.value}"))
      case GhCall.OpenOrUpdate(title, body) =>
        if dryRun then ZIO.succeed(state.copy(notes = state.notes :+ s"pull $title"))
        else
          openOrUpdate(net, repo, token, "histos/deps", title, body, base).map(note =>
            state.copy(notes = state.notes :+ note)
          )
      case GhCall.Comment(body) =>
        val url = s"https://api.github.com/repos/${repo.value}/issues/comments"
        record(net, "POST", url, jsonString("body", body), token, dryRun, state, "comment")
      case GhCall.Close(branch) =>
        if dryRun then ZIO.succeed(state.copy(notes = state.notes :+ s"close ${branch.value}"))
        else closePull(net, repo, token, branch).map(note => state.copy(notes = state.notes :+ note))
      case GhCall.Push(branch, signed) =>
        ZIO.succeed(state.copy(notes = state.notes :+ s"push ${branch.value} signed=$signed"))

  private def record(
      net: Net,
      method: String,
      url: String,
      body: String,
      token: String,
      dryRun: Boolean,
      state: State,
      note: String,
  ): IO[SupplyFault, State] =
    if dryRun then ZIO.succeed(state.copy(notes = state.notes :+ s"dry-run $note"))
    else send(net, method, url, body, token).as(state.copy(notes = state.notes :+ note))

  private def openOrUpdate(
      net: Net,
      repo: RepoName,
      token: String,
      head: String,
      title: String,
      body: String,
      base: String,
  ): IO[SupplyFault, String] =
    val url    = s"https://api.github.com/repos/${repo.value}/pulls"
    val listed = s"$url?state=open&head=${repo.value.takeWhile(_ != '/')}:${encode(head)}"
    send(net, "GET", listed, "", token).flatMap: page =>
      val number  = Scan.longs(page.body, "number").headOption
      val payload = s"""{"title":${quote(title)},"body":${quote(body)},"head":${quote(head)},"base":"$base"}"""
      number match
        case Some(existing) =>
          send(net, "PATCH", s"$url/$existing", payload, token).as(s"updated $existing")
        case None =>
          send(net, "POST", url, payload, token).as("opened")

  private def closePull(net: Net, repo: RepoName, token: String, branch: BranchName): IO[SupplyFault, String] =
    val url =
      s"https://api.github.com/repos/${repo.value}/pulls?state=open&head=${repo.value.takeWhile(_ != '/')}:${encode(branch.value)}"
    send(net, "GET", url, "", token).flatMap: page =>
      Scan.longs(page.body, "number").headOption match
        case None         => ZIO.succeed(s"close ${branch.value}: no pull request")
        case Some(number) =>
          send(
            net,
            "PATCH",
            s"https://api.github.com/repos/${repo.value}/pulls/$number",
            """{"state":"closed"}""",
            token,
          ).as(s"close ${branch.value}")

  private def issueMoves(
      net: Net,
      repo: RepoName,
      token: String,
      label: String,
      found: Set[AdvisoryId],
      dryRun: Boolean,
  ): IO[SupplyFault, String] =
    val url = s"https://api.github.com/repos/${repo.value}/issues?labels=$label&state=open"
    if dryRun then ZIO.succeed(s"dry-run issues $label\n")
    else
      send(net, "GET", url, "", token).flatMap: listed =>
        val pairs = Scan
          .longs(listed.body, "number")
          .zip(Scan.strings(listed.body, "title"))
          .flatMap: (number, title) =>
            AdvisoryId.from(title.take(19)).toOption.map(id => id -> number)
        val moves = Alert.issues(pairs.map(_._1).toSet, found)
        ZIO.foreach(moves)(move => applyMove(net, repo, token, label, pairs.toMap, move)).map(_.mkString)

  private def applyMove(
      net: Net,
      repo: RepoName,
      token: String,
      label: String,
      open: Map[AdvisoryId, Long],
      move: Alert.Move,
  ): IO[SupplyFault, String] =
    val url = s"https://api.github.com/repos/${repo.value}/issues"
    move match
      case Alert.Move.Open(id) =>
        val body = s"""{"title":"${id.value}","body":"${id.value}","labels":["$label"]}"""
        send(net, "POST", url, body, token).as(s"open ${id.value}\n")
      case Alert.Move.Update(id) =>
        open.get(id) match
          case Some(number) =>
            send(net, "PATCH", s"$url/$number", s"""{"title":"${id.value}"}""", token).as(s"update ${id.value}\n")
          case None => ZIO.succeed("")
      case Alert.Move.Close(id) =>
        open.get(id) match
          case Some(number) =>
            send(net, "PATCH", s"$url/$number", """{"state":"closed"}""", token).as(s"close ${id.value}\n")
          case None => ZIO.succeed("")

  private def securityPull(
      net: Net,
      repo: RepoName,
      token: String,
      policy: SupplyChain,
      findings: List[Finding],
      dryRun: Boolean,
  ): IO[SupplyFault, String] =
    policy.security match
      case SecurityFixes.OutOfBand(branch, _, _) if findings.exists(_.fixedIn.nonEmpty) =>
        val body = findings.map(_.id.value).mkString(", ")
        if dryRun then ZIO.succeed(s"dry-run security ${branch.value}\n")
        else
          openOrUpdate(net, repo, token, branch.value, "deps: security", body, "main").map(note => s"security $note\n")
      case SecurityFixes.OutOfBand(branch, _, _) =>
        ZIO.succeed(s"security ${branch.value}: nothing to open\n")
      case _ =>
        ZIO.succeed("")

  private def commitBody(
      repo: RepoName,
      branch: BranchName,
      expected: CommitSha,
      headline: String,
      files: List[(String, String)],
  ): String =
    val additions = files.map: (path, contents) =>
      val encoded = Base64.getEncoder.encodeToString(contents.getBytes(StandardCharsets.UTF_8))
      s"""{"path":${quote(path)},"contents":"$encoded"}"""
    val query = "mutation($input:CreateCommitOnBranchInput!){createCommitOnBranch(input:$input){commit{oid}}}"
    s"""{"query":"$query","variables":{"input":{"branch":{"repositoryNameWithOwner":"${repo.value}","branchName":"${branch.value}"},"message":{"headline":${quote(
        headline
      )}},"expectedHeadOid":"${expected.value}","fileChanges":{"additions":[${additions.mkString(",")}]}}}}"""

  private def send(net: Net, method: String, url: String, body: String, token: String): IO[SupplyFault, Reply] =
    val headers = List(
      "Authorization" -> s"Bearer $token",
      "Accept"        -> "application/vnd.github+json",
    )
    net
      .send(method, url, body, headers)
      .mapError(_ => SupplyFault.ServiceDown(SourceName.trusted("github"), OutageStrategy.FailClosed))

  private def encode(text: String): String =
    java.net.URLEncoder.encode(text, StandardCharsets.UTF_8)

  private def quote(text: String): String =
    "\"" + text.replace("\\", "\\\\").replace("\"", "\\\"") + "\""

  private def jsonString(key: String, value: String): String =
    s"""{"$key":${quote(value)}}"""
end GhRun
