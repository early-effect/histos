package histos.doctor

import histos.supply.{Net, Reply, RepoName, Scan}

import zio.{UIO, ZIO}

/** Pages Source and the github-pages environment, read with the caller's token. */
object PagesProbe:
  enum Source:
    case Actions
    case Other

  enum Gate:
    case Open
    case Blocks(branch: String)
    case Unread

  enum Hit:
    case Absent
    case Unreachable
    case Ready(source: Source, gate: Gate)

  def read(net: Net, repo: RepoName, branch: String, token: String): UIO[Hit] =
    val root    = s"https://api.github.com/repos/${repo.value}"
    val headers = List(
      "Authorization" -> s"Bearer $token",
      "Accept"        -> "application/vnd.github+json",
      "User-Agent"    -> "histos",
    )
    get(net, s"$root/pages", headers).flatMap:
      case None => ZIO.succeed(Hit.Unreachable)
      case Some(pages) if pages.status == 401 || pages.status == 403 => ZIO.succeed(Hit.Unreachable)
      case Some(pages) =>
        val source =
          if pages.status == 200 && Scan.strings(pages.body, "build_type").contains("workflow") then Source.Actions
          else Source.Other
        gate(net, root, branch, headers).map(found => Hit.Ready(source, found))

  private def gate(net: Net, root: String, branch: String, headers: List[(String, String)]): UIO[Gate] =
    get(net, s"$root/environments/github-pages", headers).flatMap:
      case None => ZIO.succeed(Gate.Unread)
      case Some(reply) if reply.status == 404 => ZIO.succeed(Gate.Open)
      case Some(reply) if reply.status != 200 => ZIO.succeed(Gate.Unread)
      case Some(reply) =>
        val body = reply.body
        val nullPolicy =
          body.contains("\"deployment_branch_policy\":null") || body.contains("\"deployment_branch_policy\": null")
        if nullPolicy then ZIO.succeed(Gate.Open)
        else if Scan.bool(body, "custom_branch_policies").contains(true) then policies(net, root, branch, headers)
        else if Scan.bool(body, "protected_branches").contains(true) then protectedBranch(net, root, branch, headers)
        else ZIO.succeed(Gate.Open)

  private def policies(net: Net, root: String, branch: String, headers: List[(String, String)]): UIO[Gate] =
    get(net, s"$root/environments/github-pages/deployment-branch-policies", headers).map:
      case Some(reply) if reply.status == 200 =>
        if names(reply.body).contains(branch) then Gate.Open else Gate.Blocks(branch)
      case _ => Gate.Unread

  private def protectedBranch(net: Net, root: String, branch: String, headers: List[(String, String)]): UIO[Gate] =
    get(net, s"$root/branches/$branch", headers).map:
      case Some(reply) if reply.status == 200 =>
        if Scan.bool(reply.body, "protected").contains(true) then Gate.Open else Gate.Blocks(branch)
      case _ => Gate.Unread

  private def names(body: String): List[String] =
    val mark = "\"branch_policies\":"
    val at   = body.indexOf(mark)
    if at < 0 then Nil
    else
      val slice = body.substring(at + mark.length)
      val end   = slice.indexOf(']')
      if end < 0 then Nil else Scan.strings(slice.substring(0, end), "name")

  private def get(net: Net, url: String, headers: List[(String, String)]): UIO[Option[Reply]] =
    net.send("GET", url, "", headers).map(Some(_)).catchAll(_ => ZIO.succeed(None))
end PagesProbe
