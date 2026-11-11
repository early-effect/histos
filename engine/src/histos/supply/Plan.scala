package histos.supply

import histos.plugin.ActionName
import histos.version.CommitSha
import histos.workflow.ActionPins

import java.net.URLEncoder
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.time.{Instant, LocalDate, ZoneOffset}

import scala.jdk.CollectionConverters.*

import zio.{IO, ZIO}

/** A direct dependency plus the published module the Maven index is queried with. */
final case class IndexedDirect(
    coordinate: Coordinate,
    version: String,
    module: String,
    purl: Purl,
    cell: String,
    testOnly: Boolean,
)

final case class ActionChange(
    name: String,
    fromTag: String,
    toTag: String,
    fromSha: String,
    sha: String,
    listed: Boolean,
)

final case class Proposal(text: String, libraries: BumpPlan, actions: List[ActionChange])

/** Release indexes and the file edits a plan applies. */
object Plan:
  def directs(lock: String): List[IndexedDirect] =
    val lines = lock.split('\n').toList.map(_.trim).filter(_.nonEmpty)
    lines
      .foldLeft((false, "jvm", Option.empty[(String, String, String)], List.empty[IndexedDirect])):
        case ((_, cell, pending, rows), line) if line.startsWith("cell ") =>
          (false, line.split(" ").lift(1).getOrElse(cell), pending, rows)
        case ((_, cell, pending, rows), line) if line == "test" || line.startsWith("test ") =>
          (true, cell, pending, rows)
        case ((testOnly, cell, _, rows), line) if line.startsWith("direct ") =>
          val parts = line.split(" ").toList
          parts match
            case _ :: org :: name :: version :: _ => (testOnly, cell, Some((org, name, version)), rows)
            case _                                => (testOnly, cell, None, rows)
        case ((testOnly, cell, Some((org, name, version)), rows), line) if line.startsWith("artifact ") =>
          val parts = line.split(" ").toList
          parts match
            case _ :: artifactOrg :: module :: artifactVersion :: _
                if artifactOrg == org && artifactVersion == version =>
              val row = IndexedDirect(
                Coordinate.trusted(s"$org:$name"),
                version,
                module,
                Purl.trusted(s"pkg:maven/$org/$module@$version"),
                cell,
                testOnly,
              )
              (testOnly, cell, None, rows :+ row)
            case _ => (testOnly, cell, None, rows)
        case (state, _) => state
      ._4
      .distinctBy(_.coordinate.value)

  /** A lock direct the graph names as a root `dep"..."` literal. A `.module` member is not one. */
  def roots(lock: String, graph: String): List[IndexedDirect] =
    directs(lock).filter(row => literal(graph, row))

  def mavenUrl(group: String, artifact: String): String =
    val query = URLEncoder.encode(s"""g:"$group" AND a:"$artifact"""", StandardCharsets.UTF_8)
    s"https://search.maven.org/solrsearch/select?q=$query&rows=50&wt=json&core=gav"

  def releases(coordinate: Coordinate, body: String, security: Map[String, AdvisoryId]): List[Release] =
    Scan
      .strings(body, "v")
      .zip(Scan.longs(body, "timestamp"))
      .map: (version, stamp) =>
        val day = Instant.ofEpochMilli(stamp).atZone(ZoneOffset.UTC).toLocalDate.toString
        Release(
          coordinate,
          version,
          Date.trusted(day),
          security.get(version),
          version.exists(char => char == '-' || char == '+'),
        )

  def tags(body: String): List[(String, String)] =
    Scan
      .strings(body, "name")
      .zip(Scan.strings(body, "sha"))
      .filter: (name, sha) =>
        semver(name) && sha.length == 40

  def repoId(body: String): Option[RepoId] =
    Scan.longs(body, "id").headOption.map(id => RepoId.trusted(id.toString))

  def commitDay(body: String): Option[Date] =
    Scan
      .strings(body, "date")
      .headOption
      .flatMap: raw =>
        val day = raw.take(10)
        if day.length == 10 && day.charAt(4) == '-' && day.charAt(7) == '-' then Some(Date.trusted(day)) else None

  /** Fixed versions named by an OSV batch body, keyed by the purl they follow. */
  def fixedByPurl(body: String): Map[String, List[(String, String)]] =
    val ids   = Scan.strings(body, "id").filter(_.startsWith("GHSA-"))
    val purls = Scan.strings(body, "purl")
    val fixed = Scan.strings(body, "fixed")
    val id    = ids.headOption.getOrElse("")
    purls.map(purl => purl -> fixed.map(version => (id, version))).toMap

  def propose(
      lock: String,
      graph: String,
      catalog: Map[String, (String, String)],
      policy: SupplyChain,
      today: Date,
      security: Boolean,
      net: Net,
      knownRepos: Map[String, RepoId],
  ): IO[SupplyFault, Proposal] =
    val rows = roots(lock, graph)
    for
      fetched <- ZIO.foreach(rows)(row => fetchMaven(row, security, net))
      actions <- ZIO.foreach(catalog.toList):
        case (name, pin) => fetchAction(name, pin, policy, today, net, knownRepos)
      failed = actions.collect { case Left(fault) => fault }
      _      <- ZIO.foreachDiscard(failed.headOption)(ZIO.fail(_))
      chosen <- ZIO.fromEither:
        val releases = fetched.flatMap(_._2)
        val locked   = rows.map: row =>
          LockedDependency(row.coordinate, row.version, row.purl, true, row.testOnly, row.cell)
        Bump.choose(locked, releases, policy.updates.versions, today, security, policy.transitive)
    yield
      val marks      = fetched.flatMap(_._1).toMap
      val actionPlan = actions.collect { case Right(Some(change)) => change }
      Proposal(render(chosen, actionPlan, Nil, marks), chosen, actionPlan)

  def render(
      libraries: BumpPlan,
      actions: List[ActionChange],
      failed: List[SupplyFault],
      notes: Map[String, String],
  ): String =
    val libraryLines =
      libraries.apply.map(bump =>
        s"apply ${bump.kind} ${bump.from.coordinate.value} ${bump.from.version} -> ${bump.to.version}"
      ) ++
        libraries.listed.map(bump =>
          s"listed ${bump.kind} ${bump.from.coordinate.value} ${bump.from.version} -> ${bump.to.version}"
        )
    val actionLines = actions
      .filterNot(change => change.fromTag == change.toTag && change.fromSha == change.sha)
      .map: change =>
        val verb = if change.listed then "listed" else "apply"
        s"$verb Actions ${change.name} ${change.fromTag} -> ${change.toTag} ${change.sha}"
    val faults = failed.map(fault => s"fault ${fault.message}")
    val extra  = notes.toList.map((key, value) => s"note $key $value")
    (List("deps plan") ++ libraryLines ++ actionLines ++ faults ++ extra).mkString("\n") + "\n"

  def write(root: Path, proposal: Proposal, histos: String, workflows: List[(Path, String)]): List[String] =
    val nextHistos = proposal.libraries.apply.foldLeft(histos)(rewriteDep)
    val written    =
      if nextHistos == histos then Nil
      else
        Files.writeString(root.resolve("histos.scala"), nextHistos)
        List("histos.scala")
    val actionFiles = workflows.flatMap: (path, text) =>
      val next = proposal.actions.filterNot(_.listed).foldLeft(text)(rewriteAction)
      if next == text then Nil
      else
        Files.writeString(path, next)
        List(root.relativize(path).toString)
    written ++ actionFiles

  def workflowFiles(root: Path): List[Path] =
    val workflows = root.resolve(".github").resolve("workflows")
    val actions   = root.resolve(".github").resolve("actions")
    val pins = root.resolve("engine").resolve("src").resolve("histos").resolve("workflow").resolve("ActionPins.scala")
    val yml  =
      if Files.isDirectory(workflows) then
        Files.list(workflows).iterator.asScala.filter(_.toString.endsWith(".yml")).toList
      else Nil
    val nested =
      if Files.isDirectory(actions) then
        Files.walk(actions).iterator.asScala.filter(_.getFileName.toString == "action.yml").toList
      else Nil
    val catalog = if Files.isRegularFile(pins) then List(pins) else Nil
    yml ++ nested ++ catalog

  def rewriteDep(source: String, bump: ChosenBump): String =
    val parts = bump.from.coordinate.value.split(":", 2)
    parts match
      case Array(group, name) =>
        source
          .replace(s"$group::$name:${bump.from.version}", s"$group::$name:${bump.to.version}")
          .replace(s"$group:$name:${bump.from.version}", s"$group:$name:${bump.to.version}")
      case _ => source

  def rewriteAction(source: String, change: ActionChange): String =
    source
      .replace(s"${change.name}@${change.fromSha}", s"${change.name}@${change.sha}")
      .replace(s"# ${change.fromTag}", s"# ${change.toTag}")
      .replace(s""""${change.fromSha}", "${change.fromTag}"""", s""""${change.sha}", "${change.toTag}"""")

  /** `:::` then `::` then `:`. A shorter revision is not a match for a longer one. */
  private def literal(graph: String, row: IndexedDirect): Boolean =
    val org  = row.coordinate.value.takeWhile(_ != ':')
    val name = row.coordinate.value.drop(org.length + 1)
    List(s"$org:::$name:${row.version}", s"$org::$name:${row.version}", s"$org:$name:${row.version}")
      .exists(spell => occurs(graph, spell))

  private def occurs(graph: String, spell: String): Boolean =
    def loop(from: Int): Boolean =
      val at = graph.indexOf(spell, from)
      if at < 0 then false
      else
        val end = at + spell.length
        if end == graph.length || !revisionContinues(graph.charAt(end)) then true
        else loop(at + 1)
    loop(0)

  private def revisionContinues(char: Char): Boolean =
    char.isLetterOrDigit || char == '.' || char == '-' || char == '+'

  private def fetchMaven(
      row: IndexedDirect,
      security: Boolean,
      net: Net,
  ): IO[SupplyFault, (Map[String, String], List[Release])] =
    val group = row.coordinate.value.takeWhile(_ != ':')
    for
      page  <- get(net, mavenUrl(group, row.module), "maven")
      marks <-
        if security then advisoryMarks(row, net)
        else ZIO.succeed(Map.empty[String, AdvisoryId])
    yield (Map(row.coordinate.value -> row.module), releases(row.coordinate, page.body, marks))

  private def advisoryMarks(row: IndexedDirect, net: Net): IO[SupplyFault, Map[String, AdvisoryId]] =
    val payload = s"""{"queries":[{"package":{"purl":"${row.purl.value}"}}]}"""
    get(net, "https://api.osv.dev/v1/querybatch", "osv", "POST", payload).map: reply =>
      fixedByPurl(reply.body)
        .getOrElse(row.purl.value, Nil)
        .flatMap: (id, version) =>
          if AdvisoryId.from(id).isRight then Some(version -> AdvisoryId.trusted(id)) else None
        .toMap

  private def fetchAction(
      name: String,
      pin: (String, String),
      policy: SupplyChain,
      today: Date,
      net: Net,
      knownRepos: Map[String, RepoId],
  ): IO[SupplyFault, Either[SupplyFault, Option[ActionChange]]] =
    val (sha, tag) = pin
    val owner      = name.split("/").take(2).mkString("/")
    val tagsUrl    = s"https://api.github.com/repos/$owner/tags?per_page=30"
    val repoUrl    = s"https://api.github.com/repos/$owner"
    for
      listed <- get(net, tagsUrl, "github")
      repo   <- get(net, repoUrl, "github")
      dated  <- ZIO.foreach(tags(listed.body)):
        case (tagName, tagSha) => datedTag(owner, tagName, tagSha, net)
    yield
      val now = repoId(repo.body).getOrElse(RepoId.trusted("0"))
      val was = knownRepos.getOrElse(owner, now)
      if was != now then Left(SupplyFault.ActionRepoMoved(ActionName.trusted(name), was, now))
      else
        Right:
          chooseAction(name, tag, sha, dated.flatten, policy, today).flatMap: change =>
            CommitSha.from(change.sha) match
              case Right(commit) =>
                Bump.verify(ActionName.trusted(name), change.toTag, commit, true, was, now).map(_ => change).toOption
              case Left(_) => None

  private final case class Dated(tag: String, sha: CommitSha, published: Date)

  private def datedTag(owner: String, tag: String, sha: String, net: Net): IO[SupplyFault, Option[Dated]] =
    CommitSha.from(sha) match
      case Left(_)       => ZIO.succeed(None)
      case Right(commit) =>
        get(net, s"https://api.github.com/repos/$owner/commits/$sha", "github").map: reply =>
          commitDay(reply.body).map(day => Dated(tag, commit, day))

  private def chooseAction(
      name: String,
      currentTag: String,
      currentSha: String,
      dated: List[Dated],
      policy: SupplyChain,
      today: Date,
  ): Option[ActionChange] =
    val coordinate = Coordinate.trusted(s"act:${name.replace('/', '-')}")
    val locked     = LockedDependency(
      coordinate,
      strip(currentTag),
      Purl.trusted(s"pkg:github/$name@$currentTag"),
      true,
      false,
      "jvm",
    )
    val releases = dated.map: item =>
      Release(coordinate, strip(item.tag), item.published, None, item.tag.contains("-"))
    Bump
      .choose(List(locked), releases, policy.updates.versions, today, false, TransitiveFix.ParentOnly)
      .toOption
      .flatMap: plan =>
        val chosen =
          plan.apply.headOption.map(bump => (bump, false)).orElse(plan.listed.headOption.map(bump => (bump, true)))
        chosen.map: (bump, listed) =>
          val nextTag = dated.find(item => strip(item.tag) == bump.to.version).fold(currentTag)(_.tag)
          val nextSha = dated.find(_.tag == nextTag).fold(currentSha)(_.sha.value)
          ActionChange(name, currentTag, nextTag, currentSha, nextSha, listed)

  private def strip(tag: String): String =
    if tag.startsWith("v") then tag.drop(1) else tag

  private def semver(tag: String): Boolean =
    val core = strip(tag).takeWhile(char => char.isDigit || char == '.')
    core.count(_ == '.') >= 1

  private def get(
      net: Net,
      url: String,
      source: String,
      method: String = "GET",
      body: String = "",
  ): IO[SupplyFault, Reply] =
    net
      .send(method, url, body, Nil)
      .mapError(_ => SupplyFault.ServiceDown(SourceName.trusted(source), OutageStrategy.FailClosed))

  def catalog(extra: Map[histos.plugin.ActionName, histos.plugin.ActionPin]): Map[String, (String, String)] =
    ActionPins.catalog(extra).map((name, pin) => name -> (pin.sha.value, pin.tag))
end Plan
