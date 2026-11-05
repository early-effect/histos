package histos.workflow

import histos.plugin.{ActionName, ActionPin, RepoPath}
import histos.supply.SupplyFault
import histos.version.CommitSha
import histos.ProjectGraph

import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import zio.{IO, ZIO}

/** Offline scan of every workflow and composite action. Local `./` and `docker://...@sha256:` are allowed. */
object PinScan:
  def check(repo: Path, graph: ProjectGraph): IO[CiError, Unit] =
    faults(repo, graph) match
      case Nil   => ZIO.unit
      case found => ZIO.fail(CiError.Pins(report(found)))

  def faults(repo: Path, graph: ProjectGraph): List[SupplyFault] =
    val pins = ActionPins.catalog(graph.supply.extraActions)
    files(repo).flatMap(file => scan(repo, file, pins))

  def report(faults: List[SupplyFault]): String =
    val body  = faults.map(fault => s"workflow: ${fault.message}")
    val count = faults.length
    val noun  = if count == 1 then "1 pin fault" else s"$count pin faults"
    (body :+ s"$noun. Fix with './histos workflow --update' or add the action to SupplyChain.extraActions.")
      .mkString("\n")

  private def files(repo: Path): List[Path] =
    val workflows = list(repo.resolve(".github").resolve("workflows"), ".yml")
    val actions   = walk(repo.resolve(".github").resolve("actions")).filter(_.getFileName.toString == "action.yml")
    workflows ++ actions

  private def list(dir: Path, suffix: String): List[Path] =
    if !Files.isDirectory(dir) then Nil
    else
      val stream = Files.list(dir)
      try stream.iterator.asScala.filter(path => path.getFileName.toString.endsWith(suffix)).toList.sortBy(_.toString)
      finally stream.close()

  private def walk(dir: Path): List[Path] =
    if !Files.isDirectory(dir) then Nil
    else
      val stream = Files.walk(dir)
      try stream.iterator.asScala.filter(Files.isRegularFile(_)).toList.sortBy(_.toString)
      finally stream.close()

  private def scan(repo: Path, file: Path, catalog: Map[String, ActionPin]): List[SupplyFault] =
    val relative = repo.relativize(file).toString.replace('\\', '/')
    val path     = RepoPath.from(relative).fold(_ => RepoPath.trusted(relative), identity)
    val lines    = Files.readAllLines(file).asScala.toList
    lines.zipWithIndex.flatMap: (text, index) =>
      useOf(text).flatMap((ref, tag) => fault(path, index + 1, ref, tag, catalog))

  private def useOf(text: String): Option[(String, Option[String])] =
    val trimmed = text.trim
    val marker  = "uses:"
    val at      = trimmed.indexOf(marker)
    if at < 0 then None
    else
      val rest = trimmed.drop(at + marker.length).trim
      val ref  = rest.takeWhile(char => !char.isWhitespace)
      if ref.isEmpty then None
      else
        val after = rest.drop(ref.length).trim
        val tag   =
          if after.startsWith("#") then
            val body = after.drop(1).trim
            if body.isEmpty then None else Some(body.takeWhile(char => !char.isWhitespace))
          else None
        Some((ref, tag))

  private def fault(
      file: RepoPath,
      line: Int,
      ref: String,
      tag: Option[String],
      catalog: Map[String, ActionPin],
  ): Option[SupplyFault] =
    if ref.startsWith("./") then None
    else if ref.startsWith("docker://") && ref.contains("@sha256:") then None
    else
      val hash = ref.indexOf('@')
      if hash < 0 then Some(SupplyFault.UnpinnedUses(file, line, ref))
      else
        val name = ref.take(hash)
        val pin  = ref.drop(hash + 1)
        if !sha(pin) then Some(SupplyFault.UnpinnedUses(file, line, ref))
        else
          val action = ActionName.from(name).fold(_ => ActionName.trusted(name), identity)
          tag match
            case None      => Some(SupplyFault.PinWithoutTag(file, line, action))
            case Some(tag) => mismatch(file, line, action, pin, tag, catalog)

  private def mismatch(
      file: RepoPath,
      line: Int,
      action: ActionName,
      pin: String,
      tag: String,
      catalog: Map[String, ActionPin],
  ): Option[SupplyFault] =
    ActionPins.lookup(catalog, action.value) match
      case None           => None
      case Some(expected) =>
        val found = ActionPin(CommitSha.trusted(pin), tag)
        if found == expected then None else Some(SupplyFault.PinMismatch(file, line, action, found, expected))

  private def sha(raw: String): Boolean =
    raw.length == 40 && raw.forall(char => char.isDigit || (char >= 'a' && char <= 'f'))
end PinScan
