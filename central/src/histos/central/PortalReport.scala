package histos.central

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, IOException}
import java.nio.charset.StandardCharsets
import java.util.zip.{ZipException, ZipInputStream}
import javax.xml.parsers.{DocumentBuilderFactory, ParserConfigurationException}

import org.w3c.dom.Element
import org.xml.sax.SAXException

import zio.Chunk

/** One coordinate inside a bundle. `:` is excluded so a stored line has three fields. */
final case class Gav(group: String, artifact: String, version: String):
  def render: String = s"$group:$artifact:$version"

  def directory: String =
    s"${group.replace('.', '/')}/$artifact/$version"
end Gav

object Gav:
  def from(line: String): Option[Gav] =
    val first  = line.indexOf(':')
    val second = if first < 0 then -1 else line.indexOf(':', first + 1)
    if first <= 0 || second <= first + 1 || second >= line.length - 1 then None
    else Some(Gav(line.substring(0, first), line.substring(first + 1, second), line.substring(second + 1)))

/** Why a bundle is not a release Central would publish. */
enum Finding:
  case Layout(path: String)
  case FileSet(name: String)
  case Checksum(name: String)
  case PomFields(field: String)
  case SnapshotVersion(version: String)
  case SnapshotDependency(coordinate: String)
  case AlreadyPublished(gav: String)
  case EmptyPom
  case TooLarge

  def token: String = this match
    case Layout(path)                   => s"layout:${Finding.sane(path)}"
    case FileSet(name)                  => s"file-set:${Finding.sane(name)}"
    case Checksum(name)                 => s"checksum:${Finding.sane(name)}"
    case PomFields(field)               => s"pom-fields:${Finding.sane(field)}"
    case SnapshotVersion(version)       => s"snapshot-version:${Finding.sane(version)}"
    case SnapshotDependency(coordinate) => s"snapshot-dependency:${Finding.sane(coordinate)}"
    case AlreadyPublished(gav)          => s"already-published:${Finding.sane(gav)}"
    case EmptyPom                       => "empty-pom"
    case TooLarge                       => "too-large"
end Finding

object Finding:
  def from(token: String): Option[Finding] =
    token match
      case "empty-pom" => Some(EmptyPom)
      case "too-large" => Some(TooLarge)
      case other       =>
        val cut = other.indexOf(':')
        if cut <= 0 then None
        else
          val detail = other.substring(cut + 1)
          other.substring(0, cut) match
            case "layout"              => Some(Layout(detail))
            case "file-set"            => Some(FileSet(detail))
            case "checksum"            => Some(Checksum(detail))
            case "pom-fields"          => Some(PomFields(detail))
            case "snapshot-version"    => Some(SnapshotVersion(detail))
            case "snapshot-dependency" => Some(SnapshotDependency(detail))
            case "already-published"   => Some(AlreadyPublished(detail))
            case _                     => None

  /** A report line is one token. A newline in a path would split it. */
  def sane(raw: String): String =
    raw.flatMap:
      case '\n' | '\r' => "_"
      case char        => String.valueOf(char)
end Finding

/** The validation report a deployment carries. `size` is the upload the server measured. */
final case class Report(findings: Chunk[Finding], gavs: Chunk[Gav])

object PortalReport:
  /** Central refuses a bundle larger than this. `Bundle.Cap` is the same bound. */
  val Cap: Long = 1024L * 1024L * 1024L

  def inspect(zip: Chunk[Byte], size: Long, published: Set[Gav]): Report =
    if size > Cap || zip.length.toLong > Cap then Report(Chunk(Finding.TooLarge), Chunk.empty)
    else
      unzip(zip) match
        case Left(findings) => Report(findings, Chunk.empty)
        case Right(files)   => judge(files, published)

  def render(id: DeploymentId, state: DeploymentState, findings: Chunk[Finding]): String =
    val report = findings.map(finding => quote(finding.token)).mkString("[", ",", "]")
    s"""{"deploymentId":${quote(id.value)},"deploymentState":${quote(wire(state))},"report":$report}"""

  def parse(json: String): Option[(DeploymentState, Chunk[Finding])] =
    for
      state <- stateOf(json)
      found <- reportOf(json)
    yield (state, found)

  private def judge(files: Map[String, Chunk[Byte]], published: Set[Gav]): Report =
    val grouped       = files.groupBy((path, _) => directoryOf(path))
    val (found, gavs) = grouped.foldLeft((Chunk.empty[Finding], Chunk.empty[Gav])):
      case ((acc, gavs), ("", entries)) =>
        (acc ++ entries.map((path, _) => Finding.Layout(path)), gavs)
      case ((acc, gavs), (dir, entries)) =>
        val (more, gav) = module(dir, entries)
        (acc ++ more, gav.fold(gavs)(gavs :+ _))
    val taken = gavs.filter(published.contains).map(gav => Finding.AlreadyPublished(gav.render))
    Report(found ++ taken, gavs)

  private def module(dir: String, entries: Map[String, Chunk[Byte]]): (Chunk[Finding], Option[Gav]) =
    val segments = dir.split("/", -1).toList
    segments match
      case group :+ artifact :+ version if group.nonEmpty && group.forall(legal) && legal(artifact) && legal(version) =>
        val gav   = Gav(group.mkString("."), artifact, version)
        val stem  = s"$artifact-$version"
        val names = entries.map((path, bytes) => (fileName(dir, path), bytes))
        val extra = names.collect:
          case (name, _) if !allowed(stem, name) => Finding.Layout(s"$dir/$name")
        val jars = Chunk(
          artifactFile(names, dir, s"$stem.jar", zip = true),
          artifactFile(names, dir, s"$stem-sources.jar", zip = true),
          artifactFile(names, dir, s"$stem-javadoc.jar", zip = true),
        ).flatten
        val (pomFound, _) = pomFile(names, dir, s"$stem.pom", gav)
        val snapshot      =
          if version.endsWith("-SNAPSHOT") then Chunk(Finding.SnapshotVersion(version)) else Chunk.empty
        (Chunk.fromIterable(extra) ++ jars ++ pomFound ++ snapshot, Some(gav))
      case _ =>
        (Chunk(Finding.Layout(dir)), None)

  private def artifactFile(
      files: Map[String, Chunk[Byte]],
      dir: String,
      name: String,
      zip: Boolean,
  ): Chunk[Finding] =
    val path = s"$dir/$name"
    files.get(name) match
      case None =>
        Chunk(Finding.FileSet(path))
      case Some(bytes) if zip && !zipMagic(bytes) =>
        Chunk(Finding.FileSet(path))
      case Some(bytes) =>
        sidecars(files, path, bytes)

  private def pomFile(
      files: Map[String, Chunk[Byte]],
      dir: String,
      name: String,
      gav: Gav,
  ): (Chunk[Finding], Option[Gav]) =
    val path = s"$dir/$name"
    files.get(name) match
      case None =>
        (Chunk(Finding.FileSet(path)), None)
      case Some(bytes) if new String(bytes.toArray, StandardCharsets.UTF_8).trim.isEmpty =>
        (Chunk(Finding.EmptyPom) ++ sidecars(files, path, bytes), Some(gav))
      case Some(bytes) =>
        val fields =
          parsePom(bytes) match
            case None          => Chunk(Finding.PomFields("pom"))
            case Some(project) => pomFields(project, gav)
        (fields ++ sidecars(files, path, bytes), Some(gav))

  private def sidecars(files: Map[String, Chunk[Byte]], path: String, bytes: Chunk[Byte]): Chunk[Finding] =
    Chunk.fromIterable(
      List(
        checksum(files, path, ".md5", Digest.md5(bytes)),
        checksum(files, path, ".sha1", Digest.sha1(bytes)),
        signature(files, path),
      ).flatten
    )

  private def checksum(
      files: Map[String, Chunk[Byte]],
      path: String,
      ext: String,
      expected: String,
  ): Option[Finding] =
    val name = s"$path$ext"
    val file = name.substring(name.lastIndexOf('/') + 1)
    files.get(file) match
      case None                                     => Some(Finding.FileSet(name))
      case Some(data) if hexMatches(data, expected) => None
      case Some(_)                                  => Some(Finding.Checksum(name))

  private def signature(files: Map[String, Chunk[Byte]], path: String): Option[Finding] =
    val name = s"$path.asc"
    val file = name.substring(name.lastIndexOf('/') + 1)
    files.get(file) match
      case Some(data) if data.nonEmpty => None
      case _                           => Some(Finding.FileSet(name))

  private def allowed(stem: String, name: String): Boolean =
    val artifacts = List(s"$stem.jar", s"$stem-sources.jar", s"$stem-javadoc.jar", s"$stem.pom")
    artifacts.exists(artifact =>
      name == artifact || name == s"$artifact.md5" || name == s"$artifact.sha1" || name == s"$artifact.sha256" ||
        name == s"$artifact.asc"
    )

  private def pomFields(project: Element, gav: Gav): Chunk[Finding] =
    val found = List.newBuilder[Finding]
    if one(project, "groupId") != Some(gav.group) then found += Finding.PomFields("groupId")
    if one(project, "artifactId") != Some(gav.artifact) then found += Finding.PomFields("artifactId")
    one(project, "version") match
      case Some(version) if version == gav.version        => ()
      case Some(version) if version.endsWith("-SNAPSHOT") =>
        found += Finding.PomFields("version")
        found += Finding.SnapshotVersion(version)
      case Some(_) => found += Finding.PomFields("version")
      case None    => found += Finding.PomFields("version")
    if one(project, "name").isEmpty then found += Finding.PomFields("name")
    if !licenseOk(project) then found += Finding.PomFields("license")
    if !developerOk(project) then found += Finding.PomFields("developer")
    if !scmOk(project) then found += Finding.PomFields("scm")
    found ++= snapshotDependencies(project)
    Chunk.fromIterable(found.result())

  private def licenseOk(project: Element): Boolean =
    direct(project, "licenses").exists: licenses =>
      direct(licenses, "license").exists: license =>
        one(license, "name").isDefined && one(license, "url").isDefined &&
          one(license, "distribution").contains("repo")

  private def developerOk(project: Element): Boolean =
    direct(project, "developers").exists: developers =>
      direct(developers, "developer").exists: developer =>
        one(developer, "id").isDefined && one(developer, "name").isDefined

  private def scmOk(project: Element): Boolean =
    direct(project, "scm").exists: scm =>
      one(scm, "url").isDefined && one(scm, "connection").isDefined && one(scm, "developerConnection").isDefined

  private def snapshotDependencies(project: Element): List[Finding] =
    val parent = direct(project, "parent").flatMap: node =>
      coordinate(node).filter(_.version.endsWith("-SNAPSHOT")).map(gav => Finding.SnapshotDependency(gav.render))
    val listed = direct(project, "dependencies").flatMap: deps =>
      direct(deps, "dependency").flatMap: dep =>
        coordinate(dep).filter(_.version.endsWith("-SNAPSHOT")).map(gav => Finding.SnapshotDependency(gav.render))
    parent ++ listed

  private def coordinate(node: Element): Option[Gav] =
    for
      group    <- one(node, "groupId")
      artifact <- one(node, "artifactId")
      version  <- one(node, "version")
    yield Gav(group, artifact, version)

  private def parsePom(bytes: Chunk[Byte]): Option[Element] =
    try
      val factory = DocumentBuilderFactory.newInstance()
      factory.setNamespaceAware(false)
      factory.setXIncludeAware(false)
      factory.setExpandEntityReferences(false)
      factory.setFeature("http://apache.org/xml/features/disallow-doctype-decl", true)
      factory.setFeature("http://xml.org/sax/features/external-general-entities", false)
      factory.setFeature("http://xml.org/sax/features/external-parameter-entities", false)
      val document = factory.newDocumentBuilder().parse(new ByteArrayInputStream(bytes.toArray))
      document.getDocumentElement match
        case element: Element if element.getNodeName == "project" => Some(element)
        case _                                                    => None
    catch case _: ParserConfigurationException | _: SAXException | _: IOException => None

  private def direct(parent: Element, name: String): List[Element] =
    val nodes = parent.getChildNodes
    (0 until nodes.getLength).toList.flatMap: index =>
      nodes.item(index) match
        case element: Element if element.getNodeName == name => Some(element)
        case _                                               => None

  private def one(parent: Element, name: String): Option[String] =
    direct(parent, name) match
      case element :: Nil =>
        val text = element.getTextContent.trim
        if text.isEmpty then None else Some(text)
      case _ => None

  private def directoryOf(path: String): String =
    val cut = path.lastIndexOf('/')
    if cut <= 0 then "" else path.substring(0, cut)

  private def fileName(dir: String, path: String): String =
    if path.startsWith(s"$dir/") then path.substring(dir.length + 1) else path

  private def legal(segment: String): Boolean =
    segment.nonEmpty && segment != "." && segment != ".." && !segment.exists(_.isWhitespace) &&
      !segment.contains(':') && !segment.contains('\\')

  private def hexMatches(data: Chunk[Byte], expected: String): Boolean =
    val text  = new String(data.toArray, StandardCharsets.UTF_8).trim
    val cut   = text.indexWhere(_.isWhitespace)
    val token = if cut < 0 then text else text.substring(0, cut)
    token.equalsIgnoreCase(expected)

  private def zipMagic(bytes: Chunk[Byte]): Boolean =
    bytes.take(2) match
      case first +: second +: Chunk() => first == 0x50.toByte && second == 0x4b.toByte
      case _                          => false

  private def unzip(bytes: Chunk[Byte]): Either[Chunk[Finding], Map[String, Chunk[Byte]]] =
    if !zipMagic(bytes) then Left(Chunk(Finding.Layout("zip")))
    else
      val zip = new ZipInputStream(new ByteArrayInputStream(bytes.toArray))
      try
        val found = List.newBuilder[(String, Chunk[Byte])]
        var entry = zip.getNextEntry
        var stop  = Option.empty[Chunk[Finding]]
        while entry != null && stop.isEmpty do
          val name = entry.getName
          if !entry.isDirectory && !name.endsWith("/") then
            readEntry(zip) match
              case Left(finding) => stop = Some(Chunk(finding))
              case Right(data)   => found += ((name, data))
          zip.closeEntry()
          entry = zip.getNextEntry
        stop match
          case Some(findings) => Left(findings)
          case None           => unique(found.result())
      catch case _: ZipException => Left(Chunk(Finding.Layout("zip")))
      finally
        try zip.close()
        catch case _: IOException => ()

  private def readEntry(zip: ZipInputStream): Either[Finding, Chunk[Byte]] =
    val buffer  = new Array[Byte](8192)
    val out     = new ByteArrayOutputStream
    var total   = 0L
    var reading = true
    while reading do
      val n = zip.read(buffer)
      if n < 0 then reading = false
      else
        total += n
        if total > Cap then reading = false
        else if n > 0 then out.write(buffer, 0, n)
    if total > Cap then Left(Finding.TooLarge) else Right(Chunk.fromArray(out.toByteArray))

  private def unique(pairs: List[(String, Chunk[Byte])]): Either[Chunk[Finding], Map[String, Chunk[Byte]]] =
    pairs.foldLeft[Either[Chunk[Finding], Map[String, Chunk[Byte]]]](Right(Map.empty)):
      case (Left(findings), _)                             => Left(findings)
      case (Right(have), (name, _)) if have.contains(name) =>
        Left(Chunk(Finding.Layout(name)))
      case (Right(have), (name, data)) => Right(have.updated(name, data))

  private def wire(state: DeploymentState): String = state match
    case DeploymentState.Pending    => "PENDING"
    case DeploymentState.Validating => "VALIDATING"
    case DeploymentState.Validated  => "VALIDATED"
    case DeploymentState.Publishing => "PUBLISHING"
    case DeploymentState.Published  => "PUBLISHED"
    case DeploymentState.Failed     => "FAILED"

  private def stateOf(json: String): Option[DeploymentState] =
    val key = "\"deploymentState\""
    val at  = json.indexOf(key)
    if at < 0 then None
    else
      val after = json.substring(at + key.length).dropWhile(_.isWhitespace)
      if !after.startsWith(":") then None
      else
        val quoted = after.drop(1).dropWhile(_.isWhitespace)
        if !quoted.startsWith("\"") then None
        else
          readString(quoted.drop(1)).flatMap: (token, _) =>
            token match
              case "PENDING"    => Some(DeploymentState.Pending)
              case "VALIDATING" => Some(DeploymentState.Validating)
              case "VALIDATED"  => Some(DeploymentState.Validated)
              case "PUBLISHING" => Some(DeploymentState.Publishing)
              case "PUBLISHED"  => Some(DeploymentState.Published)
              case "FAILED"     => Some(DeploymentState.Failed)
              case _            => None

  private def reportOf(json: String): Option[Chunk[Finding]] =
    val key = "\"report\""
    val at  = json.indexOf(key)
    if at < 0 then None
    else
      val after = json.substring(at + key.length).dropWhile(_.isWhitespace)
      if !after.startsWith(":") then None
      else
        val list = after.drop(1).dropWhile(_.isWhitespace)
        if !list.startsWith("[") then None else readList(list.drop(1), Chunk.empty)

  private def readList(text: String, acc: Chunk[Finding]): Option[Chunk[Finding]] =
    val rest = text.dropWhile(char => char.isWhitespace || char == ',')
    if rest.startsWith("]") then Some(acc)
    else if rest.startsWith("\"") then
      readString(rest.drop(1)) match
        case None              => None
        case Some(token, next) =>
          Finding.from(token) match
            case None          => None
            case Some(finding) => readList(next, acc :+ finding)
    else None

  private def readString(text: String): Option[(String, String)] =
    val out                                      = new StringBuilder
    def go(index: Int): Option[(String, String)] =
      if index >= text.length then None
      else
        text.charAt(index) match
          case '"'  => Some((out.result(), text.substring(index + 1)))
          case '\\' =>
            if index + 1 >= text.length then None
            else
              text.charAt(index + 1) match
                case '"'  => out.append('"'); go(index + 2)
                case '\\' => out.append('\\'); go(index + 2)
                case 'n'  => out.append('\n'); go(index + 2)
                case 'r'  => out.append('\r'); go(index + 2)
                case char => out.append(char); go(index + 2)
          case char =>
            out.append(char)
            go(index + 1)
    go(0)

  private def quote(text: String): String =
    val body = text.flatMap:
      case '"'  => "\\\""
      case '\\' => "\\\\"
      case '\n' => "\\n"
      case '\r' => "\\r"
      case char => String.valueOf(char)
    s"\"$body\""
end PortalReport
