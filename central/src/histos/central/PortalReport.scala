package histos.central

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, IOException}
import java.nio.charset.StandardCharsets
import java.util.zip.{ZipException, ZipInputStream}
import javax.xml.parsers.{DocumentBuilderFactory, ParserConfigurationException}

import scala.annotation.{publicInBinary, tailrec}

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
  /** `group:artifact:version`, the line the store writes. */
  def from(line: String): Either[GavError, Gav] =
    line.split(":", -1).toList match
      case group :: artifact :: version :: Nil =>
        if group.isEmpty then Left(GavError.NoGroup(line))
        else if artifact.isEmpty then Left(GavError.NoArtifact(line))
        else if version.isEmpty then Left(GavError.NoVersion(line))
        else Right(Gav(group, artifact, version))
      case _ => Left(GavError.Fields(line))

enum GavError:
  case Fields(line: String)
  case NoGroup(line: String)
  case NoArtifact(line: String)
  case NoVersion(line: String)

  def message: String = this match
    case Fields(line)     => s"'$line' is not group:artifact:version"
    case NoGroup(line)    => s"'$line' has no group"
    case NoArtifact(line) => s"'$line' has no artifact"
    case NoVersion(line)  => s"'$line' has no version"
end GavError

/** A path inside an uploaded bundle, as the zip names it. Never empty. */
opaque type EntryPath = String

object EntryPath:
  given CanEqual[EntryPath, EntryPath] = CanEqual.derived

  enum Error:
    case Empty

    def message: String = this match
      case Empty => "a bundle entry path is empty"
  end Error

  inline def apply(inline raw: String): EntryPath = ${ EntryPathMacro.literal('raw) }

  def from(raw: String): Either[Error, EntryPath] =
    if raw.isEmpty then Left(Error.Empty) else Right(trusted(raw))

  extension (path: EntryPath)
    def value: String = path

    /** The last segment, which is how a module directory looks its files up. */
    def name: String = path.substring(path.lastIndexOf('/') + 1)

    /** A file `name` inside this directory. */
    def child(name: String): EntryPath = s"$path/$name"

    /** This path with `suffix` appended, as `.md5` names the checksum beside its file. */
    def suffixed(suffix: String): EntryPath = path + suffix

    /** The directory that holds this entry. An entry at the root has none. */
    def parent: Option[EntryPath] =
      val cut = path.lastIndexOf('/')
      if cut <= 0 then None else Some(path.substring(0, cut))
  end extension

  /** Spliced by the literal macro. Source code cannot call it. */
  @publicInBinary private[central] def trusted(raw: String): EntryPath = raw
end EntryPath

/** What a layout finding names. */
enum LayoutPlace:
  /** The upload itself: not a zip, or a zip that holds no file. */
  case Upload

  /** One entry, or a directory of entries that is not `group/artifact/version`. */
  case Entry(path: EntryPath)
end LayoutPlace

/** A pom element Central requires. */
enum PomField:
  /** The document is not a `<project>`. */
  case Project
  case GroupId, ArtifactId, Version, Name, License, Developer, Scm

  /** A snapshot parent or dependency whose coordinate is not `group:artifact:version`. */
  case Parent, Dependency

  def token: String = this match
    case Project    => "pom"
    case GroupId    => "groupId"
    case ArtifactId => "artifactId"
    case Version    => "version"
    case Name       => "name"
    case License    => "license"
    case Developer  => "developer"
    case Scm        => "scm"
    case Parent     => "parent"
    case Dependency => "dependency"
end PomField

/** Why a bundle is not a release Central would publish. */
enum Finding:
  case Layout(place: LayoutPlace)
  case FileSet(path: EntryPath)
  case Checksum(path: EntryPath)
  case PomFields(field: PomField)
  case SnapshotVersion(version: SnapshotRevision)
  case SnapshotDependency(gav: Gav)
  case AlreadyPublished(gav: Gav)
  case EmptyPom
  case TooLarge

  /** The upload is `layout` with no detail, so a root entry named `zip` is not mistaken for it. */
  def token: String = this match
    case Layout(LayoutPlace.Upload)      => "layout"
    case Layout(LayoutPlace.Entry(path)) => s"layout:${Finding.sane(path.value)}"
    case FileSet(path)                   => s"file-set:${Finding.sane(path.value)}"
    case Checksum(path)                  => s"checksum:${Finding.sane(path.value)}"
    case PomFields(field)                => s"pom-fields:${field.token}"
    case SnapshotVersion(version)        => s"snapshot-version:${Finding.sane(version.value)}"
    case SnapshotDependency(gav)         => s"snapshot-dependency:${Finding.sane(gav.render)}"
    case AlreadyPublished(gav)           => s"already-published:${Finding.sane(gav.render)}"
    case EmptyPom                        => "empty-pom"
    case TooLarge                        => "too-large"
end Finding

object Finding:
  def from(token: String): Either[FindingError, Finding] =
    token.indexOf(':') match
      case -1 =>
        token match
          case "empty-pom" => Right(EmptyPom)
          case "too-large" => Right(TooLarge)
          case "layout"    => Right(Layout(LayoutPlace.Upload))
          case kind        => named(token, kind, "")
      case cut => named(token, token.substring(0, cut), token.substring(cut + 1))

  /** A report line is one token. A newline in a path would split it. */
  def sane(raw: String): String =
    raw.flatMap:
      case '\n' | '\r' => "_"
      case char        => String.valueOf(char)

  private def named(token: String, kind: String, detail: String): Either[FindingError, Finding] =
    def entry(make: EntryPath => Finding): Either[FindingError, Finding] =
      EntryPath.from(detail).left.map(_ => FindingError.Bare(kind)).map(make)
    def parsed[A](parse: String => Either[FindingError, A])(make: A => Finding): Either[FindingError, Finding] =
      if detail.isEmpty then Left(FindingError.Bare(kind)) else parse(detail).map(make)
    def gav(raw: String): Either[FindingError, Gav] =
      Gav.from(raw).left.map(FindingError.Coordinate(_))
    kind match
      case "empty-pom" | "too-large" => Left(FindingError.Extra(kind))
      case "layout"                  => entry(path => Layout(LayoutPlace.Entry(path)))
      case "file-set"                => entry(FileSet(_))
      case "checksum"                => entry(Checksum(_))
      case "pom-fields"              =>
        parsed(raw => PomField.values.find(_.token == raw).toRight(FindingError.Field(raw)))(PomFields(_))
      case "snapshot-version" =>
        parsed(raw => SnapshotRevision.from(raw).left.map(FindingError.Version(_)))(SnapshotVersion(_))
      case "snapshot-dependency" => parsed(gav)(SnapshotDependency(_))
      case "already-published"   => parsed(gav)(AlreadyPublished(_))
      case _                     => Left(FindingError.Unknown(token))
end Finding

/** Why a report token is not a finding. */
enum FindingError:
  case Unknown(token: String)
  case Bare(kind: String)
  case Extra(kind: String)
  case Field(raw: String)
  case Version(error: SnapshotRevisionError)
  case Coordinate(error: GavError)

  def message: String = this match
    case Unknown(token)    => s"'$token' is not a finding"
    case Bare(kind)        => s"finding '$kind' needs a detail after ':'"
    case Extra(kind)       => s"finding '$kind' takes no detail"
    case Field(raw)        => s"'$raw' is not a pom field"
    case Version(error)    => error.message
    case Coordinate(error) => error.message
end FindingError

/** Why a status body does not read as a state and a report. */
enum StatusError:
  case NoState
  case NoReport
  case Token(error: FindingError)

  def message: String = this match
    case NoState      => "the status body names no deployment state"
    case NoReport     => "the status body has no report list"
    case Token(error) => error.message
end StatusError

/** The validation report a deployment carries. `size` is the upload the server measured. */
final case class Report(findings: Chunk[Finding], gavs: Chunk[Gav])

object PortalReport:
  /** Central refuses a bundle larger than this. `Bundle.Cap` is the same bound. */
  val Cap: Long = 1024L * 1024L * 1024L

  def inspect(zip: Chunk[Byte], size: Long, published: Set[Gav]): Report =
    if size > Cap || zip.length.toLong > Cap then Report(Chunk(Finding.TooLarge), Chunk.empty)
    else
      unzip(zip) match
        case Left(findings)                => Report(findings, Chunk.empty)
        case Right(files) if files.isEmpty => Report(Chunk(Finding.Layout(LayoutPlace.Upload)), Chunk.empty)
        case Right(files)                  => judge(files, published)

  def render(id: DeploymentId, state: DeploymentState, findings: Chunk[Finding]): String =
    val report = findings.map(finding => quote(finding.token)).mkString("[", ",", "]")
    s"""{"deploymentId":${quote(id.value)},"deploymentState":${quote(wire(state))},"report":$report}"""

  def parse(json: String): Either[StatusError, (DeploymentState, Chunk[Finding])] =
    for
      state <- stateOf(json).toRight(StatusError.NoState)
      found <- reportOf(json)
    yield (state, found)

  private def judge(files: Map[EntryPath, Chunk[Byte]], published: Set[Gav]): Report =
    val grouped       = files.groupBy((path, _) => path.parent)
    val (found, gavs) = grouped.foldLeft((Chunk.empty[Finding], Chunk.empty[Gav])):
      case ((acc, gavs), (None, entries)) =>
        (acc ++ entries.map((path, _) => Finding.Layout(LayoutPlace.Entry(path))), gavs)
      case ((acc, gavs), (Some(dir), entries)) =>
        val (more, gav) = module(dir, entries)
        (acc ++ more, gav.fold(gavs)(gavs :+ _))
    val taken = gavs.filter(published.contains).map(Finding.AlreadyPublished(_))
    Report(found ++ taken, gavs)

  private def module(dir: EntryPath, entries: Map[EntryPath, Chunk[Byte]]): (Chunk[Finding], Option[Gav]) =
    val segments = dir.value.split("/", -1).toList
    segments match
      case group :+ artifact :+ version if group.nonEmpty && group.forall(legal) && legal(artifact) && legal(version) =>
        val gav   = Gav(group.mkString("."), artifact, version)
        val stem  = s"$artifact-$version"
        val names = entries.map((path, bytes) => (fileName(dir, path), bytes))
        val extra = entries.collect:
          case (path, _) if !allowed(stem, fileName(dir, path)) => Finding.Layout(LayoutPlace.Entry(path))
        val jars = Chunk(
          artifactFile(names, dir.child(s"$stem.jar")),
          artifactFile(names, dir.child(s"$stem-sources.jar")),
          artifactFile(names, dir.child(s"$stem-javadoc.jar")),
        ).flatten
        val pomFound = pomFile(names, dir.child(s"$stem.pom"), gav)
        val snapshot =
          SnapshotRevision.from(version).fold(_ => Chunk.empty, found => Chunk(Finding.SnapshotVersion(found)))
        (Chunk.fromIterable(extra) ++ jars ++ pomFound ++ snapshot, Some(gav))
      case _ =>
        (Chunk(Finding.Layout(LayoutPlace.Entry(dir))), None)

  /** A jar that is missing or is not a zip is the same file-set finding. */
  private def artifactFile(files: Map[String, Chunk[Byte]], path: EntryPath): Chunk[Finding] =
    files.get(path.name) match
      case Some(bytes) if zipMagic(bytes) => sidecars(files, path, bytes)
      case _                              => Chunk(Finding.FileSet(path))

  private def pomFile(files: Map[String, Chunk[Byte]], path: EntryPath, gav: Gav): Chunk[Finding] =
    files.get(path.name) match
      case None =>
        Chunk(Finding.FileSet(path))
      case Some(bytes) if new String(bytes.toArray, StandardCharsets.UTF_8).trim.isEmpty =>
        Chunk(Finding.EmptyPom) ++ sidecars(files, path, bytes)
      case Some(bytes) =>
        val fields =
          parsePom(bytes) match
            case None          => Chunk(Finding.PomFields(PomField.Project))
            case Some(project) => pomFields(project, gav)
        fields ++ sidecars(files, path, bytes)

  private def sidecars(files: Map[String, Chunk[Byte]], path: EntryPath, bytes: Chunk[Byte]): Chunk[Finding] =
    Chunk.fromIterable(
      List(
        required(files, path.suffixed(".md5"), Digest.md5(bytes)),
        required(files, path.suffixed(".sha1"), Digest.sha1(bytes)),
        present(files, path.suffixed(".sha256"), Digest.sha256(bytes)),
        present(files, path.suffixed(".sha512"), Digest.sha512(bytes)),
        signature(files, path.suffixed(".asc")),
      ).flatten
    )

  private def required(files: Map[String, Chunk[Byte]], path: EntryPath, expected: String): Option[Finding] =
    files.get(path.name) match
      case None       => Some(Finding.FileSet(path))
      case Some(data) => matched(path, data, expected)

  /** Central does not require `.sha256` or `.sha512`. One that is present still has to match. */
  private def present(files: Map[String, Chunk[Byte]], path: EntryPath, expected: String): Option[Finding] =
    files.get(path.name).flatMap(data => matched(path, data, expected))

  private def matched(path: EntryPath, data: Chunk[Byte], expected: String): Option[Finding] =
    if hexMatches(data, expected) then None else Some(Finding.Checksum(path))

  private def signature(files: Map[String, Chunk[Byte]], path: EntryPath): Option[Finding] =
    files.get(path.name) match
      case Some(data) if data.nonEmpty => None
      case _                           => Some(Finding.FileSet(path))

  private def allowed(stem: String, name: String): Boolean =
    val artifacts = List(s"$stem.jar", s"$stem-sources.jar", s"$stem-javadoc.jar", s"$stem.pom")
    val suffixes  = List("", ".md5", ".sha1", ".sha256", ".sha512", ".asc")
    artifacts.exists(artifact => suffixes.exists(suffix => name == s"$artifact$suffix"))

  private def pomFields(project: Element, gav: Gav): Chunk[Finding] =
    val found = List.newBuilder[Finding]
    if one(project, "groupId") != Some(gav.group) then found += Finding.PomFields(PomField.GroupId)
    if one(project, "artifactId") != Some(gav.artifact) then found += Finding.PomFields(PomField.ArtifactId)
    one(project, "version") match
      case Some(version) if version == gav.version => ()
      case Some(version)                           =>
        found += Finding.PomFields(PomField.Version)
        SnapshotRevision.from(version).foreach(snapshot => found += Finding.SnapshotVersion(snapshot))
      case None => found += Finding.PomFields(PomField.Version)
    if one(project, "name").isEmpty then found += Finding.PomFields(PomField.Name)
    if !licenseOk(project) then found += Finding.PomFields(PomField.License)
    if !developerOk(project) then found += Finding.PomFields(PomField.Developer)
    if !scmOk(project) then found += Finding.PomFields(PomField.Scm)
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
    val parent = direct(project, "parent").flatMap(node => snapshot(node, PomField.Parent))
    val listed = direct(project, "dependencies").flatMap: deps =>
      direct(deps, "dependency").flatMap(dep => snapshot(dep, PomField.Dependency))
    parent ++ listed

  /** A coordinate at a snapshot version. A `:` inside one of its parts is not a coordinate, so it is that field. */
  private def snapshot(node: Element, field: PomField): Option[Finding] =
    for
      group    <- one(node, "groupId")
      artifact <- one(node, "artifactId")
      version  <- one(node, "version") if SnapshotRevision.from(version).isRight
    yield Gav.from(s"$group:$artifact:$version").fold(_ => Finding.PomFields(field), Finding.SnapshotDependency(_))

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

  private def fileName(dir: EntryPath, path: EntryPath): String =
    if path.value.startsWith(s"${dir.value}/") then path.value.substring(dir.value.length + 1) else path.value

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

  /** An entry with no name makes the whole upload unreadable as a bundle. */
  private def unzip(bytes: Chunk[Byte]): Either[Chunk[Finding], Map[EntryPath, Chunk[Byte]]] =
    val unbundled = Chunk(Finding.Layout(LayoutPlace.Upload))
    if !zipMagic(bytes) then Left(unbundled)
    else
      val zip = new ZipInputStream(new ByteArrayInputStream(bytes.toArray))
      try
        val found = List.newBuilder[(EntryPath, Chunk[Byte])]
        var entry = zip.getNextEntry
        var stop  = Option.empty[Chunk[Finding]]
        while entry != null && stop.isEmpty do
          val name = entry.getName
          if !entry.isDirectory && !name.endsWith("/") then
            EntryPath.from(name) match
              case Left(EntryPath.Error.Empty) => stop = Some(unbundled)
              case Right(path)                 =>
                readEntry(zip) match
                  case Left(finding) => stop = Some(Chunk(finding))
                  case Right(data)   => found += ((path, data))
          zip.closeEntry()
          entry = zip.getNextEntry
        stop match
          case Some(findings) => Left(findings)
          case None           => unique(found.result())
      catch case _: ZipException => Left(unbundled)
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

  private def unique(pairs: List[(EntryPath, Chunk[Byte])]): Either[Chunk[Finding], Map[EntryPath, Chunk[Byte]]] =
    pairs.foldLeft[Either[Chunk[Finding], Map[EntryPath, Chunk[Byte]]]](Right(Map.empty)):
      case (Left(findings), _)                             => Left(findings)
      case (Right(have), (path, _)) if have.contains(path) =>
        Left(Chunk(Finding.Layout(LayoutPlace.Entry(path))))
      case (Right(have), (path, data)) => Right(have.updated(path, data))

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

  private def reportOf(json: String): Either[StatusError, Chunk[Finding]] =
    val key = "\"report\""
    val at  = json.indexOf(key)
    if at < 0 then Left(StatusError.NoReport)
    else
      val after = json.substring(at + key.length).dropWhile(_.isWhitespace)
      if !after.startsWith(":") then Left(StatusError.NoReport)
      else
        val list = after.drop(1).dropWhile(_.isWhitespace)
        if !list.startsWith("[") then Left(StatusError.NoReport) else readList(list.drop(1), Chunk.empty)

  @tailrec
  private def readList(text: String, acc: Chunk[Finding]): Either[StatusError, Chunk[Finding]] =
    val rest = text.dropWhile(char => char.isWhitespace || char == ',')
    if rest.startsWith("]") then Right(acc)
    else if rest.startsWith("\"") then
      readString(rest.drop(1)) match
        case None              => Left(StatusError.NoReport)
        case Some(token, next) =>
          Finding.from(token) match
            case Left(error)    => Left(StatusError.Token(error))
            case Right(finding) => readList(next, acc :+ finding)
    else Left(StatusError.NoReport)

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
