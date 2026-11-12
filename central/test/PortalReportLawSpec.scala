package histos.central

import histos.version.{ReleaseVersion, ShipNumber, SnapshotPointer}
import histos.{Developer, DeveloperId, HttpUrl, License, LicenseName, Organization, PomText, Scm, ScmConnection}

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets
import java.util.zip.{ZipEntry, ZipOutputStream}

import scala.compiletime.testing.typeCheckErrors

import zio.Chunk
import zio.test.*

object PortalReportLawSpec extends ZIOSpecDefault:
  private val ship     = ShipNumber("1.0.0")
  private val release  = Revision.Release(ReleaseVersion(ship))
  private val snapshot = Revision.Pointer(SnapshotPointer(ship))
  private val jar      = Chunk[Byte](0x50.toByte, 0x4b.toByte, 3.toByte, 4.toByte)
  private val asc      = Chunk.single(1.toByte)
  private val gav      = Gav("demo", "widget", "1.0.0")

  private val documented = PomDocument(
    license = Some(License(LicenseName("Apache-2.0"), HttpUrl("https://www.apache.org/licenses/LICENSE-2.0.txt"))),
    developer = Some(Developer(DeveloperId("ada"), PomText("Ada"))),
    scm = Some(Scm(HttpUrl("https://example.com/demo"), ScmConnection("scm:git:https://example.com/demo.git"))),
  )

  def spec = suite("portal report")(
    test("a complete release bundle names the gav and has no findings"):
      assertTrue(judged(bundle("1.0.0", pom(release))) == Right(Report(Chunk.empty, Chunk(gav))))
    ,
    test("a bundle over the cap is too large and is not opened"):
      val over = PortalReport.inspect(Chunk[Byte](1, 2, 3), PortalReport.Cap + 1, Set.empty)
      val edge = PortalReport.inspect(Chunk[Byte](1, 2, 3), PortalReport.Cap, Set.empty)
      assertTrue(
        over == Report(Chunk(Finding.TooLarge), Chunk.empty),
        edge == Report(Chunk(upload), Chunk.empty),
      )
    ,
    test("a non-zip, an empty zip, a nameless entry, a duplicate name, and a root file are layout"):
      val empty  = zipped(Nil)
      val rooted = bundle("1.0.0", pom(release)).map(entries => ("README" -> utf8("note")) :: entries)
      val copied = duplicateZip("demo/widget/1.0.0/widget-1.0.0.jar")
      assertTrue(
        PortalReport.inspect(Chunk[Byte](1, 2, 3), 3L, Set.empty) == Report(Chunk(upload), Chunk.empty),
        PortalReport.inspect(empty, empty.length.toLong, Set.empty) == Report(Chunk(upload), Chunk.empty),
        judged(Right(List("demo/widget/1.0.0/" -> Chunk.empty))) == Right(Report(Chunk(upload), Chunk.empty)),
        judged(Right(List("" -> utf8("x")))) == Right(Report(Chunk(upload), Chunk.empty)),
        PortalReport.inspect(copied, copied.length.toLong, Set.empty) ==
          Report(Chunk(entry(EntryPath("demo/widget/1.0.0/widget-1.0.0.jar"))), Chunk.empty),
        judged(rooted).map(_.findings) == Right(Chunk(entry(EntryPath("README")))),
        judged(rooted).map(_.gavs) == Right(Chunk(gav)),
      )
    ,
    test("a directory entry is skipped and a short path is not a module"):
      val withDir = bundle("1.0.0", pom(release)).map(entries => ("demo/" -> Chunk.empty) :: entries)
      val short   = judged(Right(List("demo/widget/readme.txt" -> utf8("x"))))
      assertTrue(
        judged(withDir) == Right(Report(Chunk.empty, Chunk(gav))),
        short == Right(Report(Chunk(entry(EntryPath("demo/widget"))), Chunk.empty)),
      )
    ,
    test("an illegal path segment is layout and does not invent a gav"):
      val dirs = List(
        EntryPath("demo/./1.0.0"),
        EntryPath("demo/../1.0.0"),
        EntryPath("demo/wid get/1.0.0"),
        EntryPath("demo/a:b/1.0.0"),
        EntryPath("demo//1.0.0"),
        EntryPath("demo/widget\\x/1.0.0"),
      )
      val misses = dirs.flatMap: dir =>
        val report = judged(Right(List(s"${dir.value}/readme.txt" -> utf8("x"))))
        if report == Right(Report(Chunk(entry(dir)), Chunk.empty)) then Nil else List(dir)
      assertTrue(misses == Nil)
    ,
    test("uppercase checksum text still matches, and a wrong md5 is a checksum"):
      val upper = rewrite("demo/widget/1.0.0/widget-1.0.0.jar.md5", Digest.md5(jar).toUpperCase + " note\n")
      val wrong = rewrite("demo/widget/1.0.0/widget-1.0.0.jar.md5", "00\n")
      assertTrue(
        judged(upper) == Right(Report(Chunk.empty, Chunk(gav))),
        judged(wrong) == Right(
          Report(Chunk(Finding.Checksum(EntryPath("demo/widget/1.0.0/widget-1.0.0.jar.md5"))), Chunk(gav))
        ),
      )
    ,
    test("sha256 and sha512 are allowed and are not required"):
      val extra = bundle("1.0.0", pom(release)).map: entries =>
        entries ++ List(
          "demo/widget/1.0.0/widget-1.0.0.jar.sha256" -> utf8(Digest.sha256(jar) + "\n"),
          "demo/widget/1.0.0/widget-1.0.0.jar.sha512" -> utf8(Digest.sha512(jar) + "\n"),
        )
      assertTrue(judged(extra) == Right(Report(Chunk.empty, Chunk(gav))))
    ,
    test("a sha256 or sha512 that is present and wrong is a checksum"):
      val wrong = bundle("1.0.0", pom(release)).map: entries =>
        entries ++ List(
          "demo/widget/1.0.0/widget-1.0.0.jar.sha256" -> utf8("00\n"),
          "demo/widget/1.0.0/widget-1.0.0.pom.sha512" -> utf8("00\n"),
        )
      assertTrue(
        judged(wrong) == Right(
          Report(
            Chunk(
              Finding.Checksum(EntryPath("demo/widget/1.0.0/widget-1.0.0.jar.sha256")),
              Finding.Checksum(EntryPath("demo/widget/1.0.0/widget-1.0.0.pom.sha512")),
            ),
            Chunk(gav),
          )
        )
      )
    ,
    test("a missing or non-zip jar is a file set, and an empty signature is a file set"):
      val dropped = bundle("1.0.0", pom(release)).map: entries =>
        entries.filterNot(_._1 == "demo/widget/1.0.0/widget-1.0.0-sources.jar")
      val plain = bundle("1.0.0", pom(release)).map: entries =>
        entries.map:
          case ("demo/widget/1.0.0/widget-1.0.0-javadoc.jar", _) =>
            "demo/widget/1.0.0/widget-1.0.0-javadoc.jar" -> utf8("not a zip")
          case other => other
      val blank = rewrite("demo/widget/1.0.0/widget-1.0.0.jar.asc", "")
      assertTrue(
        judged(dropped) == Right(
          Report(Chunk(Finding.FileSet(EntryPath("demo/widget/1.0.0/widget-1.0.0-sources.jar"))), Chunk(gav))
        ),
        judged(plain) == Right(
          Report(Chunk(Finding.FileSet(EntryPath("demo/widget/1.0.0/widget-1.0.0-javadoc.jar"))), Chunk(gav))
        ),
        judged(blank) == Right(
          Report(Chunk(Finding.FileSet(EntryPath("demo/widget/1.0.0/widget-1.0.0.jar.asc"))), Chunk(gav))
        ),
      )
    ,
    test("an empty pom is empty, and a pom that is not a project names the pom"):
      val empty = judged(bundle("1.0.0", Right(" \n")))
      val alien = judged(bundle("1.0.0", Right("<not-a-project/>")))
      val typed = judged(bundle("1.0.0", Right("<!DOCTYPE a><project></project>")))
      assertTrue(
        empty.map(_.findings) == Right(Chunk(Finding.EmptyPom)),
        alien.map(_.findings) == Right(Chunk(Finding.PomFields(PomField.Project))),
        typed.map(_.findings) == Right(Chunk(Finding.PomFields(PomField.Project))),
      )
    ,
    test("a pom field that does not match the path is named"):
      val shifted = pom(release).map(_.replace("<groupId>demo</groupId>", "<groupId>other</groupId>"))
      assertTrue(
        judged(bundle("1.0.0", shifted)).map(_.findings) == Right(Chunk(Finding.PomFields(PomField.GroupId)))
      )
    ,
    test("a snapshot version on the path or in the pom is a snapshot finding"):
      val onPath = judged(bundle("1.0.0-SNAPSHOT", pom(snapshot)))
      val inPom  = pom(release).map(_.replace("<version>1.0.0</version>", "<version>9.9.9-SNAPSHOT</version>"))
      assertTrue(
        onPath.map(_.findings) == Right(Chunk(Finding.SnapshotVersion(SnapshotRevision("1.0.0-SNAPSHOT")))),
        onPath.map(_.gavs) == Right(Chunk(Gav("demo", "widget", "1.0.0-SNAPSHOT"))),
        judged(bundle("1.0.0", inPom)).map(_.findings) == Right(
          Chunk(Finding.PomFields(PomField.Version), Finding.SnapshotVersion(SnapshotRevision("9.9.9-SNAPSHOT")))
        ),
      )
    ,
    test("a snapshot parent or dependency is named by its gav, and one that is not a coordinate is that field"):
      def inserted(element: String) = pom(release).map(_.replace("</project>", s"  $element\n</project>"))
      val parent                    = inserted(
        "<parent><groupId>demo</groupId><artifactId>parent</artifactId><version>1.0.0-SNAPSHOT</version></parent>"
      )
      val depends = inserted(
        "<dependencies><dependency><groupId>demo</groupId><artifactId>core</artifactId><version>2.0.0-SNAPSHOT</version></dependency></dependencies>"
      )
      val coloned = inserted(
        "<parent><groupId>a:b</groupId><artifactId>parent</artifactId><version>1.0.0-SNAPSHOT</version></parent><dependencies><dependency><groupId>demo</groupId><artifactId>c:d</artifactId><version>2.0.0-SNAPSHOT</version></dependency></dependencies>"
      )
      val released = inserted(
        "<dependencies><dependency><groupId>a:b</groupId><artifactId>core</artifactId><version>2.0.0</version></dependency></dependencies>"
      )
      assertTrue(
        judged(bundle("1.0.0", parent)).map(_.findings) == Right(
          Chunk(Finding.SnapshotDependency(Gav("demo", "parent", "1.0.0-SNAPSHOT")))
        ),
        judged(bundle("1.0.0", depends)).map(_.findings) == Right(
          Chunk(Finding.SnapshotDependency(Gav("demo", "core", "2.0.0-SNAPSHOT")))
        ),
        judged(bundle("1.0.0", coloned)).map(_.findings) == Right(
          Chunk(Finding.PomFields(PomField.Parent), Finding.PomFields(PomField.Dependency))
        ),
        judged(bundle("1.0.0", released)).map(_.findings) == Right(Chunk.empty),
      )
    ,
    test("an extra file is layout, and a gav already published stays in the report"):
      val extra = bundle("1.0.0", pom(release)).map: entries =>
        entries :+ ("demo/widget/1.0.0/notes.txt" -> utf8("x"))
      val taken = judged(bundle("1.0.0", pom(release)), Set(gav))
      assertTrue(
        judged(extra).map(_.findings) == Right(Chunk(entry(EntryPath("demo/widget/1.0.0/notes.txt")))),
        taken == Right(Report(Chunk(Finding.AlreadyPublished(gav)), Chunk(gav))),
      )
    ,
    test("a gav line is three fields, and a bad line names what is missing"):
      assertTrue(
        Gav.from("demo:widget:1.0.0") == Right(gav),
        Gav.from("demo.group:widget:1.0.0").map(_.directory) == Right("demo/group/widget/1.0.0"),
        Gav.from("demo:widget") == Left(GavError.Fields("demo:widget")),
        Gav.from("demo:widget:1.0.0:extra") == Left(GavError.Fields("demo:widget:1.0.0:extra")),
        Gav.from(":widget:1.0.0") == Left(GavError.NoGroup(":widget:1.0.0")),
        Gav.from("demo::1.0.0") == Left(GavError.NoArtifact("demo::1.0.0")),
        Gav.from("demo:widget:") == Left(GavError.NoVersion("demo:widget:")),
        GavError.Fields("demo:widget").message == "'demo:widget' is not group:artifact:version",
        GavError.NoGroup(":widget:1.0.0").message == "':widget:1.0.0' has no group",
        GavError.NoArtifact("demo::1.0.0").message == "'demo::1.0.0' has no artifact",
        GavError.NoVersion("demo:widget:").message == "'demo:widget:' has no version",
      )
    ,
    test("a finding token is its kind and detail, the upload is a bare layout, and a newline becomes _"):
      val fields = List(
        PomField.Project    -> "pom-fields:pom",
        PomField.GroupId    -> "pom-fields:groupId",
        PomField.ArtifactId -> "pom-fields:artifactId",
        PomField.Version    -> "pom-fields:version",
        PomField.Name       -> "pom-fields:name",
        PomField.License    -> "pom-fields:license",
        PomField.Developer  -> "pom-fields:developer",
        PomField.Scm        -> "pom-fields:scm",
        PomField.Parent     -> "pom-fields:parent",
        PomField.Dependency -> "pom-fields:dependency",
      ).map((field, text) => Finding.PomFields(field) -> text)
      val tokens = List(
        upload                                                      -> "layout",
        entry(EntryPath("zip"))                                     -> "layout:zip",
        Finding.FileSet(EntryPath("demo/w.jar"))                    -> "file-set:demo/w.jar",
        Finding.Checksum(EntryPath("demo/w.jar.md5"))               -> "checksum:demo/w.jar.md5",
        Finding.SnapshotVersion(SnapshotRevision("1.0.0-SNAPSHOT")) -> "snapshot-version:1.0.0-SNAPSHOT",
        Finding.SnapshotDependency(Gav("d", "c", "1-SNAPSHOT"))     -> "snapshot-dependency:d:c:1-SNAPSHOT",
        Finding.AlreadyPublished(Gav("demo", "w", "1.0.0"))         -> "already-published:demo:w:1.0.0",
        Finding.EmptyPom                                            -> "empty-pom",
        Finding.TooLarge                                            -> "too-large",
      ) ++ fields
      assertTrue(
        fields.collect { case (Finding.PomFields(field), _) => field } == PomField.values.toList,
        tokens.map((finding, _) => finding.token) == tokens.map((_, text) => text),
        tokens.map((_, text) => Finding.from(text)) == tokens.map((finding, _) => Right(finding)),
        entry(EntryPath("a\nb\rc")).token == "layout:a_b_c",
        Finding.from("layout:a_b_c") == Right(entry(EntryPath("a_b_c"))),
        Finding.from("nope") == Left(FindingError.Unknown("nope")),
        Finding.from(":nope") == Left(FindingError.Unknown(":nope")),
        Finding.from("bogus:x") == Left(FindingError.Unknown("bogus:x")),
        Finding.from("layout:") == Left(FindingError.Bare("layout")),
        Finding.from("file-set") == Left(FindingError.Bare("file-set")),
        Finding.from("checksum:") == Left(FindingError.Bare("checksum")),
        Finding.from("already-published:") == Left(FindingError.Bare("already-published")),
        Finding.from("empty-pom:x") == Left(FindingError.Extra("empty-pom")),
        Finding.from("pom-fields:homepage") == Left(FindingError.Field("homepage")),
        Finding.from("snapshot-version:1.0.0") ==
          Left(FindingError.Version(SnapshotRevisionError.NotSnapshot("1.0.0"))),
        Finding.from("snapshot-dependency:d:c") == Left(FindingError.Coordinate(GavError.Fields("d:c"))),
        Finding.from("already-published::w:1.0.0") == Left(FindingError.Coordinate(GavError.NoGroup(":w:1.0.0"))),
        FindingError.Unknown("nope").message == "'nope' is not a finding",
        FindingError.Bare("layout").message == "finding 'layout' needs a detail after ':'",
        FindingError.Extra("empty-pom").message == "finding 'empty-pom' takes no detail",
        FindingError.Field("homepage").message == "'homepage' is not a pom field",
        FindingError.Version(SnapshotRevisionError.NotSnapshot("1.0.0")).message == "'1.0.0' is not a snapshot",
        FindingError.Coordinate(GavError.Fields("d:c")).message == "'d:c' is not group:artifact:version",
        EntryPath.from("") == Left(EntryPath.Error.Empty),
        EntryPath.Error.Empty.message == "a bundle entry path is empty",
        typeCheckErrors("""EntryPath("")""").map(_.message) == List("a bundle entry path is empty"),
        EntryPath("demo/w.jar").name == "w.jar",
        EntryPath("demo").child("w.jar") == EntryPath("demo/w.jar"),
        EntryPath("demo/w.jar").suffixed(".md5") == EntryPath("demo/w.jar.md5"),
        SnapshotRevision.from("-SNAPSHOT").map(_.value) == Right("-SNAPSHOT"),
        SnapshotRevision.from("1.0.0") == Left(SnapshotRevisionError.NotSnapshot("1.0.0")),
        typeCheckErrors("""SnapshotRevision("1.0.0")""").map(_.message) == List("'1.0.0' is not a snapshot"),
      )
    ,
    test("render then parse keeps the state and the findings, and a bad state, list, or token says which"):
      val id       = DeploymentId("deploy-1")
      val findings = Chunk(upload, entry(EntryPath("demo/widget")), Finding.EmptyPom, Finding.TooLarge)
      val states   = List(
        DeploymentState.Pending,
        DeploymentState.Validating,
        DeploymentState.Validated,
        DeploymentState.Publishing,
        DeploymentState.Published,
        DeploymentState.Failed,
      )
      val misses = states.flatMap: state =>
        val json = PortalReport.render(id, state, findings)
        if PortalReport.parse(json) == Right((state, findings)) && json.contains("deploy-1") then Nil
        else List(state)
      val validated = PortalReport.render(id, DeploymentState.Validated, Chunk.empty)
      assertTrue(
        misses == Nil,
        PortalReport.parse(validated.replace("VALIDATED", "NOPE")) == Left(StatusError.NoState),
        PortalReport.parse(validated.replace(""""report":[]""", """"report":"x"""")) == Left(StatusError.NoReport),
        PortalReport.parse(validated.replace(""""report":[]""", """"report":["nope"]""")) ==
          Left(StatusError.Token(FindingError.Unknown("nope"))),
        StatusError.NoState.message == "the status body names no deployment state",
        StatusError.NoReport.message == "the status body has no report list",
        StatusError.Token(FindingError.Unknown("nope")).message == "'nope' is not a finding",
      ),
  )

  private val upload: Finding = Finding.Layout(LayoutPlace.Upload)

  private def entry(path: EntryPath): Finding = Finding.Layout(LayoutPlace.Entry(path))

  private def pom(revision: Revision): Either[ArtifactIdError, String] =
    ArtifactId
      .from("widget")
      .map(name => Layout.pom(Artifact(Organization("demo"), name, revision, jar, document = documented)))

  private def bundle(
      version: String,
      text: Either[ArtifactIdError, String],
  ): Either[ArtifactIdError, List[(String, Chunk[Byte])]] =
    text.map: pom =>
      val dir  = s"demo/widget/$version"
      val stem = s"widget-$version"
      List(s"$stem.jar" -> jar, s"$stem-sources.jar" -> jar, s"$stem-javadoc.jar" -> jar, s"$stem.pom" -> utf8(pom))
        .flatMap: (name, bytes) =>
          List(
            s"$dir/$name"      -> bytes,
            s"$dir/$name.md5"  -> utf8(Digest.md5(bytes) + "\n"),
            s"$dir/$name.sha1" -> utf8(Digest.sha1(bytes) + "\n"),
            s"$dir/$name.asc"  -> asc,
          )

  private def rewrite(name: String, text: String): Either[ArtifactIdError, List[(String, Chunk[Byte])]] =
    bundle("1.0.0", pom(release)).map: entries =>
      entries.map:
        case (found, _) if found == name => found -> utf8(text)
        case other                       => other

  private def judged(
      entries: Either[ArtifactIdError, List[(String, Chunk[Byte])]],
      published: Set[Gav] = Set.empty,
  ): Either[ArtifactIdError, Report] =
    entries.map(zipped).map(zip => PortalReport.inspect(zip, zip.length.toLong, published))

  private def zipped(entries: List[(String, Chunk[Byte])]): Chunk[Byte] =
    val out = new ByteArrayOutputStream
    val zip = new ZipOutputStream(out)
    entries.foreach: (name, bytes) =>
      zip.putNextEntry(new ZipEntry(name))
      if bytes.nonEmpty then zip.write(bytes.toArray)
      zip.closeEntry()
    zip.close()
    Chunk.fromArray(out.toByteArray)

  private def utf8(text: String): Chunk[Byte] =
    Chunk.fromArray(text.getBytes(StandardCharsets.UTF_8))

  /** Two empty stored entries of `name`. `ZipOutputStream` refuses that pair, and the layout law is about reading it.
    */
  private def duplicateZip(name: String): Chunk[Byte] =
    val raw     = name.getBytes(StandardCharsets.UTF_8)
    val local   = zipLocal(raw)
    val first   = zipCentral(raw, 0)
    val second  = zipCentral(raw, local.length)
    val central = first ++ second
    val end     = zipEnd(central.length, local.length * 2)
    Chunk.fromArray(local ++ local ++ central ++ end)

  private def zipLocal(name: Array[Byte]): Array[Byte] =
    val out = new ByteArrayOutputStream
    writeInt(out, 0x04034b50)
    writeShort(out, 20)
    writeShort(out, 0)
    writeShort(out, 0)
    writeShort(out, 0)
    writeShort(out, 0)
    writeInt(out, 0)
    writeInt(out, 0)
    writeInt(out, 0)
    writeShort(out, name.length)
    writeShort(out, 0)
    out.write(name)
    out.toByteArray

  private def zipCentral(name: Array[Byte], offset: Int): Array[Byte] =
    val out = new ByteArrayOutputStream
    writeInt(out, 0x02014b50)
    writeShort(out, 20)
    writeShort(out, 20)
    writeShort(out, 0)
    writeShort(out, 0)
    writeShort(out, 0)
    writeShort(out, 0)
    writeInt(out, 0)
    writeInt(out, 0)
    writeInt(out, 0)
    writeShort(out, name.length)
    writeShort(out, 0)
    writeShort(out, 0)
    writeShort(out, 0)
    writeShort(out, 0)
    writeInt(out, 0)
    writeInt(out, offset)
    out.write(name)
    out.toByteArray

  private def zipEnd(centralBytes: Int, centralOffset: Int): Array[Byte] =
    val out = new ByteArrayOutputStream
    writeInt(out, 0x06054b50)
    writeShort(out, 0)
    writeShort(out, 0)
    writeShort(out, 2)
    writeShort(out, 2)
    writeInt(out, centralBytes)
    writeInt(out, centralOffset)
    writeShort(out, 0)
    out.toByteArray

  private def writeInt(out: ByteArrayOutputStream, value: Int): Unit =
    out.write(value)
    out.write(value >>> 8)
    out.write(value >>> 16)
    out.write(value >>> 24)

  private def writeShort(out: ByteArrayOutputStream, value: Int): Unit =
    out.write(value)
    out.write(value >>> 8)
end PortalReportLawSpec
