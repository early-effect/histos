package histos.central

import histos.version.{ReleaseVersion, ShipNumber, SnapshotPointer}
import histos.{Developer, DeveloperId, HttpUrl, License, LicenseName, Organization, PomText, Scm, ScmConnection}

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets
import java.util.zip.{ZipEntry, ZipOutputStream}

import zio.{Chunk, ZIO}
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
        edge == Report(Chunk(Finding.Layout("zip")), Chunk.empty),
      )
    ,
    test("a non-zip, an empty zip, a duplicate name, and a root file are layout"):
      val empty  = zipped(Nil)
      val rooted = bundle("1.0.0", pom(release)).map(entries => ("README" -> utf8("note")) :: entries)
      val copied = duplicateZip("demo/widget/1.0.0/widget-1.0.0.jar")
      assertTrue(
        PortalReport.inspect(Chunk[Byte](1, 2, 3), 3L, Set.empty) == Report(Chunk(Finding.Layout("zip")), Chunk.empty),
        PortalReport.inspect(empty, empty.length.toLong, Set.empty) == Report(
          Chunk(Finding.Layout("zip")),
          Chunk.empty,
        ),
        judged(Right(List("demo/widget/1.0.0/" -> Chunk.empty))) == Right(
          Report(Chunk(Finding.Layout("zip")), Chunk.empty)
        ),
        PortalReport.inspect(copied, copied.length.toLong, Set.empty) ==
          Report(Chunk(Finding.Layout("demo/widget/1.0.0/widget-1.0.0.jar")), Chunk.empty),
        judged(rooted).map(_.findings) == Right(Chunk(Finding.Layout("README"))),
        judged(rooted).map(_.gavs) == Right(Chunk(gav)),
      )
    ,
    test("a directory entry is skipped and a short path is not a module"):
      val withDir = bundle("1.0.0", pom(release)).map(entries => ("demo/" -> Chunk.empty) :: entries)
      val short   = judged(Right(List("demo/widget/readme.txt" -> utf8("x"))))
      assertTrue(
        judged(withDir) == Right(Report(Chunk.empty, Chunk(gav))),
        short == Right(Report(Chunk(Finding.Layout("demo/widget")), Chunk.empty)),
      )
    ,
    test("an illegal path segment is layout and does not invent a gav"):
      val dirs = List(
        "demo/./1.0.0",
        "demo/../1.0.0",
        "demo/wid get/1.0.0",
        "demo/a:b/1.0.0",
        "demo//1.0.0",
        "demo/widget\\x/1.0.0",
      )
      val misses = dirs.flatMap: dir =>
        val report = judged(Right(List(s"$dir/readme.txt" -> utf8("x"))))
        if report == Right(Report(Chunk(Finding.Layout(dir)), Chunk.empty)) then Nil else List(dir)
      assertTrue(misses == Nil)
    ,
    test("uppercase checksum text still matches, and a wrong md5 is a checksum"):
      val upper = rewrite("demo/widget/1.0.0/widget-1.0.0.jar.md5", Digest.md5(jar).toUpperCase + " note\n")
      val wrong = rewrite("demo/widget/1.0.0/widget-1.0.0.jar.md5", "00\n")
      assertTrue(
        judged(upper) == Right(Report(Chunk.empty, Chunk(gav))),
        judged(wrong) == Right(
          Report(Chunk(Finding.Checksum("demo/widget/1.0.0/widget-1.0.0.jar.md5")), Chunk(gav))
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
              Finding.Checksum("demo/widget/1.0.0/widget-1.0.0.jar.sha256"),
              Finding.Checksum("demo/widget/1.0.0/widget-1.0.0.pom.sha512"),
            ),
            Chunk(gav),
          )
        )
      )
    ,
    test("a missing sources jar is a file set, and an empty signature is a file set"):
      val dropped = bundle("1.0.0", pom(release)).map: entries =>
        entries.filterNot(_._1 == "demo/widget/1.0.0/widget-1.0.0-sources.jar")
      val blank = rewrite("demo/widget/1.0.0/widget-1.0.0.jar.asc", "")
      assertTrue(
        judged(dropped) == Right(
          Report(Chunk(Finding.FileSet("demo/widget/1.0.0/widget-1.0.0-sources.jar")), Chunk(gav))
        ),
        judged(blank) == Right(
          Report(Chunk(Finding.FileSet("demo/widget/1.0.0/widget-1.0.0.jar.asc")), Chunk(gav))
        ),
      )
    ,
    test("an empty pom is empty, and a pom that is not a project names the pom"):
      val empty = judged(bundle("1.0.0", Right(" \n")))
      val alien = judged(bundle("1.0.0", Right("<not-a-project/>")))
      val typed = judged(bundle("1.0.0", Right("<!DOCTYPE a><project></project>")))
      assertTrue(
        empty.map(_.findings) == Right(Chunk(Finding.EmptyPom)),
        alien.map(_.findings.contains(Finding.PomFields("pom"))) == Right(true),
        typed.map(_.findings.contains(Finding.PomFields("pom"))) == Right(true),
      )
    ,
    test("a pom field that does not match the path is named"):
      val shifted = pom(release).map(_.replace("<groupId>demo</groupId>", "<groupId>other</groupId>"))
      assertTrue(
        judged(bundle("1.0.0", shifted)).map(_.findings) == Right(Chunk(Finding.PomFields("groupId")))
      )
    ,
    test("a snapshot version on the path or in the pom is a snapshot finding"):
      val onPath = judged(bundle("1.0.0-SNAPSHOT", pom(snapshot)))
      val inPom  = pom(release).map(_.replace("<version>1.0.0</version>", "<version>9.9.9-SNAPSHOT</version>"))
      assertTrue(
        onPath.map(_.findings) == Right(Chunk(Finding.SnapshotVersion("1.0.0-SNAPSHOT"))),
        onPath.map(_.gavs) == Right(Chunk(Gav("demo", "widget", "1.0.0-SNAPSHOT"))),
        judged(bundle("1.0.0", inPom)).map(_.findings) == Right(
          Chunk(Finding.PomFields("version"), Finding.SnapshotVersion("9.9.9-SNAPSHOT"))
        ),
      )
    ,
    test("a snapshot parent or dependency is named by its gav"):
      val parent = pom(release).map: text =>
        text.replace(
          "</project>",
          """  <parent><groupId>demo</groupId><artifactId>parent</artifactId><version>1.0.0-SNAPSHOT</version></parent>
            |</project>""".stripMargin,
        )
      val depends = pom(release).map: text =>
        text.replace(
          "</project>",
          """  <dependencies><dependency><groupId>demo</groupId><artifactId>core</artifactId><version>2.0.0-SNAPSHOT</version></dependency></dependencies>
            |</project>""".stripMargin,
        )
      assertTrue(
        judged(bundle("1.0.0", parent)).map(_.findings) == Right(
          Chunk(Finding.SnapshotDependency("demo:parent:1.0.0-SNAPSHOT"))
        ),
        judged(bundle("1.0.0", depends)).map(_.findings) == Right(
          Chunk(Finding.SnapshotDependency("demo:core:2.0.0-SNAPSHOT"))
        ),
      )
    ,
    test("an extra file is layout, and a gav already published stays in the report"):
      val extra = bundle("1.0.0", pom(release)).map: entries =>
        entries :+ ("demo/widget/1.0.0/notes.txt" -> utf8("x"))
      val taken = judged(bundle("1.0.0", pom(release)), Set(gav))
      assertTrue(
        judged(extra).map(_.findings) == Right(Chunk(Finding.Layout("demo/widget/1.0.0/notes.txt"))),
        taken == Right(Report(Chunk(Finding.AlreadyPublished(gav.render)), Chunk(gav))),
      )
    ,
    test("a gav line is three fields and a finding token survives a newline by replacing it"):
      val token = Finding.Layout("a\nb\rc").token
      assertTrue(
        Gav.from("demo:widget:1.0.0") == Some(gav),
        Gav.from("demo.group:widget:1.0.0").map(_.directory) == Some("demo/group/widget/1.0.0"),
        Gav.from("demo:widget") == None,
        Gav.from(":widget:1.0.0") == None,
        Gav.from("demo::1.0.0") == None,
        Gav.from("demo:widget:") == None,
        token == "layout:a_b_c",
        Finding.from(token) == Some(Finding.Layout("a_b_c")),
        Finding.from("empty-pom") == Some(Finding.EmptyPom),
        Finding.from("too-large") == Some(Finding.TooLarge),
        Finding.from("nope") == None,
        Finding.from(":nope") == None,
      )
    ,
    test("render then parse keeps the state and the findings, and an unknown token does not"):
      val findings = Chunk(Finding.Layout("demo/widget"), Finding.EmptyPom, Finding.TooLarge)
      val states   = List(
        DeploymentState.Pending,
        DeploymentState.Validating,
        DeploymentState.Validated,
        DeploymentState.Publishing,
        DeploymentState.Published,
        DeploymentState.Failed,
      )
      ZIO
        .fromEither(DeploymentId.from("deploy-1"))
        .map: id =>
          val misses = states.flatMap: state =>
            val json = PortalReport.render(id, state, findings)
            if PortalReport.parse(json) == Some((state, findings)) && json.contains("deploy-1") then Nil
            else List(state)
          val validated = PortalReport.render(id, DeploymentState.Validated, Chunk.empty)
          assertTrue(
            misses == Nil,
            PortalReport.parse(validated.replace("VALIDATED", "NOPE")) == None,
            PortalReport.parse(validated.replace(""""report":[]""", """"report":["nope"]""")) == None,
          ),
  )

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
