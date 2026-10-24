package histos.toolchain

import histos.archive.*
import histos.jdk.{
  JdkLock,
  JdkVersion,
  JdkPinError,
  LockError,
  VersionError,
  Sha256,
  Sha256Error
}
import histos.node.*
import histos.workspace.{Workspace, IoFailure, IoFault}

import java.nio.file.Path
import zio.{Chunk, ZIO}
import zio.test.*

object ToolLockSpec extends ZIOSpecDefault:
  private def corpus(name: String) =
    for
      raw <- Live
        .live(zio.System.env("HISTOS_REPO"))
        .mapError(_ => IoFailure(Path.of("."), IoFault.Read))
      root <- ZIO
        .attemptBlocking(Path.of(raw.getOrElse(".")))
        .mapError(_ => IoFailure(Path.of("."), IoFault.Read))
      text <- Workspace.readString(
        root.resolve("engine/test/fixtures/runtime-lock").resolve(name)
      )
    yield text

  private val sha = "ab" * 32
  private val jdk =
    s"major 21\nversion 21.0.2\narchive linux-x86_64 file:///opt/jdk.zip $sha\n"
  private val node =
    s"major 22\nversion 22.14.0\narchive linux-x86_64 https://example.test/node.tar.gz $sha\n"

  private enum CorpusError:
    case Malformed(line: String)
    case Environment
    case Root(raw: String)

  def spec = suite("shared host tool locks")(
    test("generic parsing retains pin errors and preserves archive row order"):
      val codec = ToolLock.Codec[JdkVersion, VersionError](
        (_, version) => JdkVersion.from(version),
        version => (version.major.value.toString, version.value)
      )
      val parsed = ToolLock.parse(jdk, codec)
      assertTrue(
        parsed.map(lock => ToolLock.render(lock, codec)) == Right(jdk),
        parsed.map(_.pin) == JdkLock.parse(jdk).map(_.version),
        ToolLock.parse(jdk.replace("21.0.2", "bad"), codec) == Left(
          ToolLockError.Pin(VersionError.Bad("bad"))
        )
      )
    ,
    test("duplicate and missing headers use the same shared field enum"):
      assertTrue(
        JdkLock.parse(jdk + "major 21\n") == Left(
          LockError.Schema(ToolLockError.Duplicate(ToolLockField.Major))
        ),
        NodeLock.parse(node + "major 22\n") == Left(
          NodeLockError.Schema(ToolLockError.Duplicate(ToolLockField.Major))
        ),
        JdkLock.parse(
          jdk.linesIterator.filterNot(_.startsWith("major ")).mkString("\n")
        ) == Left(LockError.Schema(ToolLockError.Missing(ToolLockField.Major))),
        NodeLock.parse(
          node.linesIterator.filterNot(_.startsWith("version ")).mkString("\n")
        ) == Left(
          NodeLockError.Schema(ToolLockError.Missing(ToolLockField.Version))
        )
      )
    ,
    test(
      "empty, duplicate, unknown, alias and malformed archive tables are rejected"
    ):
      assertTrue(
        JdkLock.parse("major 21\nversion 21.0.2\n") == Left(
          LockError.Schema(ToolLockError.EmptyArchives)
        ),
        JdkLock.parse(
          jdk + s"archive linux-x86_64 file:///different.zip $sha\n"
        ) == Left(
          LockError.Schema(
            ToolLockError.DuplicateHost(HostPlatform.LinuxX86_64)
          )
        ),
        JdkLock.parse(jdk.replace("linux-x86_64", "linux-amd64")) == Left(
          LockError.Schema(
            ToolLockError.Host(HostPlatformError.Unknown("linux-amd64"))
          )
        ),
        JdkLock.parse(jdk.replace("linux-x86_64", "plan9-x86_64")) == Left(
          LockError.Schema(
            ToolLockError.Host(HostPlatformError.Unknown("plan9-x86_64"))
          )
        ),
        JdkLock.parse(
          "major 21\nversion 21.0.2\narchive linux-x86_64 file:///jdk.zip"
        ) == Left(
          LockError.Schema(
            ToolLockError.BadLine("archive linux-x86_64 file:///jdk.zip")
          )
        ),
        JdkLock.parse("major 21\nversion 21.0.2\nurl file:///jdk.zip") == Left(
          LockError.Schema(ToolLockError.UnknownField("url"))
        )
      )
    ,
    test(
      "ASCII full versions, bounded positive majors and hexadecimal checksums remain strict"
    ):
      val versions = List("21.0.2é", "٢١.0.2", "v21.0.2", "21.0.2-extra")
      val invalid = versions.take(3)
      val majors = List("0", "-21", "2147483648", "21.0", "٢١")
      assertTrue(
        invalid.map(version =>
          JdkLock.parse(jdk.replace("21.0.2", version))
        ) == invalid.map(version =>
          Left(
            LockError.Schema(
              ToolLockError.Pin(
                JdkPinError.BadVersion(VersionError.Bad(version))
              )
            )
          )
        ),
        majors.map(major =>
          JdkLock.parse(jdk.replace("major 21", s"major $major"))
        ) == majors.map(major =>
          Left(LockError.Schema(ToolLockError.Pin(JdkPinError.BadMajor(major))))
        ),
        JdkLock.parse(jdk.replace("major 21", "major +21")) == JdkLock.parse(
          jdk
        ),
        JdkLock.parse(jdk.replace(sha, "٠" * 64)) == Left(
          LockError.Schema(ToolLockError.BadSha256(Sha256Error.Bad("٠" * 64)))
        )
      )
    ,
    test("programmatic tables enforce nonempty and unique canonical hosts"):
      for
        version <- ZIO.fromEither(JdkVersion.from("21.0.2"))
        checksum <- ZIO.fromEither(Sha256.from(sha))
        row = HostArchive(
          HostPlatform.LinuxX86_64,
          ArchivePin(ArchiveSource.Local(Path.of("/jdk.zip")), checksum)
        )
      yield assertTrue(
        JdkLock.from(version, Chunk.empty) == Left(ToolLockError.EmptyArchives),
        JdkLock.from(version, Chunk(row, row)) == Left(
          ToolLockError.DuplicateHost(HostPlatform.LinuxX86_64)
        )
      )
    ,
    test(
      "the shared standalone lock corpus has identical header and archive acceptance"
    ):
      for
        text <- corpus("locks.tsv")
        actual <- ZIO.foreach(text.linesIterator.toList): line =>
          line.split("\t", 2).toList match
            case expected :: encoded :: Nil =>
              val raw = encoded.replace("{LF}", "\n").replace("{NUL}", "\u0000")
              ZIO.succeed(
                (
                  expected,
                  JdkLock.parse(raw).fold(_ => "reject", _ => "accept")
                )
              )
            case _ => ZIO.fail(CorpusError.Malformed(line))
      yield assertTrue(actual.map(_._1) == actual.map(_._2))
    ,
    test(
      "the reusable POSIX and Windows URL corpus agrees with the typed source parser"
    ):
      for
        text <- corpus("urls.tsv")
        actual <- ZIO.foreach(text.linesIterator.filter(_.nonEmpty).toList):
          line =>
            line.split("\t", 2).toList match
              case expected :: raw :: Nil =>
                val source = raw
                  .replace("{TAB}", "\t")
                  .replace("{CR}", "\r")
                  .replace("{LF}", "\n")
                  .replace("{NUL}", "\u0000")
                ZIO.succeed(
                  (
                    raw,
                    expected,
                    ArchiveSource
                      .from(source)
                      .fold(_ => "reject", _ => "accept")
                  )
                )
              case _ => ZIO.fail(CorpusError.Malformed(line))
      yield assertTrue(
        actual.map(value => value._1 -> value._3) == actual.map(value =>
          value._1 -> value._2
        )
      )
    ,
    test(
      "B8 every archive corpus row validates before JDK and Node host selection"
    ):
      for
        location <- Live
          .live(zio.System.env("HISTOS_REPO"))
          .mapError(_ => CorpusError.Environment)
        root <- ZIO
          .attemptBlocking(Path.of(location.getOrElse(".")))
          .mapError(_ => CorpusError.Root(location.getOrElse(".")))
        launcherText <- Workspace.readString(
          root.resolve("engine/test/fixtures/launcher-urls.tsv")
        )
        launcherRows <- ZIO.foreach(
          launcherText.linesIterator.filter(_.startsWith("archive\t")).toList
        ): line =>
          line.split("\t", 3).toList match
            case _ :: verdict :: encoded :: Nil =>
              ZIO
                .attempt(
                  new String(
                    java.util.Base64.getDecoder.decode(encoded),
                    java.nio.charset.StandardCharsets.UTF_8
                  )
                )
                .mapError(_ => CorpusError.Malformed(line))
                .map(raw => (verdict == "1", raw))
            case _ => ZIO.fail(CorpusError.Malformed(line))
        runtimeText <- Workspace.readString(
          root.resolve("engine/test/fixtures/runtime-lock/urls.tsv")
        )
        runtimeRows <- ZIO.foreach(
          runtimeText.linesIterator.filter(_.nonEmpty).toList
        ): line =>
          line.split("\t", 2).toList match
            case verdict :: raw :: Nil =>
              ZIO.succeed(
                (
                  verdict == "accept",
                  raw
                    .replace("{TAB}", "\t")
                    .replace("{CR}", "\r")
                    .replace("{LF}", "\n")
                    .replace("{NUL}", "\u0000")
                )
              )
            case _ => ZIO.fail(CorpusError.Malformed(line))
        actual = (launcherRows ++ runtimeRows).map: (accepted, raw) =>
          val table =
            s"archive linux-x86_64 file:///usable.zip $sha\narchive windows-aarch64 $raw $sha\n"
          val jdkResult = JdkLock.parse("major 21\nversion 21.0.2\n" + table)
          val nodeResult = NodeLock.parse("major 22\nversion 22.14.0\n" + table)
          (
            raw,
            accepted,
            ArchiveSource.from(raw).isRight,
            jdkResult.isRight,
            nodeResult.isRight
          )
      yield assertTrue(
        actual
          .filter(row =>
            row._2 != row._3 || row._2 != row._4 || row._2 != row._5
          )
          .isEmpty
      )
    ,
    test(
      "B8 all six hosts retain foreign portable file paths without materialization"
    ):
      val hosts = List(
        "macos-x86_64",
        "macos-aarch64",
        "linux-x86_64",
        "linux-aarch64",
        "windows-x86_64",
        "windows-aarch64"
      )
      val table = hosts
        .map(host =>
          s"archive $host file:///C:/foreign%20path/$host.zip $sha\n"
        )
        .mkString
      val jdkResult = JdkLock.parse("major 21\nversion 21.0.2\n" + table)
      val nodeResult = NodeLock.parse("major 22\nversion 22.14.0\n" + table)
      assertTrue(
        jdkResult.map(_.archives.size) == Right(6),
        nodeResult.map(_.archives.size) == Right(6)
      )
    ,
    test(
      "comments and header order do not change the pin, rendering is stable"
    ):
      val modified =
        "# fixture\n" + jdk.linesIterator.toList.reverse.mkString("\n")
      assertTrue(
        JdkLock.parse(modified) == JdkLock.parse(jdk),
        JdkLock.parse(jdk).map(JdkLock.render) == Right(jdk)
      )
    ,
    test("explicit local override parsing remains strict"):
      assertTrue(
        LocalOverride.parse("") == Left(LocalOverrideError.Empty),
        LocalOverride.parse("relative") == Left(
          LocalOverrideError.Relative("relative")
        ),
        LocalOverride.parse("/one\n/two") == Left(LocalOverrideError.Multiple),
        LocalOverride.parse("/bad\u0000") == Left(
          LocalOverrideError.Invalid("/bad\u0000")
        )
      )
  )
