package histos.format

import histos.ScalafmtConf
import histos.plugin.RepoPath
import histos.workspace.Workspace

import java.nio.file.{Files, Path}

import zio.ZIO
import zio.test.*

object ScalafmtVersionLawSpec extends ZIOSpecDefault:
  private val conf = Path.of(".scalafmt.conf")

  private val number   = Gen.int(0, 40).map(_.toString)
  private val suffix   = Gen.option(Gen.elements("-RC1", "-M2", "-RC7"))
  private val released = (Gen.int(3, 9) <*> number <*> number <*> suffix).map((major, minor, patch, tag) =>
    s"$major.$minor.$patch${tag.getOrElse("")}"
  )
  private val older = (Gen.int(0, 2) <*> number <*> number).map((major, minor, patch) => s"$major.$minor.$patch")
  private val junk  = Gen.elements("", "latest", "3", "3.11", "v3.11.5", "3.11.5.1", "3.x.0", "3.11.5 # pinned")

  private val layouts: List[String => String] = List(
    raw => s"version = \"$raw\"",
    raw => s"version=$raw",
    raw => s"version: \"$raw\"",
    raw => s"  version = \"$raw\"   # pinned",
    raw => s"version = \"$raw\" // pinned",
  )

  def spec = suite("scalafmt version")(
    test("a released 3.x version is that version, and an older one is unsupported"):
      check(released, older): (fresh, old) =>
        assertTrue(
          ScalafmtVersion.from(conf, fresh).map(_.value) == Right(fresh),
          ScalafmtVersion.from(conf, old) == Left(ScalafmtConfError.Unsupported(conf, old)),
        )
    ,
    test("text that is not major.minor.patch is a bad version"):
      check(junk): raw =>
        assertTrue(ScalafmtVersion.from(conf, raw) == Left(ScalafmtConfError.BadVersion(conf, raw)))
    ,
    test("every top-level layout names the version"):
      check(released, Gen.elements(layouts*)): (raw, layout) =>
        val text = s"runner.dialect = scala3\n${layout(raw)}\nmaxColumn = 120\n"
        assertTrue(ScalafmtConfText.version(text).contains(raw))
    ,
    test("the last top-level line wins, and a nested, quoted, or commented version is not the version"):
      val text =
        """version = "3.8.0"
          |project {
          |  version = "9.9.9"
          |}
          |# version = "1.0.0"
          |// version = "1.0.1"
          |docstrings.style = "version = 2.0.0"
          |version = "3.11.5"
          |fileOverride {
          |  "glob:**/scala-2.13*/**" { version = "0.0.1" }
          |}
          |""".stripMargin
      assertTrue(
        ScalafmtConfText.version(text).contains("3.11.5"),
        ScalafmtConfText.version("maxColumn = 120\nproject { version = \"3.0.0\" }\n").isEmpty,
      )
    ,
    test("the declared configuration is read from the repository"):
      Workspace
        .work("histos-scalafmt-version")
        .flatMap: repo =>
          val nested = RepoPath("config/scalafmt.conf")
          for
            missing <- ScalafmtVersion.of(repo, ScalafmtConf.Root).either
            _       <- ZIO.attempt(Files.writeString(repo.resolve(".scalafmt.conf"), "maxColumn = 120\n")).orDie
            bare    <- ScalafmtVersion.of(repo, ScalafmtConf.Root).either
            _       <- ZIO.attempt(Files.writeString(repo.resolve(".scalafmt.conf"), "version = \"2.7.5\"\n")).orDie
            old     <- ScalafmtVersion.of(repo, ScalafmtConf.Root).either
            _       <- ZIO.attempt(Files.createDirectories(repo.resolve("config"))).orDie
            _       <- ZIO.attempt(Files.writeString(repo.resolve("config/scalafmt.conf"), "version = 3.11.5\n")).orDie
            found   <- ScalafmtVersion.of(repo, ScalafmtConf.At(nested)).map(_.value).either
          yield assertTrue(
            missing == Left(ScalafmtConfError.Missing(conf)),
            bare == Left(ScalafmtConfError.NoVersion(conf)),
            old == Left(ScalafmtConfError.Unsupported(conf, "2.7.5")),
            found == Right("3.11.5"),
          ),
  )
end ScalafmtVersionLawSpec
