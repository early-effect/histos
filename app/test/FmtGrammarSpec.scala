package histos.cli

import histos.client.Batch
import histos.format.{FormatMode, FormatTargets}
import histos.server.Call
import histos.ProjectName

import java.nio.file.Path

import zio.NonEmptyChunk
import zio.test.*

object FmtGrammarSpec extends ZIOSpecDefault:
  private val api = ProjectName.trusted("api")
  private val app = ProjectName.trusted("app")

  def spec = suite("fmt grammar")(
    test("no target is the build, names are modules, and --check writes nothing wherever it sits"):
      for
        build   <- Grammar.parse(List("fmt"))
        checked <- Grammar.parse(List("fmt", "--check"))
        named   <- Grammar.parse(List("fmt", "api", "app"))
        late    <- Grammar.parse(List("fmt", "api", "--check"))
        watched <- Grammar.parse(List("~", "fmt", "--check"))
      yield assertTrue(
        build == Command.Fmt(FormatTargets.Build, FormatMode.Write),
        checked == Command.Fmt(FormatTargets.Build, FormatMode.Check),
        named == Command.Fmt(FormatTargets.Projects(NonEmptyChunk(api, app)), FormatMode.Write),
        late == Command.Fmt(FormatTargets.Projects(NonEmptyChunk(api)), FormatMode.Check),
        watched == Command.Watch(checked, List("fmt", "--check")),
        Operation.operation(checked) == Right(Call.Format(FormatTargets.Build, FormatMode.Check)),
        Help.names.contains("fmt"),
      )
    ,
    test("--file makes every target a repository-relative file"):
      for
        files <- Grammar.parse(List("fmt", "--file", "api/src/A.scala", "histos.scala"))
        late  <- Grammar.parse(List("fmt", "--check", "api/src/A.scala", "--file"))
      yield assertTrue(
        files == Command.Fmt(
          FormatTargets.Files(NonEmptyChunk(Path.of("api/src/A.scala"), Path.of("histos.scala"))),
          FormatMode.Write,
        ),
        late == Command.Fmt(FormatTargets.Files(NonEmptyChunk(Path.of("api/src/A.scala"))), FormatMode.Check),
      )
    ,
    test("--all, an unknown flag, --file with nothing, and an absolute file are usage"):
      for
        all      <- Grammar.parse(List("fmt", "--all")).either
        unknown  <- Grammar.parse(List("fmt", "api", "--diff")).either
        nothing  <- Grammar.parse(List("fmt", "--file")).either
        absolute <- Grammar.parse(List("fmt", "--file", "/tmp/A.scala")).either
        badName  <- Grammar.parse(List("fmt", "1api")).either
      yield assertTrue(
        all == Left(CliParseError.Fmt(FmtUsage.Flag("--all"))),
        unknown == Left(CliParseError.Fmt(FmtUsage.Flag("--diff"))),
        nothing == Left(CliParseError.Fmt(FmtUsage.NoFiles)),
        absolute == Left(CliParseError.Fmt(FmtUsage.Absolute("/tmp/A.scala"))),
        badName.isLeft,
        CliExit.from(
          Call.Format(FormatTargets.Build, FormatMode.Check),
          histos.report.CallResult.reported("unformatted"),
        ) ==
          CliExit.TaskFailed,
      )
    ,
    test("the native client batches the build and project forms, and leaves --file to the JVM client"):
      assertTrue(
        Batch.parse(List("fmt")) == Some(Call.Format(FormatTargets.Build, FormatMode.Write)),
        Batch.parse(List("fmt", "--check", "api")) ==
          Some(Call.Format(FormatTargets.Projects(NonEmptyChunk(api)), FormatMode.Check)),
        Batch.parse(List("fmt", "--file", "api/src/A.scala")).isEmpty,
        Cli.callOf(Command.Fmt(FormatTargets.Build, FormatMode.Check)) == Batch.parse(List("fmt", "--check")),
        Cli.callOf(Command.Fmt(FormatTargets.Files(NonEmptyChunk(Path.of("A.scala"))), FormatMode.Write)).isEmpty,
      ),
  )
end FmtGrammarSpec
