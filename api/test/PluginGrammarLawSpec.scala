package histos.plugin

import zio.test.*

object PluginGrammarLawSpec extends ZIOSpecDefault:
  private val samples = List("", "a/b", "1ab", "has_under", "echo", "ab-c")

  def spec = suite("plugin grammar laws")(
    test("every simple name agrees on empty, a bad token, and a kept token"):
      val expected = List(
        ""          -> "Empty",
        "a/b"       -> "Bad",
        "1ab"       -> "Bad",
        "has_under" -> "Bad",
        "echo"      -> "echo",
        "ab-c"      -> "ab-c",
      )
      assertTrue(
        samples.forall: raw =>
          val want = expected.collectFirst { case (sample, tag) if sample == raw => tag }.getOrElse("missing")
          parsers.forall(parse => parse(raw) == want)
      )
    ,
    test("an environment name and a secret name are the env token, and an input key has no whitespace"):
      assertTrue(
        EnvName.from("_A1").map(_.value) == Right("_A1"),
        EnvName.from("") == Left(EnvNameError.Empty),
        EnvName.from("1A") == Left(EnvNameError.Bad("1A")),
        EnvName.from("A-B") == Left(EnvNameError.Bad("A-B")),
        SecretName.from("_A1").map(_.value) == Right("_A1"),
        SecretName.from("") == Left(SecretNameError.Empty),
        SecretName.from("1A") == Left(SecretNameError.Bad("1A")),
        SecretName.from("A-B") == Left(SecretNameError.Bad("A-B")),
        InputKey.from("a/b").map(_.value) == Right("a/b"),
        InputKey.from("") == Left(InputKeyError.Empty),
        InputKey.from("a b") == Left(InputKeyError.Bad("a b")),
        NoteText.from("a b").map(_.value) == Right("a b"),
        NoteText.from("") == Left(NoteTextError.Empty),
        NoteText.from("a\nb") == Left(NoteTextError.Bad("a\nb")),
        PluginDetail.from("a b").map(_.value) == Right("a b"),
        PluginDetail.from("") == Left(PluginDetailError.Empty),
        PluginDetail.from("a\nb") == Left(PluginDetailError.Bad("a\nb")),
      )
    ,
    test("an action name is owner and repo, and a hidden segment is not dotdot"):
      val kept = ".github/workflows"
      assertTrue(
        ActionName.from(kept).map(_.value) == Right(kept),
        ActionName.from("aws-actions/configure-aws-credentials").map(_.value) ==
          Right("aws-actions/configure-aws-credentials"),
        ActionName.from("") == Left(ActionNameError.Empty),
        ActionName.from("/actions") == Left(ActionNameError.Bad("/actions")),
        ActionName.from("actions/") == Left(ActionNameError.Bad("actions/")),
        ActionName.from("actions//checkout") == Left(ActionNameError.Bad("actions//checkout")),
        ActionName.from("..") == Left(ActionNameError.Bad("..")),
      )
    ,
    test("a cron is five GitHub fields, and a field refuses the bound, an inverted range, and a third slash"):
      assertTrue(
        Cron.from("0 0 * * *") == Right(Cron("0 0 * * *")),
        Cron.from("0  0  *  *  *") == Right(Cron("0 0 * * *")),
        Cron.from("*/15 * * * MON-FRI").map(_.value) == Right("*/15 * * * MON-FRI"),
        Cron.from("0 0 * JAN SUN").map(_.value) == Right("0 0 * JAN SUN"),
        Cron.from("0 6 * jan SUN").map(_.value) == Right("0 6 * jan SUN"),
        Cron.from("0 6 * * SUN-MON").map(_.value) == Right("0 6 * * SUN-MON"),
        Cron.from("0,30 6 1-15/2 * MON,FRI").map(_.value) == Right("0,30 6 1-15/2 * MON,FRI"),
        Cron.from("") == Left(CronError.Empty),
        Cron.from("   ") == Left(CronError.Empty),
        Cron.from("0 0 * *") == Left(CronError.Fields(4)),
        Cron.from("0 6 * * 1 2") == Left(CronError.Fields(6)),
        Cron.from("60 * * * *") == Left(CronError.Field(0, "60")),
        Cron.from("*/0 * * * *") == Left(CronError.Field(0, "*/0")),
        Cron.from("1/2/3 0 * * *") == Left(CronError.Field(0, "1/2/3")),
        Cron.from("0 24 * * *") == Left(CronError.Field(1, "24")),
        Cron.from("0 6 0 * *") == Left(CronError.Field(2, "0")),
        Cron.from("0 6 32 * *") == Left(CronError.Field(2, "32")),
        Cron.from("0 6 * 0 *") == Left(CronError.Field(3, "0")),
        Cron.from("0 0 * * 7") == Left(CronError.Field(4, "7")),
        Cron.from("0 6 * * MON-SUN") == Left(CronError.Field(4, "MON-SUN")),
        Cron.from("0 6 * * 5-1") == Left(CronError.Field(4, "5-1")),
      ),
  )

  private val parsers: List[String => String] = List(
    raw => mark(CommandName.from(raw))(_.value),
    raw => mark(JobName.from(raw))(_.value),
    raw => mark(OutputName.from(raw))(_.value),
    raw => mark(ParamName.from(raw))(_.value),
    raw => mark(FlagName.from(raw))(_.value),
    raw => mark(NoteKey.from(raw))(_.value),
    raw => mark(GroupName.from(raw))(_.value),
    raw => mark(RunnerName.from(raw))(_.value),
    raw => mark(EnvironmentName.from(raw))(_.value),
    raw => mark(TargetName.from(raw))(_.value),
    raw => mark(TaskName.from(raw))(_.value),
  )

  private def mark[E <: Product, A](parsed: Either[E, A])(value: A => String): String =
    parsed match
      case Left(error)  => error.productPrefix
      case Right(found) => value(found)
end PluginGrammarLawSpec
