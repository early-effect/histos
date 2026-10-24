package histos.testrun

import java.nio.file.Path

import zio.ZIO
import zio.test.*

object ProviderNoteSpec extends ZIOSpecDefault:
  def spec = suite("provider note")(
    test("a note is that child's environment"):
      for
        first <- ZIO.fromEither(SuitePattern.from("alpha.*"))
        second <- ZIO.fromEither(SuitePattern.from("beta?"))
      yield
        val env = ProviderNote("zio.test.sbt.ZTestFramework", Path.of("classes"), List(first, second)).environment
        assertTrue(
          env.get(ProviderRunner.Framework).contains("zio.test.sbt.ZTestFramework"),
          env.get(ProviderRunner.Classes).contains(Path.of("classes").toAbsolutePath.toString),
          env.get(ProviderRunner.PatternCount).contains("2"),
          env.get(s"${ProviderRunner.PatternPrefix}0").contains("alpha.*"),
          env.get(s"${ProviderRunner.PatternPrefix}1").contains("beta?"),
        )
    ,
    test("two cells keep two notes"):
      for
        pattern <- ZIO.fromEither(SuitePattern.from("demo.*"))
      yield
        val api = ProviderNote("zio.test.sbt.ZTestFramework", Path.of("api-classes"), List(pattern)).environment
        val engine = ProviderNote("zio.test.sbt.ZTestFramework", Path.of("engine-classes"), Nil).environment
        assertTrue(
          api.get(ProviderRunner.Classes).contains(Path.of("api-classes").toAbsolutePath.toString),
          engine.get(ProviderRunner.Classes).contains(Path.of("engine-classes").toAbsolutePath.toString),
          api.get(ProviderRunner.PatternCount).contains("1"),
          engine.get(ProviderRunner.PatternCount).contains("0"),
          api.get(ProviderRunner.Classes) != engine.get(ProviderRunner.Classes),
        )
  )
