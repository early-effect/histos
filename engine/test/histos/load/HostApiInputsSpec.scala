package histos.load

import histos.cache.ContentStore
import histos.jdk.{JdkMajor, JdkRuntime, JdkVersion, Sha256}
import histos.suite.HistosSpec
import histos.toolchain.RuntimeIdentity
import histos.workspace.Workspace
import histos.zinc.{CompileInput, CompileInputs, CompileKey, CompileRequest, CompileRow, CompileToolchain}
import histos.{Platform, ScalacFlags, ScalaVersion}

import java.nio.file.{Files, Path}
import java.time.Instant

import zio.test.*
import zio.{Scope, ZIO}

object HostApiInputsSpec extends HistosSpec:
  private val jdk  = JdkVersion.trusted("25", JdkMajor.trusted(25))
  private val salt = Sha256.trusted("ab" * 32)

  def spec = suite("host api inputs")(
    test("substituted api bytes are part of the compile key") {
      val sources = Sha256.trusted("cc" * 32)
      val left    = request(List(Sha256.trusted("aa" * 32)))
      val right   = request(List(Sha256.trusted("bb" * 32)))
      assertTrue(CompileKey.of(left, sources) != CompileKey.of(right, sources))
    },
    test("remember stamps the analysis with the test clock") {
      val when = Instant.parse("2024-06-01T00:00:00Z")
      ZIO.scoped:
        for
          root <- ZIO.attemptBlocking(Files.createTempDirectory("histos-remember")).orDie
          _    <- ZIO.addFinalizer(ZIO.attemptBlocking(Workspace.deleteSync(root)).orDie)
          _    <- ZIO.attemptBlocking(Files.createDirectories(root.resolve("cas"))).orDie
          store = ContentStore.in(root.resolve("cas"))
          _ <- TestClock.setTime(when)
          analysis = root.resolve("analysis")
          remembered <- CompileInputs
            .remember(store, analysis, List(CompileRow(CompileInput.Sources, "abc")))
            .either
          listed <- store.list.either
        yield
          val slot = CompileInputs.slot(analysis)
          assertTrue(
            remembered.isRight,
            listed.exists(
              _.exists(entry => entry.digest == slot && entry.lastUsed.getEpochSecond == when.getEpochSecond)
            ),
          )
    },
  )

  private def request(api: List[Sha256]): CompileRequest =
    CompileRequest(
      CompileToolchain(
        ScalaVersion("3.10.1"),
        Platform.Jvm,
        JdkRuntime(Path.of("/jdk"), jdk, RuntimeIdentity.Local(Path.of("/jdk"), salt)),
        ScalacFlags.empty,
      ),
      Nil,
      Nil,
      Path.of("/out"),
      Path.of("/analysis"),
      Nil,
      Nil,
      checkout = Path.of("/repo"),
      apiBytes = api,
    )
end HostApiInputsSpec
