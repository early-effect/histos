package histos.load

import histos.cache.ContentStore
import histos.plugin.{FrozenEnv, StageAssurance, StageContract, StageContractError, StageInputs}
import histos.toolchain.HostCompilerSession

import java.net.{ServerSocket, SocketTimeoutException}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import zio.test.*
import zio.{durationInt, Ref, Scope, ZIO}

object PluginStageInputsSpec extends ZIOSpecDefault:
  private val Macro =
    """|package stage
       |import scala.quoted.*
       |object Stage:
       |  inline def token: String = ${ tokenImpl }
       |  private def tokenImpl(using Quotes): Expr[String] =
       |    val file = java.nio.file.Files.readString(java.nio.file.Path.of("input.txt"))
       |    val env = sys.env.getOrElse("STAGE_VALUE", "")
       |    sys.env.get("STAGE_PROBE").flatMap(_.toIntOption).foreach { port =>
       |      val socket = new java.net.Socket("127.0.0.1", port)
       |      socket.close()
       |    }
       |    Expr(file.trim + ":" + env)
       |""".stripMargin

  private val Use =
    """|package app
       |object Use:
       |  def text: String = stage.Stage.token
       |""".stripMargin

  private val Labels =
    """|package demo
       |import scala.quoted.*
       |object Labels:
       |  inline def here: String = ${ hereImpl }
       |  private def hereImpl(using Quotes): Expr[String] =
       |    import quotes.reflect.*
       |    Expr(Position.ofMacroExpansion.sourceFile.path)
       |""".stripMargin

  private val MainSource =
    """|package demo
       |object Main:
       |  def text: String = Labels.here
       |""".stripMargin

  def spec = suite("plugin stage inputs")(
    test("file and env values change the key and the output") {
      PluginCompileSupport
        .run { (root, _) =>
          for
            ready  <- stageSession(root)
            probe  <- Ref.make(0)
            first  <- staged(root, ready, "alpha", "A", "a", probe)
            second <- staged(root, ready, "beta", "B", "b", probe)
          yield (first, second)
        }
        .map { ran =>
          val (first, second) = ran.value
          assertTrue(
            first.text == "alpha:A",
            second.text == "beta:B",
            first.key != second.key,
          )
        }
    },
    test("a missing required binding is refused before the worker") {
      PluginCompileSupport
        .run { (root, _) =>
          for
            ready  <- stageSession(root)
            probe  <- Ref.make(0)
            file   <- PluginCompileSupport.write(root.resolve("missing/input.txt"), "alpha")
            bytes  <- ZIO.attemptBlocking(Files.readAllBytes(file)).orDie
            frozen <- ZIO.fromEither(PluginCompileSupport.frozen("input.txt", bytes))
            contract = PluginCompileSupport.declared("stage.Stage", frozen, "STAGE_VALUE", "A")
            refused  = contract.copy(inputs = StageInputs(contract.inputs.files, Nil, Nil))
            result <- compileUse(root.resolve("missing"), ready, refused, probe, Map.empty).either
            calls  <- probe.get
          yield (result, calls)
        }
        .map { ran =>
          assertTrue(
            ran.value._2 == 0,
            ran.value._1 == Left(
              PluginCompileError.Stage(StageContractError.MissingBinding(histos.plugin.InputKey.trusted("STAGE_VALUE")))
            ),
          )
        }
    },
    test("caller env does not replace a frozen binding and offline refuses before the probe") {
      PluginCompileSupport
        .run { (root, _) =>
          for
            ready <- stageSession(root)
            open  <- listen
            probe <- Ref.make(0)
            hijack = Map("STAGE_VALUE" -> "hijack", "STAGE_PROBE" -> open.port.toString)
            ran          <- staged(root, ready, "alpha", "A", "caller", probe, hijack)
            contacted    <- open.accepted
            offlineProbe <- Ref.make(0)
            denied       <- compileUse(
              root.resolve("offline"),
              ready,
              ran.contract,
              offlineProbe,
              Map("STAGE_PROBE" -> open.port.toString),
              offline = true,
            ).either
            offlineCalls <- offlineProbe.get
            quiet        <- open.accepted
          yield (ran.text, contacted, denied, offlineCalls, quiet)
        }
        .map { ran =>
          val (text, contacted, denied, calls, quiet) = ran.value
          assertTrue(
            text == "alpha:A",
            !contacted,
            calls == 0,
            !quiet,
            denied == Left(PluginCompileError.Stage(StageContractError.UnsupportedOffline)),
          )
        }
    },
    test("a host-known recipe is stable across scratch roots while offline") {
      PluginCompileSupport
        .run { (root, _) =>
          for
            ready <- stageSession(root)
            left  <- label(root.resolve("left"), ready)
            right <- label(root.resolve("right"), ready)
          yield (left, right)
        }
        .map { ran =>
          assertTrue(ran.value._1 == "src/Main.scala", ran.value._2 == "src/Main.scala")
        }
    },
  ) @@ TestAspect.withLiveClock @@ TestAspect.timeout(10.minutes)

  private final case class Ready(session: HostCompilerSession, macros: Path)

  private final case class Staged(text: String, key: histos.jdk.Sha256, contract: StageContract)

  private final case class Open(port: Int, accepted: ZIO[Any, Nothing, Boolean])

  private def stageSession(root: Path): ZIO[PluginCompileSupport.Env & Scope, PluginCompileError, Ready] =
    for
      store   <- ZIO.service[ContentStore]
      payload <- PluginCompileSupport
        .payload(root.resolve("compiler"), "3.10.1", PluginCompileSupport.Scala3101)
        .mapError(PluginCompileError.Host(_))
      session <- PluginCompileSupport.session(payload, store).mapError(PluginCompileError.Host(_))
      source  <- PluginCompileSupport.write(root.resolve("macro/src/stage/Stage.scala"), Macro)
      _       <- PluginCompiler.compile(
        PluginCompileJob(
          PluginCompileSupport.App,
          session,
          root.resolve("macro"),
          root.resolve("macro"),
          List(source),
          classes = root.resolve("macro/classes"),
          analysis = root.resolve("macro/analysis"),
        )
      )
    yield Ready(session, root.resolve("macro/classes"))

  private def staged(
      root: Path,
      ready: Ready,
      file: String,
      env: String,
      name: String,
      probe: Ref[Int],
      caller: Map[String, String] = Map.empty,
  ): ZIO[PluginCompileSupport.Env & Scope, PluginCompileError, Staged] =
    val base = root.resolve(name)
    for
      input  <- PluginCompileSupport.write(base.resolve("input.txt"), file)
      bytes  <- ZIO.attemptBlocking(Files.readAllBytes(input)).orDie
      frozen <- ZIO.fromEither(PluginCompileSupport.frozen("input.txt", bytes))
      contract = PluginCompileSupport.declared("stage.Stage", frozen, "STAGE_VALUE", env)
      result <- compileUse(base, ready, contract, probe, caller)
      text   <- PluginCompileSupport.invoke(
        base.resolve("classes"),
        PluginCompileSupport.runtimeJars(ready.session) ++ List(ready.macros),
        "app.Use$",
        "text",
      )
    yield Staged(text, result.key, contract)

  private def compileUse(
      base: Path,
      ready: Ready,
      contract: StageContract,
      probe: Ref[Int],
      caller: Map[String, String],
      offline: Boolean = false,
  ): ZIO[PluginCompileSupport.Env & Scope, PluginCompileError, PluginCompileResult] =
    for
      source <- PluginCompileSupport.write(base.resolve("src/app/Use.scala"), Use)
      result <- PluginCompiler.compile(
        PluginCompileJob(
          PluginCompileSupport.App,
          ready.session,
          base,
          base,
          List(source),
          classpath = List(ready.macros),
          classes = base.resolve("classes"),
          analysis = base.resolve("analysis"),
          stages = List(contract),
          offline = offline,
          caller = caller,
          probe = probe.update(_ + 1),
        )
      )
    yield result

  private def label(base: Path, ready: Ready): ZIO[PluginCompileSupport.Env & Scope, PluginCompileError, String] =
    val stage = KnownStageRegistry.contract(KnownStageRegistry.ScalaQuotes)
    for
      known  <- ZIO.fromOption(stage).orElseFail(PluginCompileError.UnknownStage(KnownStageRegistry.ScalaQuotes))
      labels <- PluginCompileSupport.write(base.resolve("src/demo/Labels.scala"), Labels)
      _      <- PluginCompiler.compile(
        PluginCompileJob(
          PluginCompileSupport.App,
          ready.session,
          base,
          base,
          List(labels),
          classes = base.resolve("label-classes"),
          analysis = base.resolve("label-analysis"),
          stages = List(known),
          offline = true,
        )
      )
      main <- PluginCompileSupport.write(base.resolve("src/Main.scala"), MainSource)
      _    <- PluginCompiler.compile(
        PluginCompileJob(
          PluginCompileSupport.App,
          ready.session,
          base,
          base,
          List(main),
          classpath = List(base.resolve("label-classes")),
          classes = base.resolve("classes"),
          analysis = base.resolve("analysis"),
          stages = List(known),
          offline = true,
        )
      )
      text <- PluginCompileSupport.invoke(
        base.resolve("classes"),
        PluginCompileSupport.runtimeJars(ready.session) ++ List(base.resolve("label-classes")),
        "demo.Main$",
        "text",
      )
    yield text

  private def listen: ZIO[Scope, Nothing, Open] =
    ZIO
      .acquireRelease(
        ZIO.attemptBlocking {
          val server = new ServerSocket(0)
          server.setSoTimeout(500)
          server
        }.orDie
      )(server => ZIO.attemptBlocking(server.close()).ignore)
      .map: server =>
        val accepted = ZIO.attemptBlocking {
          try
            val socket = server.accept()
            socket.close()
            true
          catch case _: SocketTimeoutException => false
        }.orDie
        Open(server.getLocalPort, accepted)
end PluginStageInputsSpec
