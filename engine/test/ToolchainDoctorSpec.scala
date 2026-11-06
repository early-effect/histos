package histos.doctor

import histos.jdk.{Machine, Os}
import histos.native.NativeOrigin
import histos.node.{NodeToolchain, NodeVersion}
import histos.toolchain.{Architecture, Survey, SurveyError}
import histos.workspace.{IoFailure, IoFault, Workspace}
import histos.{
  ClangMajor,
  Js,
  Jvm,
  Native,
  NodeFloor,
  Projects,
  RunnerTools,
  ScalaVersion,
  ToolRevision,
  module,
}

import java.nio.file.{Files, Path}
import java.nio.file.attribute.PosixFilePermissions

import zio.{IO, ZIO}
import zio.test.*

object ToolchainDoctorSpec extends ZIOSpecDefault:
  private val scala3 = ScalaVersion("3.10.0")

  def spec = suite("toolchain doctor")(
    test("a shadowed node warns with export HISTOS_NODE and the floor names how to install"):
      ZIO.scoped:
        for
          root  <- Workspace.work("histos-doctor-shadow")
          older <- node(root.resolve("old"), "v22.14.0")
          newer <- node(root.resolve("new"), "v25.2.0")
          graph <- ZIO.fromEither(Projects(module("web", Js)(scala3)))
          host  = onPath(root, older.resolve("bin"), newer.resolve("bin"))
          floor <- ToolchainChecks.nodeFloor(host, graph)
          shade <- ToolchainChecks.nodeShadow(host)
          newerPath <- ZIO.attemptBlocking(
            newer.resolve("bin").resolve("node").toRealPath().normalize.toString
          )
        yield
          val found  = (floor ++ shade).collect { case Outcome.Found(diagnostic) => diagnostic }
          val shadow = found.collect { case diagnostic: ToolchainDiagnostic.NodeShadowed => diagnostic }
          assertTrue(
            found.map(_.check.value) == List("node.floor", "node.shadowed"),
            found.head.summary.contains("HISTOS_NODE"),
            found.head.summary.contains(".histos/node.local"),
            found.head.summary.contains("https://nodejs.org/"),
            found.head.summary.contains("older than 25"),
            found.head.explain.contains("HISTOS_NODE"),
            shadow.head.level == Level.Warning,
            shadow.head.fixes.head match
              case Fix.Command(line) => line.value == s"export HISTOS_NODE=$newerPath"
              case _                 => false,
          )
    ,
    test("runnerTools cover and agree name the call to add"):
      val low = RunnerTools(Some(NodeFloor(22)), Some(ToolRevision("20.1.8")), None, None)
      val web = module("web", Js)(scala3)
      val native = module("core", Native)(scala3).clang(ClangMajor(21))
      val empty = Projects(web, native)
      val raised = Projects.runnerTools(low)(web, native)
      val good = Projects.runnerTools(
        RunnerTools(Some(NodeFloor(25)), Some(ToolRevision("21.1.8")), None, None)
      )(web, native)
      assertTrue(
        empty.exists(graph =>
          ToolchainChecks.cover(graph) match
            case Outcome.Found(diagnostic) =>
              val shape =
                ".runnerTools(RunnerTools(Some(NodeFloor(25)), Some(ToolRevision(\"21.0.0\")), None))"
              val fix = diagnostic.fixes.head match
                case Fix.Steps(items) => items.head == shape
                case _                => false
              diagnostic.check.value == "runner-tools.cover" && diagnostic.summary.contains(shape) && fix
            case _ => false
        ),
        raised.exists(graph =>
          ToolchainChecks.agree(graph).collect { case Outcome.Found(diagnostic) => diagnostic.summary } match
            case node :: llvm :: Nil =>
              node.contains("NodeFloor(22)") &&
              node.contains("NodeFloor(25)") &&
              node.contains(".runnerTools(RunnerTools(") &&
              llvm.contains("ClangMajor(21)") &&
              llvm.contains("ToolRevision(\"21.1.8\")")
            case _ => false
        ),
        good.exists(graph =>
          ToolchainChecks.cover(graph) == Outcome.Ok(ToolchainChecks.coverId, "runnerTools covers the modules") &&
          ToolchainChecks.agree(graph) == List(Outcome.Ok(ToolchainChecks.agreeId, "runnerTools agree with the modules"))
        ),
      )
    ,
    test("a Jvm graph does not require Node or clang"):
      ZIO.scoped:
        for
          root  <- Workspace.work("histos-doctor-jvm")
          home  <- jdk(root.resolve("jdk"), "21.0.4")
          graph <- ZIO.fromEither(Projects(module("core", Jvm)(scala3)))
          host  = machine(root, Some(home), Map.empty)
          floor <- ToolchainChecks.nodeFloor(host, graph)
          clang <- ToolchainChecks.clang(host, graph)
          jdkRead <- ToolchainChecks.jdk(host)
        yield assertTrue(
          floor == List(Outcome.Ok(ToolchainChecks.nodeFloorId, "no Js module")),
          clang == List(Outcome.Ok(ToolchainChecks.clangId, "no Native module")),
          jdkRead.exists {
            case Outcome.Ok(id, note) =>
              id == ToolchainChecks.jdkId && note.contains("21.0.4") && note.contains("from java.home")
            case _ => false
          },
        )
    ,
    test("the survey keeps going after the JDK fails"):
      ZIO.scoped:
        for
          root    <- Workspace.work("histos-survey-all")
          running <- jdk(root.resolve("running"), "21.0.4")
          ancient <- jdk(root.resolve("ancient"), "17.0.2")
          home    <- node(root.resolve("node"), "v25.2.0")
          _       <- Workspace.writeString(root.resolve(".histos/toolchain.local"), ancient.toString + "\n")
          graph   <- ZIO.fromEither(Projects(module("web", Js)(scala3)))
          host    = machine(root, Some(running), Map("PATH" -> home.resolve("bin").toString))
          result  <- Survey.read(host, graph).either
        yield
          val report = result.left.toOption.collect { case SurveyError.Incomplete(text, detail) => (text, detail) }
          assertTrue(
            report.exists { (text, detail) =>
              text.contains("17.0.2") &&
              text.contains(".histos/toolchain.local") &&
              text.contains("node 25.2.0") &&
              text.contains("from PATH") &&
              text.contains("clang not required") &&
              detail.contains("17.0.2") &&
              !detail.contains("node 25.2.0")
            }
          )
    ,
    test("a missing node does not hide clang"):
      ZIO.scoped:
        for
          root  <- Workspace.work("histos-survey-clang")
          home  <- jdk(root.resolve("jdk"), "21.0.4")
          clang <- clang(root.resolve("clang"), "21.1.8")
          graph <- ZIO.fromEither(
            Projects(module("web", Js)(scala3), module("core", Native)(scala3).clang(ClangMajor(21)))
          )
          host = machine(root, Some(home), Map("PATH" -> clang.toString))
          text <- Survey.collect(host, graph).map(_.text)
        yield assertTrue(
          text.contains("from java.home"),
          text.contains("HISTOS_NODE"),
          text.contains(".histos/node.local"),
          text.contains("https://nodejs.org/"),
          text.contains("chosen clang 21.1.8"),
          text.contains("from PATH"),
        )
    ,
    test("--explain node.floor is the long form"):
      val text = ToolchainExplain.text("node.floor")
      assertTrue(
        text.exists(body =>
          body.contains("HISTOS_NODE") &&
          body.contains(".histos/node.local") &&
          body.contains("https://nodejs.org/") &&
          body.contains("runnerTools.nodePath")
        ),
        ToolchainExplain.text("no.such").isEmpty,
        ToolchainExplain.known.contains("runner-tools.cover"),
        ToolchainExplain.known.contains("node.shadowed"),
      )
    ,
    test("a node below the floor is shadowed only when a later one is newer"):
      val older = NodeVersion.from("22.14.0").toOption.get
      val mid   = NodeVersion.from("24.1.0").toOption.get
      val newer = NodeVersion.from("25.2.0").toOption.get
      val old   = histos.node.NodeSight(Path.of("/old/bin/node"), older)
      val next  = histos.node.NodeSight(Path.of("/mid/bin/node"), mid)
      val best  = histos.node.NodeSight(Path.of("/new/bin/node"), newer)
      val fresh = histos.node.NodeSight(Path.of("/fresh/bin/node"), newer)
      assertTrue(
        NodeToolchain.shadow(List(old, next, best)).exists(_.newer.binary == best.binary),
        NodeToolchain.shadow(List(fresh, old)).isEmpty,
        NodeToolchain.shadow(List(old)).isEmpty,
        ToolchainChecks.exportLine(Path.of("/usr/local/bin/node")) == "export HISTOS_NODE=/usr/local/bin/node",
      )
    ,
  ) @@ TestAspect.withLiveClock @@ TestAspect.withLiveSystem

  private def machine(root: Path, running: Option[Path], env: Map[String, String]): Machine =
    Machine(
      env,
      root.toString,
      Os.Linux,
      Architecture.X86_64,
      root.resolve("cache"),
      root,
      running,
      NativeOrigin.Central,
    )

  private def onPath(root: Path, first: Path, rest: Path*): Machine =
    machine(root, None, Map("PATH" -> (first :: rest.toList).mkString(java.io.File.pathSeparator)))

  private def node(home: Path, version: String): IO[IoFailure, Path] =
    val file = home.resolve("bin").resolve("node")
    for
      _ <- Workspace.createDirectories(file.getParent)
      _ <- Workspace.writeString(file, s"#!/bin/sh\nprintf '%s\\n' '$version'\n")
      _ <- executable(file)
    yield home

  private def jdk(home: Path, version: String): IO[IoFailure, Path] =
    Workspace.createDirectories(home.resolve("bin")) *>
      Workspace.writeString(home.resolve("bin/javac"), "javac") *>
      Workspace.writeString(home.resolve("release"), s"JAVA_VERSION=\"$version\"\n").as(home)

  private def clang(dir: Path, version: String): IO[IoFailure, Path] =
    val text =
      s"""#!/bin/sh
         |printf '%s\\n' 'clang version $version'
         |printf '%s\\n' 'Target: x86_64-unknown-linux-gnu'
         |""".stripMargin
    for
      _ <- Workspace.createDirectories(dir)
      _ <- Workspace.writeString(dir.resolve("clang"), text)
      _ <- Workspace.writeString(dir.resolve("clang++"), text)
      _ <- executable(dir.resolve("clang"))
      _ <- executable(dir.resolve("clang++"))
    yield dir

  private def executable(file: Path): IO[IoFailure, Unit] =
    ZIO
      .attemptBlocking(Files.setPosixFilePermissions(file, PosixFilePermissions.fromString("rwxr-xr-x")))
      .mapError(_ => IoFailure(file, IoFault.Write))
      .unit
end ToolchainDoctorSpec
