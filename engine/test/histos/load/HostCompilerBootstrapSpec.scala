package histos.load

import histos.cache.ContentStore
import histos.toolchain.HostToolRole

import java.nio.file.{Files, Path}

import zio.test.*
import zio.{durationInt, ZIO}

object HostCompilerBootstrapSpec extends ZIOSpecDefault:
  private val Marker =
    """|package demo;
       |public final class Marker {
       |  private Marker() {}
       |  public static String token() { return "from-java"; }
       |}
       |""".stripMargin

  private val Bridge =
    """|package demo
       |object Bridge:
       |  def text: String = Marker.token()
       |""".stripMargin

  def spec = suite("host compiler bootstrap")(
    test("a missing bridge is a host error and does not solve") {
      PluginCompileSupport
        .run { (root, _) =>
          for
            lock  <- lockBytes
            store <- ZIO.service[ContentStore]
            built <- PluginCompileSupport
              .payload(
                root.resolve("gap"),
                "3.10.1",
                PluginCompileSupport.Scala3101.filterNot(_.role == HostToolRole.Bridge),
              )
              .mapError(PluginCompileError.Host(_))
            opened <- PluginCompileSupport.session(built, store).either
            again  <- lockBytes
          yield (opened, lock.sameElements(again))
        }
        .map { ran =>
          assertTrue(
            ran.starts == 0,
            ran.value._2,
            ran.value._1 == Left(histos.toolchain.HostCompilerError.EntryAbsent(HostToolRole.Bridge)),
          )
        }
    },
    test("compiler B rebuilds a mixed recipe when the project lock has no B cell") {
      PluginCompileSupport
        .run { (root, _) =>
          for
            before   <- lockBytes
            text     <- lockText
            store    <- ZIO.service[ContentStore]
            payloadA <- PluginCompileSupport
              .payload(root.resolve("a-payload"), "3.10.1", PluginCompileSupport.Scala3101)
              .mapError(PluginCompileError.Host(_))
            sessionA  <- PluginCompileSupport.session(payloadA, store).mapError(PluginCompileError.Host(_))
            java      <- PluginCompileSupport.write(root.resolve("src/demo/Marker.java"), Marker)
            scala     <- PluginCompileSupport.write(root.resolve("src/demo/Bridge.scala"), Bridge)
            compiledA <- compile(root.resolve("out-a"), sessionA, List(java, scala))
            textA     <- PluginCompileSupport.invoke(
              root.resolve("out-a/classes"),
              PluginCompileSupport.runtimeJars(sessionA),
              "demo.Bridge$",
              "text",
            )
            payloadB <- PluginCompileSupport
              .payload(root.resolve("b-payload"), "3.8.4", PluginCompileSupport.Scala384)
              .mapError(PluginCompileError.Host(_))
            sessionB  <- PluginCompileSupport.session(payloadB, store).mapError(PluginCompileError.Host(_))
            compiledB <- compile(root.resolve("out-b"), sessionB, List(java, scala))
            textB     <- PluginCompileSupport.invoke(
              root.resolve("out-b/classes"),
              PluginCompileSupport.runtimeJars(sessionB),
              "demo.Bridge$",
              "text",
            )
            after <- lockBytes
          yield (
            text,
            before.sameElements(after),
            compiledA.snapshot.version.value,
            compiledB.snapshot.version.value,
            textA,
            textB,
          )
        }
        .map { ran =>
          val (text, same, versionA, versionB, textA, textB) = ran.value
          assertTrue(
            same,
            text.isEmpty || PluginCompileSupport.compilerCell(text, "3.10.1"),
            !PluginCompileSupport.compilerCell(text, "3.8.4"),
            !text.contains("scala3-compiler_3 3.8.4"),
            versionA == "3.10.1",
            versionB == "3.8.4",
            textA == "from-java",
            textB == "from-java",
          )
        }
    },
  ) @@ TestAspect.withLiveClock @@ TestAspect.timeout(10.minutes)

  private def compile(
      base: Path,
      session: histos.toolchain.HostCompilerSession,
      sources: List[Path],
  ): ZIO[PluginCompileSupport.Env & zio.Scope, PluginCompileError, PluginCompileResult] =
    Option(base.getParent) match
      case None =>
        ZIO.fail(PluginCompileError.Io(histos.workspace.IoFailure(base, histos.workspace.IoFault.Stat)))
      case Some(parent) =>
        PluginCompiler.compile(
          PluginCompileJob(
            PluginCompileSupport.App,
            session,
            parent,
            parent,
            sources,
            classes = base.resolve("classes"),
            analysis = base.resolve("analysis"),
          )
        )

  private def lockBytes: ZIO[Any, PluginCompileError, Array[Byte]] =
    PluginCompileSupport.lockFile match
      case None       => ZIO.succeed(Array.emptyByteArray)
      case Some(path) =>
        ZIO
          .attemptBlocking(Files.readAllBytes(path))
          .mapError(_ => PluginCompileError.Io(histos.workspace.IoFailure(path, histos.workspace.IoFault.Read)))

  private def lockText: ZIO[Any, PluginCompileError, String] =
    PluginCompileSupport.lockFile match
      case None       => ZIO.succeed("")
      case Some(path) =>
        ZIO
          .attemptBlocking(Files.readString(path))
          .mapError(_ => PluginCompileError.Io(histos.workspace.IoFailure(path, histos.workspace.IoFault.Read)))
end HostCompilerBootstrapSpec
