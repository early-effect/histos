package histos

import histos.doctor.{Fix, FixApply}
import histos.jdk.Machine
import histos.native.NativeOrigin
import histos.supply.{DoctorSurface, Net, Reply}
import histos.toolchain.Architecture
import histos.workspace.Workspace

import zio.*
import zio.test.*

/** The survey runs in process. The net answers 404 and is never a socket. */
object DoctorSurfaceLawSpec extends ZIOSpecDefault:
  private val scala3 = scala"3.10.0"

  private val emptyJson = """{"exit":0,"timedOut":[],"findings":[]}"""

  def spec = suite("doctor surface laws")(
    test("preflight skips the survey, an unknown area is empty, and a named area keeps only that area"):
      ZIO.scoped:
        Workspace
          .work("histos-doctor-surface")
          .flatMap: repo =>
            val machine = Machine(
              Map.empty,
              "/home/demo",
              histos.jdk.Os.Mac,
              Architecture.Aarch64,
              repo,
              repo,
              None,
              NativeOrigin.Central,
            )
            Projects(module("core", Jvm)(scala3)) match
              case Left(_)      => ZIO.succeed(assertTrue(Projects(module("core", Jvm)(scala3)).isRight))
              case Right(graph) =>
                def ask(fix: Fix): UIO[FixApply.Answer] = ZIO.dieMessage("doctor surface asked")
                def exec(line: String): UIO[Unit]       = ZIO.dieMessage(s"doctor surface ran $line")
                def setting(url: String): UIO[Unit]     = ZIO.dieMessage(s"doctor surface wrote $url")
                def run(request: DoctorSurface.Request) =
                  DoctorSurface.runWith(
                    repo,
                    graph,
                    request,
                    machine,
                    Closed,
                    Map.empty,
                    None,
                    false,
                    ask,
                    exec,
                    setting,
                  )
                for
                  preflight <- run(DoctorSurface.Request(None, false, Some("nightly"), false, false))
                  toolchain <- run(DoctorSurface.Request(Some("toolchain"), false, None, false, false))
                  supply    <- run(DoctorSurface.Request(Some("supply-chain"), false, None, false, false))
                  missing   <- run(DoctorSurface.Request(Some("missing"), true, None, false, false))
                yield assertTrue(
                  preflight._2.contains("nightly preflight"),
                  !preflight._2.contains("no Js module"),
                  !preflight._2.contains("every uses is pinned"),
                  toolchain._2.contains("no Js module"),
                  toolchain._2.contains("no Native module"),
                  !toolchain._2.contains("every uses is pinned"),
                  !toolchain._2.contains("nightly preflight"),
                  supply._2.contains("every uses is pinned"),
                  !supply._2.contains("no Js module"),
                  missing._2 == emptyJson,
                )
  )
end DoctorSurfaceLawSpec

private object Closed extends Net:
  def send(
      method: String,
      url: String,
      body: String,
      headers: List[(String, String)],
  ): zio.IO[histos.supply.SupplyFault, Reply] =
    ZIO.succeed(Reply(404, ""))
