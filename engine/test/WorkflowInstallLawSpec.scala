package histos

import histos.publish.{DistLayout, DistPlatform}
import histos.version.ShipName
import histos.workflow.*

import zio.test.*

/** The release install file exists only for one non-snapshot ship that owns an asset, a native module named `client`
  * with a main, and an LLVM pin that has both install archives. No graph grows a toolchain job from its scm.
  */
object WorkflowInstallLawSpec extends ZIOSpecDefault:
  private val scala3 = scala"3.10.0"

  given CiBranch = CiBranch("main")

  private val finished = SonaProbe(ReleaseMode.Automatic)

  private val linux = "b3b7f2801d15d50736acea3c73982994d025b01c2f035b91ae3b49d1b575732b"

  private val mac = "b95bdd32a33a81ee4d40363aaeb26728a26783fcef26a4d80f65457433ea4669"

  private val own = Scm(
    url"https://github.com/early-effect/histos",
    connection"scm:git:git@github.com:early-effect/histos.git",
  )

  def spec = suite("workflow install laws")(
    test("one release asset and a native client emit the install jobs and both digests"):
      val app      = demoApp
      val rendered = assemble(
        ToolRevision("21.1.8"),
        Some(own),
        ships(Ship("demo").next(1, 2, 3)(app)),
        app,
        linkedClient,
      ).flatMap(Workflow.renderAll(_, finished))
      val release = text(rendered, Workflow.releaseName)
      val ci      = text(rendered, Workflow.fileName)
      val missing = List(
        "  client:",
        "  install:",
        "    needs: client",
        "    needs: [release-demo]",
        """- run: "./histos client"""",
        "gh release upload dist",
        "version=1.2.3",
        s"sha256: $linux",
        s"sha256: $mac",
        "llvm checksum mismatch",
        "LD_LIBRARY_PATH=$dir/lib",
        "clang-sdk",
        "llvmorg-21.1.8",
        "      actions: write",
        "          name: histos-jar",
        "          name: shim",
        "- runner: ubuntu-26.04",
        "- runner: macos-26",
      ).filterNot(release.contains)
      val ciMissing = List(
        """node-version: "25"""",
        "playwright@1.56.0",
        "install firefox",
        "archive.ubuntu.com",
        "timeout 180s",
      ).filterNot(ci.contains)
      assertTrue(
        fault(rendered) == "",
        missing == Nil,
        ciMissing == Nil,
        !release.contains("histos-bootstrap"),
        !release.contains("needs.toolchain.outputs.built"),
        !ci.contains("\n  toolchain:"),
        !ci.contains("dist/histos.jar"),
        !ci.contains("./histos client"),
      )
    ,
    test("the install matrix asks for the mac archive, and a pin that lacks it fails before any text"):
      val app      = demoApp
      val rendered = assemble(
        ToolRevision("20.1.8"),
        None,
        ships(Ship("demo").next(1, 2, 3)(app)),
        app,
        linkedClient,
      ).flatMap(Workflow.renderAll(_, finished))
      assertTrue(rendered == Left(CiError.LlvmChecksum("LLVM-20.1.8-macOS-ARM64.tar.xz")))
    ,
    test("two asset ships publish both and do not invent an install job"):
      val app      = demoApp
      val extra    = extraApp
      val rendered = assemble(
        ToolRevision("21.1.8"),
        None,
        ships(Ship("demo").next(1, 2, 3)(app), Ship("other").next(1, 2, 3)(extra)),
        app,
        extra,
        linkedClient,
      ).flatMap(Workflow.renderAll(_, finished))
      val release = text(rendered, Workflow.releaseName)
      assertTrue(
        fault(rendered) == "",
        release.contains("  release-demo:"),
        release.contains("  release-other:"),
        !release.contains("\n  client:"),
        !release.contains("\n  install:"),
        !release.contains("actions: write"),
      )
    ,
    test("a native module named client with no main is not the install client"):
      val app      = demoApp
      val rendered = assemble(
        ToolRevision("21.1.8"),
        None,
        ships(Ship("demo").next(1, 2, 3)(app)),
        app,
        module("client", Native)(scala3).clang(ClangMajor(21)),
      ).flatMap(Workflow.renderAll(_, finished))
      val release = text(rendered, Workflow.releaseName)
      assertTrue(
        fault(rendered) == "",
        release.contains("  release-demo:"),
        release.contains("      contents: read"),
        !release.contains("\n  client:"),
        !release.contains("\n  install:"),
        !release.contains("actions: write"),
      )
    ,
    test("the install client is not a toolchain job, with or without this checkout's scm"):
      val app      = demoApp
      val rendered = assemble(
        ToolRevision("21.1.8"),
        None,
        ships(Ship("demo").next(1, 2, 3)(app)),
        app,
        linkedClient,
      ).flatMap(Workflow.renderAll(_, finished))
      val release = text(rendered, Workflow.releaseName)
      val ci      = text(rendered, Workflow.fileName)
      assertTrue(
        fault(rendered) == "",
        release.contains("\n  install:"),
        release.contains("""- run: "./histos client""""),
        !release.contains("histos-bootstrap"),
        !ci.contains("\n  toolchain:"),
      )
    ,
    test("a ship that keeps the default publishes snapshots without a client job"):
      val app      = demoApp
      val rendered = assemble(
        ToolRevision("21.1.8"),
        None,
        ships(Ship("demo").next(1, 2, 3)(app)),
        app,
        linkedClient,
      ).flatMap(Workflow.renderAll(_, finished))
      val snapshots = text(rendered, Workflow.snapshotsName)
      assertTrue(
        fault(rendered) == "",
        snapshots.contains("\n  snapshots:\n    runs-on: ubuntu-26.04\n"),
        snapshots.contains("""- run: "./histos publish snapshots""""),
        !snapshots.contains("\n  client:"),
        !snapshots.contains("needs:"),
        !snapshots.contains("llvm"),
        !snapshots.contains("download-artifact"),
        !snapshots.contains("actions: read"),
      )
    ,
    test("snapshot clients link on both runners, and the publish job downloads them after the jar is assembled"):
      val app   = demoApp
      val asked = ships(Ship("demo").next(1, 2, 3)(app).clients(ClientBuilds.ReleasesAndSnapshots))
      val opted = assemble(ToolRevision("21.1.8"), Some(own), asked, app, linkedClient)
        .flatMap(Workflow.renderAll(_, finished))
      val default =
        assemble(ToolRevision("21.1.8"), Some(own), ships(Ship("demo").next(1, 2, 3)(app)), app, linkedClient)
          .flatMap(Workflow.renderAll(_, finished))
      val snapshots         = text(opted, Workflow.snapshotsName)
      val (client, publish) = snapshots.split("\n  snapshots:\n", 2) match
        case Array(head, tail) => (head, tail)
        case _                 => ("", "")
      val missing = List(
        "          - runner: ubuntu-26.04",
        "            platform: linux-x86_64",
        "            archive: LLVM-21.1.8-Linux-X64.tar.xz",
        s"            sha256: $linux",
        "          - runner: macos-26",
        "            platform: macos-aarch64",
        "            archive: LLVM-21.1.8-macOS-ARM64.tar.xz",
        s"            sha256: $mac",
        """            [ "$got" = "$expected" ] || { echo "llvm checksum mismatch" >&2; exit 1; }""",
        """      - run: "./histos client"""",
        "          name: client-${{ matrix.platform }}",
        "          path: dist/histos-client",
      ).filterNot(client.contains)
      val steps = List(
        """      - run: "./histos assembly"""",
        "          pattern: client-*",
        "          path: dist/clients",
        """      - run: "./histos publish snapshots"""",
      ).map(publish.indexOf)
      assertTrue(
        fault(opted) == "",
        client.contains("\n  client:\n    runs-on: ${{ matrix.runner }}\n"),
        !client.contains("needs:"),
        !client.contains("histos-bootstrap"),
        missing == Nil,
        publish.startsWith("    needs: client\n"),
        publish.contains("      actions: read\n      contents: write\n"),
        steps.forall(_ >= 0),
        steps == steps.sorted,
        DistPlatform.values.toList.map(DistLayout.builtClient) == List(
          "dist/clients/client-linux-x86_64/histos-client",
          "dist/clients/client-macos-aarch64/histos-client",
        ),
        text(opted, Workflow.releaseName) == text(default, Workflow.releaseName),
      )
    ,
    test("a ship that asks for snapshot clients and is not the install ship fails the render"):
      val app      = demoApp
      val extra    = extraApp
      val twoAsset = assemble(
        ToolRevision("21.1.8"),
        None,
        ships(
          Ship("demo").next(1, 2, 3)(app).clients(ClientBuilds.ReleasesAndSnapshots),
          Ship("other").next(1, 2, 3)(extra),
        ),
        app,
        extra,
        linkedClient,
      ).flatMap(Workflow.renderAll(_, finished))
      val noClient = assemble(
        ToolRevision("21.1.8"),
        None,
        ships(Ship("demo").next(1, 2, 3)(app).clients(ClientBuilds.ReleasesAndSnapshots)),
        app,
      ).flatMap(Workflow.renderAll(_, finished))
      assertTrue(
        twoAsset == Left(CiError.SnapshotClients(ShipName("demo"))),
        noClient == Left(CiError.SnapshotClients(ShipName("demo"))),
      ),
  )

  private def demoApp: ProjectDef =
    module("app", Jvm)(scala3).assembly(MainClass("demo.Main")).asset(asset"dist/app.jar")

  private def extraApp: ProjectDef =
    module("extra", Jvm)(scala3).assembly(MainClass("demo.Extra")).asset(asset"dist/extra.jar")

  private def linkedClient: ProjectDef =
    module("client", Native)(scala3).clang(ClangMajor(21)).main(MainClass("histos.client.Main"))

  private def assemble(
      llvm: ToolRevision,
      scm: Option[Scm],
      catalog: CatalogSpec,
      modules: ProjectDef*
  ): Either[BuildError, ProjectGraph] =
    val tools     = RunnerTools(Some(NodeFloor(25)), Some(llvm), Some(ToolRevision("1.56.0")))
    val base      = Projects.organization(organization"example.com").runnerTools(tools)
    val organized = scm match
      case Some(found) => base.scm(found)
      case None        => base
    organized(catalog, modules*)

  private def text(rendered: Either[BuildError | CiError, List[RenderedWorkflow]], name: String): String =
    rendered.toOption.flatMap(_.find(_.name == name).map(_.body)).getOrElse("")

  private def fault(rendered: Either[BuildError | CiError, List[RenderedWorkflow]]): String =
    rendered match
      case Left(error) => error.toString
      case Right(_)    => ""
end WorkflowInstallLawSpec
