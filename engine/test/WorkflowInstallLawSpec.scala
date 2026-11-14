package histos

import histos.publish.{DistLayout, DistPlatform}
import histos.version.ShipName
import histos.workflow.*

import zio.test.*

/** The client matrix exists only for one ship that names `.toolchain` and pins LLVM. A native module named `client`
  * does not grow it. No graph grows a toolchain job from its scm.
  */
object WorkflowInstallLawSpec extends ZIOSpecDefault:
  private val scala3 = scala"3.10.0"

  given CiBranch      = CiBranch("main")
  given RenderingShip = RenderingLaw.ship

  private val finished = SonaProbe(ReleaseMode.Automatic)

  private val linux = "b3b7f2801d15d50736acea3c73982994d025b01c2f035b91ae3b49d1b575732b"

  private val mac = "b95bdd32a33a81ee4d40363aaeb26728a26783fcef26a4d80f65457433ea4669"

  private val own = Scm(
    url"https://github.com/early-effect/histos",
    connection"scm:git:git@github.com:early-effect/histos.git",
  )

  def spec = suite("workflow install laws")(
    test("one toolchain ship emits the install jobs and both digests"):
      val app      = demoApp
      val client   = linkedClient
      val rendered = assemble(
        ToolRevision("21.1.8"),
        Some(own),
        ships(Ship("demo").next(1, 2, 3)(app).toolchain(launcher = app, client = client)),
        app,
        client,
      ).flatMap(Workflow.renderAll(_, finished))
      val release = text(rendered, Workflow.releaseName)
      val ci      = text(rendered, Workflow.fileName)
      val missing = List(
        "  client:",
        "    runs-on: macos-26",
        "  install:",
        "    needs: client",
        "    needs: [client, release-demo]",
        """- run: "./histos publish local"""",
        "          name: client-macos-aarch64",
        "          path: dist/clients/client-macos-aarch64/histos-client",
        "          name: client-linux-x86_64",
        "          path: dist/clients/client-linux-x86_64/histos-client",
        "gh release upload dist",
        "\"shim-$shim_sha\"",
        "version=1.2.3",
        s"expected=\"$linux\"",
        s"expected=\"$mac\"",
        "LLVM-21.1.8-Linux-X64.tar.xz",
        "LLVM-21.1.8-macOS-ARM64.tar.xz",
        "llvm checksum mismatch",
        "LD_LIBRARY_PATH=$dir/lib",
        "clang-sdk",
        "llvmorg-21.1.8",
        "      actions: write",
        "          name: histos-jar",
        "          name: shim",
        """- run: "./histos assembly app"""",
        "path: dist/app.jar",
        "jar=dist-in/histos-jar/app.jar",
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
        !release.contains("./histos client"),
        !release.contains("matrix."),
        !release.contains("dist/histos.jar"),
      )
    ,
    test("the install matrix asks for the mac archive, and a pin that lacks it fails before any text"):
      val app      = demoApp
      val client   = linkedClient
      val rendered = assemble(
        ToolRevision("20.1.8"),
        None,
        ships(Ship("demo").next(1, 2, 3)(app).toolchain(launcher = app, client = client)),
        app,
        client,
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
      val client   = linkedClient
      val rendered = assemble(
        ToolRevision("21.1.8"),
        None,
        ships(Ship("demo").next(1, 2, 3)(app).toolchain(launcher = app, client = client)),
        app,
        client,
      ).flatMap(Workflow.renderAll(_, finished))
      val release = text(rendered, Workflow.releaseName)
      val ci      = text(rendered, Workflow.fileName)
      assertTrue(
        fault(rendered) == "",
        release.contains("\n  install:"),
        release.contains("""- run: "./histos publish local""""),
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
    test("a toolchain ship links clients on both runners for a snapshot and a release"):
      val app    = demoApp
      val client = linkedClient
      val asked  = ships(Ship("demo").next(1, 2, 3)(app).toolchain(launcher = app, client = client))
      val opted  = assemble(ToolRevision("21.1.8"), Some(own), asked, app, client)
        .flatMap(Workflow.renderAll(_, finished))
      val plain   = demoApp
      val idle    = linkedClient
      val default =
        assemble(ToolRevision("21.1.8"), Some(own), ships(Ship("demo").next(1, 2, 3)(plain)), plain, idle)
          .flatMap(Workflow.renderAll(_, finished))
      val snapshots         = text(opted, Workflow.snapshotsName)
      val (matrix, publish) = snapshots.split("\n  snapshots:\n", 2) match
        case Array(head, tail) => (head, tail)
        case _                 => ("", "")
      val missing = List(
        "    runs-on: macos-26",
        "LLVM-21.1.8-macOS-ARM64.tar.xz",
        s"expected=\"$mac\"",
        """            [ "$got" = "$expected" ] || { echo "llvm checksum mismatch" >&2; exit 1; }""",
        """      - run: "./histos publish local"""",
        "          name: client-macos-aarch64",
        "          path: dist/clients/client-macos-aarch64/histos-client",
      ).filterNot(matrix.contains)
      val linuxAt = publish.indexOf("LLVM-21.1.8-Linux-X64.tar.xz")
      val steps   = List(
        """      - run: "./histos assembly app"""",
        "          pattern: client-*",
        "          path: dist/clients",
        s"expected=\"$linux\"",
        """      - run: "./histos publish snapshots"""",
      ).map(publish.indexOf)
      assertTrue(
        fault(opted) == "",
        matrix.contains("\n  client:\n    runs-on: macos-26\n"),
        !matrix.contains("needs:"),
        !matrix.contains("matrix."),
        !matrix.contains("histos-bootstrap"),
        !matrix.contains("LLVM-21.1.8-Linux-X64"),
        missing == Nil,
        publish.startsWith("    needs: client\n"),
        publish.contains("      actions: read\n      contents: write\n"),
        linuxAt >= 0,
        steps.forall(_ >= 0),
        steps == steps.sorted,
        linuxAt > steps(2),
        linuxAt < steps(4),
        DistPlatform.values.toList.map(DistLayout.builtClient) == List(
          "dist/clients/client-linux-x86_64/histos-client",
          "dist/clients/client-macos-aarch64/histos-client",
        ),
        text(opted, Workflow.releaseName).contains("\n  client:"),
        text(opted, Workflow.releaseName).contains("""- run: "./histos assembly app""""),
        !text(default, Workflow.releaseName).contains("\n  client:"),
        !text(default, Workflow.snapshotsName).contains("\n  client:"),
      )
    ,
    test("two toolchain ships fail, and one toolchain ship with no LLVM pin fails"):
      val app    = demoApp
      val extra  = extraApp
      val client = linkedClient
      val two    = assemble(
        ToolRevision("21.1.8"),
        None,
        ships(
          Ship("demo").next(1, 2, 3)(app).toolchain(launcher = app, client = client),
          Ship("other").next(1, 2, 3)(extra).toolchain(launcher = extra, client = client),
        ),
        app,
        extra,
        client,
      ).flatMap(Workflow.renderAll(_, finished))
      val quiet = Projects
        .organization(organization"example.com")
        .runnerTools(RunnerTools(Some(NodeFloor(25)), None, None))(
          ships(Ship("demo").next(1, 2, 3)(app).toolchain(launcher = app, client = client)),
          app,
          client,
        )
        .flatMap(Workflow.renderAll(_, finished))
      assertTrue(
        two == Left(CiError.SnapshotClients(ShipName("demo"))),
        quiet == Left(CiError.SnapshotClients(ShipName("demo"))),
      )
    ,
    test("a plain ship beside a toolchain ship publishes with no client need"):
      val app      = demoApp
      val extra    = extraApp
      val client   = linkedClient
      val rendered = assemble(
        ToolRevision("21.1.8"),
        None,
        ships(
          Ship("demo").next(1, 2, 3)(app).toolchain(launcher = app, client = client),
          Ship("other").next(1, 2, 3)(extra),
        ),
        app,
        extra,
        client,
      ).flatMap(Workflow.renderAll(_, finished))
      val release = text(rendered, Workflow.releaseName)
      val other   = job(release, "release-other")
      val demo    = job(release, "release-demo")
      val install = job(release, "install")
      assertTrue(
        fault(rendered) == "",
        demo.contains("    needs: client"),
        !other.contains("needs:"),
        other.contains("""- run: "./histos publish release other""""),
        install.contains("    needs: [client, release-demo, release-other]"),
        job(release, "client").startsWith("  client:\n    runs-on: macos-26"),
      ),
  )

  /** The lines of one job, from its header through the line before the next job. */
  private def job(body: String, name: String): String =
    val lines = body.linesIterator.toList
    val start = lines.indexWhere(_ == s"  $name:")
    if start < 0 then ""
    else
      val end   = lines.indexWhere(line => line.startsWith("  ") && !line.startsWith("   "), start + 1)
      val slice = if end < 0 then lines.drop(start) else lines.slice(start, end)
      slice.mkString("\n")

  private def demoApp =
    module("app", Jvm)(scala3).assembly(MainClass("demo.Main")).asset(asset"dist/app.jar")

  private def extraApp =
    module("extra", Jvm)(scala3).assembly(MainClass("demo.Extra")).asset(asset"dist/extra.jar")

  private def linkedClient =
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
