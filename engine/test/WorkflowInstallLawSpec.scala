package histos

import histos.workflow.*

import zio.test.*

/** The release install file exists only for one non-snapshot ship that owns an asset, a native module named `client`
  * with a main, and an LLVM pin that has both install archives. The toolchain job exists only when the graph's scm is
  * this checkout.
  */
object WorkflowInstallLawSpec extends ZIOSpecDefault:
  private val scala3 = scala"3.10.0"

  given CiBranch = CiBranch("main")

  private val finished = Sona.described(ReleaseMode.Automatic)

  private val linux = "b3b7f2801d15d50736acea3c73982994d025b01c2f035b91ae3b49d1b575732b"

  private val mac = "b95bdd32a33a81ee4d40363aaeb26728a26783fcef26a4d80f65457433ea4669"

  private val own = Scm(
    url"https://github.com/early-effect/histos",
    connection"scm:git:git@github.com:early-effect/histos.git",
  )

  def spec = suite("workflow install laws")(
    test("one release asset and a native client emit the install jobs, both digests, and the toolchain"):
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
        "          name: histos-bootstrap",
        "- runner: ubuntu-26.04",
        "- runner: macos-26",
      ).filterNot(release.contains)
      val ciMissing = List(
        "  toolchain:",
        "built=false",
        "built=true",
        """node-version: "25"""",
        "playwright@1.56.0",
        "install firefox",
        "needs.toolchain.outputs.built == 'true'",
      ).filterNot(ci.contains)
      assertTrue(
        fault(rendered) == "",
        missing == Nil,
        ciMissing == Nil,
        !release.contains("needs.toolchain.outputs.built"),
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
    test("the toolchain job follows this checkout's scm, and the client job does not"):
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
