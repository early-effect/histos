package histos.doctor

import histos.RunnerTools

/** The `.runnerTools(RunnerTools(...))` call a build adds. */
object RunnerCall:
  def show(tools: RunnerTools): String =
    val node       = render(tools.node)(floor => s"NodeFloor(${floor.value})")
    val llvm       = render(tools.llvm)(revision => quoted("ToolRevision", revision.value))
    val playwright = render(tools.playwright)(revision => quoted("ToolRevision", revision.value))
    val args       = tools.nodePath match
      case None       => s"$node, $llvm, $playwright"
      case Some(path) =>
        s"$node, $llvm, $playwright, Some(${quoted("NodePath", path.value)})"
    s".runnerTools(RunnerTools($args))"

  private def quoted(name: String, raw: String): String =
    s"$name(\"${escape(raw)}\")"

  private def render[A](value: Option[A])(show: A => String): String =
    value match
      case Some(found) => s"Some(${show(found)})"
      case None        => "None"

  private def escape(raw: String): String =
    raw.replace("\\", "\\\\").replace("\"", "\\\"")
end RunnerCall

/** Long form for `histos --explain <check-id>`. The same text is [[ToolchainDiagnostic.explain]]. */
object ToolchainExplain:
  val jdk: String =
    """Histos runs on JDK 21 or newer. The home is the absolute path in .histos/toolchain.local when that file exists. Otherwise it is java.home, the JVM that launched this process. Histos does not fetch a JDK and does not fall through from a toolchain.local that is too old.
      |
      |Install a JDK 21 (or newer). Write its home, one absolute path, to .histos/toolchain.local. Remove that file to use java.home.""".stripMargin

  val nodeFloor: String =
    """A module that launches on Node needs Node 25 or newer. Scala.js Wasm does not run on the Node 22 that many PATH entries still install. A JVM module and a Native module do not launch Node. An aggregate is its modules, and those children decide.
      |
      |histos doctor with no project launches nothing, so this check does not probe, even when the graph has a Js module. histos doctor <project> asks about that project. A Js module fails when Node is missing or older than 25, and the finding names every Js module in the launch set. histos toolchain still probes whenever the graph has a Js module.
      |
      |The binary is chosen in this order. HISTOS_NODE is an absolute path to the node binary and wins over everything else. Then runnerTools.nodePath. Then the Node home written in .histos/node.local. Then the first node on PATH. A named binary that is missing or older than 25 fails. Histos does not fetch Node and does not try the next source.
      |
      |Install Node 25 from https://nodejs.org/. Then either export HISTOS_NODE to that absolute node binary, or write the Node home, one absolute path, to .histos/node.local.
      |
      |A generated test job installs Node only when runnerTools names a NodeFloor. See runner-tools.cover.""".stripMargin

  val nodeShadowed: String =
    """PATH is searched from front to back. When this run launches a Js module from PATH, a node older than 25 earlier in PATH hides a newer node later in PATH. The launch uses the first one, so the newer binary is never tried. The finding names every Js module in that launch set.
      |
      |histos doctor with no project launches nothing, so this check does not walk PATH. A JVM module and a Native module do not launch Node. HISTOS_NODE, runnerTools.nodePath, and .histos/node.local win over PATH. This check does not walk PATH when one of those names the binary, including an unreadable .histos/node.local, because that file does not fall through to PATH.
      |
      |export HISTOS_NODE to the absolute path of the newer node binary. HISTOS_NODE wins over PATH, .histos/node.local, and runnerTools.nodePath. A warning does not change the exit.""".stripMargin

  val clang: String =
    """A Native module is linked with the clang and clang++ pair on PATH. The major must be the ClangMajor that module names. clang and clang++ must report the same version and sit side by side. Histos does not fetch clang.
      |
      |histos doctor with no project links nothing, so this check does not probe, even when the graph has a Native module. histos doctor <project> asks about that project. A JVM module and a Js module do not link Native. An aggregate is its modules, and those children decide. A launch set with no Native module reports `no Native module`. A miss names every Native module in the launch set. histos toolchain still probes whenever the graph has a Native module.
      |
      |Install that major and put its bin directory on PATH ahead of any older clang.""".stripMargin

  val cover: String =
    """A generated test job installs Node only when runnerTools.node is set and the job builds Js. It installs LLVM only when runnerTools.llvm is set and the job builds Native. ToolNeed drops the install step when the field is empty, so the job fails later with no node or no clang and no line that says what to add.
      |
      |Add the missing field on the build. The call is .runnerTools(RunnerTools(node, llvm, playwright)). node is Some(NodeFloor(25)) when any module is Js. llvm is Some(ToolRevision("<ClangMajor>.0.0")) when any module is Native. The revision's major is the modules' ClangMajor. The finding prints the call with the values filled in.""".stripMargin

  val agree: String =
    """NodeFloor is the major the test job installs. Js graphs need 25. A lower floor installs a Node that histos then refuses.
      |
      |The LLVM revision's major is the clang the job puts on PATH. It must equal the ClangMajor every Native module names. A 20.x revision does not satisfy ClangMajor(21).
      |
      |Raise the floor to NodeFloor(25), or change the ToolRevision so its major matches. The finding prints the .runnerTools(RunnerTools(...)) call with the raised value. Histos does not open a pull request to bump it.""".stripMargin

  def text(raw: String): Option[String] =
    catalog.get(raw)

  def known: String =
    catalog.keys.mkString(", ")

  private val catalog: Map[String, String] = Map(
    "jdk.source"         -> jdk,
    "node.floor"         -> nodeFloor,
    "node.shadowed"      -> nodeShadowed,
    "clang.source"       -> clang,
    "runner-tools.cover" -> cover,
    "runner-tools.agree" -> agree,
  )
end ToolchainExplain

/** Toolchain and runnerTools findings. The first fix is the one to apply. */
enum ToolchainDiagnostic(
    val check: CheckId,
    val level: Level,
    val title: String,
    val summary: String,
    val explain: String,
    val fixes: ::[Fix],
) extends Diagnostic:
  case Jdk(detail: String)
      extends ToolchainDiagnostic(
        CheckId.trusted("jdk.source"),
        Level.Error,
        "the JDK is not usable",
        detail,
        ToolchainExplain.jdk,
        fixes(
          Fix.Steps(
            lines(
              "Install a JDK 21 or newer.",
              "Write its home, one absolute path, to .histos/toolchain.local.",
              "Or remove .histos/toolchain.local and run on a JDK 21 java.home.",
            )
          )
        ),
      )

  case NodeFloor(detail: String)
      extends ToolchainDiagnostic(
        CheckId.trusted("node.floor"),
        Level.Error,
        "Node is older than 25",
        detail,
        ToolchainExplain.nodeFloor,
        fixes(
          Fix.Steps(
            lines(
              "Install Node 25 from https://nodejs.org/.",
              "export HISTOS_NODE=/absolute/path/to/node",
              "Or write that Node home to .histos/node.local.",
            )
          )
        ),
      )

  case NodeShadowed(line: String, detail: String)
      extends ToolchainDiagnostic(
        CheckId.trusted("node.shadowed"),
        Level.Warning,
        "an older node hides a newer one on PATH",
        detail,
        ToolchainExplain.nodeShadowed,
        fixes(Fix.Command(ShellLine.trusted(line))),
      )

  case Clang(detail: String)
      extends ToolchainDiagnostic(
        CheckId.trusted("clang.source"),
        Level.Error,
        "clang is not usable",
        detail,
        ToolchainExplain.clang,
        fixes(
          Fix.Steps(
            lines(
              "Install the ClangMajor the Native modules name.",
              "Put that clang and clang++ on PATH, side by side.",
            )
          )
        ),
      )

  case Cover(detail: String, shape: String)
      extends ToolchainDiagnostic(
        CheckId.trusted("runner-tools.cover"),
        Level.Error,
        "runnerTools does not cover the modules",
        detail,
        ToolchainExplain.cover,
        fixes(Fix.Steps(lines(shape, "Put that call on the build, next to the modules."))),
      )

  case Agree(detail: String, shape: String)
      extends ToolchainDiagnostic(
        CheckId.trusted("runner-tools.agree"),
        Level.Error,
        "runnerTools does not match the modules",
        detail,
        ToolchainExplain.agree,
        fixes(Fix.Steps(lines(shape, "Replace the build's .runnerTools(...) with that call."))),
      )
end ToolchainDiagnostic
