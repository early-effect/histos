package histos.cli

import histos.workspace.{IoFailure, IoFault}

import java.nio.file.{Files, Path}

import zio.cli.{Args, Command as CliCommand}
import zio.{IO, ZIO}

/** The words tab can offer. The parse grammar stays separate: zio-cli reads an option only while it is the next token,
  * and Histos accepts a flag after a project.
  */
object CompletionSurface:
  enum Query:
    case Projects, Modules, Plugins

    def token: String = this match
      case Projects => "projects"
      case Modules  => "modules"
      case Plugins  => "plugins"
  end Query

  enum Slot:
    case Words(items: List[Word], query: Option[Query])
    case Paths
    case Free

  final case class Word(token: String, about: String)
  final case class Flag(
      token: String,
      about: String,
      value: Option[Slot] = None,
      many: Boolean = false,
      narrow: Option[Node] = None,
  )
  final case class Pos(slot: Slot, many: Boolean = false, required: Boolean = true, name: String = "value")
  final case class Node(
      token: String,
      about: String,
      flags: List[Flag] = Nil,
      args: List[Pos] = Nil,
      children: List[Node] = Nil,
  )

  def script(kind: ShellKind): String = kind match
    case ShellKind.Bash => bash
    case ShellKind.Zsh  => zsh

  def bash: String = render(bashHeader, bashHelpers)

  def zsh: String = render(zshHeader, zshHelpers)

  def instructions(kind: ShellKind): String = kind match
    case ShellKind.Bash =>
      "source ~/.histos/completions/histos.bash"
    case ShellKind.Zsh =>
      "fpath=(~/.histos/completions $fpath)\nautoload -Uz compinit && compinit"

  def file(home: Path, kind: ShellKind): Path =
    val dir = home.resolve(".histos").resolve("completions")
    kind match
      case ShellKind.Bash => dir.resolve("histos.bash")
      case ShellKind.Zsh  => dir.resolve("_histos")

  /** Write the script and return the lines a person adds to the shell rc. */
  def write(home: Path, kind: ShellKind): IO[IoFailure, String] =
    val path = file(home, kind)
    ZIO
      .attemptBlocking {
        Files.createDirectories(path.getParent)
        Files.writeString(path, script(kind))
      }
      .mapError(_ => IoFailure(path, IoFault.Write))
      .as(s"wrote $path\n${instructions(kind)}")

  private def sentence(key: String): String =
    Help.entries.find(_.name == key).map(_.about).getOrElse(key)

  private def fixed(tokens: String*): Slot =
    Slot.Words(tokens.toList.map(token => Word(token, token)), None)

  private def asked(query: Query, tokens: String*): Slot =
    Slot.Words(tokens.toList.map(token => Word(token, token)), Some(query))

  private val projects = Pos(asked(Query.Projects), many = true, name = "project")

  private val areas = Slot.Words(
    List(
      Word("toolchain", "JDK, Node, and clang"),
      Word("build", "The build definition"),
      Word("workflows", "Generated workflows"),
      Word("repo", "Repository settings"),
      Word("supply-chain", "Supply-chain files"),
      Word("site", "The published site"),
      Word("release", "The release"),
    ),
    None,
  )

  private val globals = List(
    Flag("--color", "Force color on"),
    Flag("--no-color", "Force color off"),
    Flag("--truecolor", "Use 24-bit color"),
    Flag("--no-truecolor", "Stay on the 256-color cube"),
    Flag("--verbose", "Print each compiler phase"),
    Flag("--explain", "Print one check", Some(Slot.Free)),
    Flag("--lock=offer", "Lock mode offer"),
    Flag("--lock=update", "Lock mode update"),
    Flag("--lock=local", "Lock mode local"),
    Flag("--lock=frozen", "Lock mode frozen"),
  )

  private val testFlags = List(
    Flag("--quick", "Skip a suite whose stamp still matches"),
    Flag("--only", "Run suites that match", Some(Slot.Free)),
    Flag("--affected", "Modules from histos affected", Some(Slot.Free)),
  )

  private val scaffold = List(
    Flag("--bare", "Write the build file and the sources only"),
    Flag("--yes", "Take the defaults"),
    Flag("--branch", "The CI branch", Some(Slot.Free)),
  )

  private val floors = List(
    "--min-stmt",
    "--min-branch",
    "--min-stmt-file",
    "--min-branch-file",
    "--min-stmt-package",
    "--min-branch-package",
    "--exclude-class",
    "--exclude-file",
  ).map(token => Flag(token, token, Some(Slot.Free)))

  private val updateFlags = List(
    Flag("--make-default", "Also write the default toolchain"),
    Flag("--stable", "Move to the newest stable"),
    Flag("--client", "Build the native client"),
    Flag("--no-client", "Skip the native client"),
    Flag("--freeze", "Pin the resolved version"),
    Flag("--follow", "Keep a moving channel"),
  )

  private val run = Node(
    "run",
    sentence("run"),
    args = List(
      Pos(asked(Query.Modules, "inspect", "versions", "doc"), many = false, name = "module"),
      Pos(fixed("jvm", "js", "native"), many = false, required = false, name = "platform"),
    ),
  )

  private val publishForms = List(
    Node("local", sentence("publish local"), args = List(Pos(Slot.Free, many = true, name = "token"))),
    Node("withdraw", sentence("publish withdraw")),
    Node(
      "snapshots",
      "Publish the snapshot coordinate at HEAD.",
      args = List(Pos(Slot.Free, many = true, name = "token")),
    ),
    Node(
      "release",
      sentence("publish release"),
      flags = List(Flag("--automatic", "Close the portal deployment")),
      args = List(Pos(Slot.Free, many = true, name = "token")),
    ),
    Node("assets", sentence("publish assets"), args = List(Pos(Slot.Free, many = true, name = "token"))),
    Node("promote", sentence("publish promote"), args = List(Pos(Slot.Free, name = "deployment"))),
    Node("drop", sentence("publish drop"), args = List(Pos(Slot.Free, name = "deployment"))),
  )

  private val commands = List(
    Node("version", sentence("version")),
    Node("help", sentence("help"), args = List(Pos(Slot.Free, many = true, name = "topic"))),
    Node("graph", sentence("graph")),
    Node("observe", sentence("observe")),
    Node("compile", sentence("compile"), args = List(projects)),
    run,
    Node(
      "clean",
      sentence("clean"),
      flags = List(Flag("--full", "Also forget this repo in the content store")),
      args = List(projects),
    ),
    Node("assembly", sentence("assembly"), args = List(Pos(asked(Query.Modules), many = true, name = "module"))),
    Node(
      "fmt",
      sentence("fmt"),
      flags = List(
        Flag("--check", "Write no source"),
        Flag("--file", "Format repository-relative files", Some(Slot.Paths), many = true),
      ),
      args = List(projects),
    ),
    Node("test", sentence("test"), flags = testFlags, args = List(projects)),
    Node("coverage", sentence("coverage"), flags = floors, args = List(projects)),
    Node("affected", sentence("affected"), flags = List(Flag("--base", "The base commit", Some(Slot.Free)))),
    Node("new", sentence("new"), flags = scaffold, args = List(Pos(Slot.Free, name = "name"))),
    Node(
      "init",
      sentence("init"),
      flags = scaffold :+ Flag("--from", "Draft from sbt or zipx", Some(fixed("sbt", "zipx"))),
    ),
    Node("publish", sentence("publish"), children = publishForms),
    Node(
      "bump",
      sentence("bump"),
      flags = List(
        Flag("--major", "Move the major"),
        Flag("--minor", "Move the minor"),
        Flag("--patch", "Move the patch"),
      ),
      args = List(Pos(Slot.Free, many = true, name = "ship")),
    ),
    Node("portal", sentence("portal")),
    Node(
      "resolve",
      sentence("resolve"),
      flags = List(
        Flag("--update", "Solve and write the lock"),
        Flag("--check", "Fail when a fresh solve differs"),
        Flag("--refresh", "Revalidate retained snapshot locators"),
      ),
      args = List(projects),
    ),
    Node(
      "deps",
      sentence("deps"),
      children = List(
        Node("check", "Check dependencies"),
        Node(
          "update",
          "Update dependencies",
          flags = List(Flag("--yes", "Apply the update"), Flag("--security", "Security updates")),
        ),
        Node("audit", "Audit dependencies", flags = List(Flag("--report", "Write the report"))),
        Node(
          "pr",
          "Open a dependency pull request",
          flags = List(Flag("--security", "Security updates"), Flag("--open", "Open the pull request")),
        ),
        Node("alert", "Report a dependency alert"),
        Node("snapshot", "Snapshot dependencies", flags = List(Flag("--submit", "Submit the snapshot"))),
      ),
    ),
    Node(
      "workflow",
      sentence("workflow"),
      flags = List(
        Flag("--check", "Fail when generated files differ"),
        Flag("--update", "Write the generated files"),
      ),
    ),
    Node(
      "doctor",
      sentence("doctor"),
      flags = List(
        Flag("--area", "Which checks run", Some(areas)),
        Flag("--json", "Print JSON"),
        Flag("--preflight", "One job's preflight", Some(Slot.Free)),
        Flag("--verbose", "List every check"),
        Flag("--fix", "Apply the repairs"),
        Flag("--yes", "Skip the prompt"),
      ),
      args = List(projects),
    ),
    Node(
      "setup",
      sentence("setup"),
      children = List(
        Node(
          "supply-chain",
          sentence("setup"),
          flags = List(
            Flag("--from", "Import from zipx", Some(fixed("zipx"))),
            Flag("--fix", "Write the files"),
            Flag("--yes", "Skip the prompt"),
          ),
        )
      ),
    ),
    Node(
      "cache",
      sentence("cache"),
      children = List(
        Node("path", "Print the cache path"),
        Node("list", "List cache entries"),
        Node("clean", "Remove old cache entries", flags = List(Flag("--dry-run", "Name entries and keep them"))),
        Node(
          "promote",
          "Copy a namespace",
          flags = List(
            Flag("--from", "Source namespace", Some(fixed("main"))),
            Flag("--to", "Destination namespace", Some(fixed("main"))),
          ),
        ),
        Node("forget", "Drop a namespace or a module", args = List(Pos(Slot.Free, name = "target"))),
        Node("verify", "Check stored hits", args = List(Pos(asked(Query.Modules), many = true, name = "module"))),
      ),
    ),
    Node(
      "toolchain",
      sentence("toolchain"),
      children = List(
        Node(
          "install",
          sentence("toolchain install"),
          flags = List(Flag("--client", "Build the native client"), Flag("--no-client", "Skip the native client")),
          args = List(Pos(Slot.Free, name = "version")),
        ),
        Node("list", sentence("toolchain list")),
        Node("client", "Build the native client for this jar"),
      ),
    ),
    Node("update", sentence("update"), flags = updateFlags),
    Node("self", sentence("self"), children = List(Node("update", sentence("self"), flags = updateFlags))),
    Node("bsp", sentence("bsp")),
    Node("mcp", sentence("mcp")),
    Node("server", "Keep a warm process in the foreground"),
    Node("reload", sentence("reload")),
    Node("reboot", sentence("reboot")),
    Node("stop", sentence("stop")),
    Node("cancel", sentence("cancel"), args = List(Pos(Slot.Free, name = "id"))),
    Node(
      "preview",
      sentence("preview"),
      children = List(
        Node("compile", "Show the compile plan", args = List(projects)),
        Node("test", "Show the test plan", flags = testFlags, args = List(projects)),
        Node(
          "publish",
          "Show the publish plan",
          children = List(
            Node("local", "Show the local publish plan", args = List(Pos(Slot.Free, many = true, name = "token"))),
            Node(
              "snapshots",
              "Show the snapshot publish plan",
              args = List(Pos(Slot.Free, many = true, name = "token")),
            ),
          ),
        ),
        Node(
          "clean",
          "Show the clean plan",
          flags = List(Flag("--full", "Also forget this repo in the content store")),
          args = List(projects),
        ),
        Node(
          "run",
          "Show the run plan",
          args = List(
            Pos(asked(Query.Modules, "inspect", "versions", "doc"), name = "module"),
            Pos(fixed("jvm", "js", "native"), required = false, name = "platform"),
          ),
        ),
      ),
    ),
  )

  private val completion = Node(
    "completion",
    sentence("completion"),
    children = List(
      Node(
        "install",
        "Write the script and print the rc lines",
        children = List(Node("zsh", "Write the zsh script"), Node("bash", "Write the bash script")),
      ),
      Node("zsh", "Print the zsh script"),
      Node("bash", "Print the bash script"),
    ),
  )

  private val tilde = Node(
    "~",
    sentence("~"),
    flags = List(
      Flag("--restart", "Restart a program", narrow = Some(Node("watched", "Restart a program", children = List(run))))
    ),
    children = commands,
  )

  private val root = Node("histos", "Histos", flags = globals, children = commands :+ tilde :+ completion)

  val command: CliCommand[Unit] = commandOf(root)

  /** Flags are sibling commands. zio-cli's completion derivative desugars optional options into a permutation. */
  private def commandOf(node: Node): CliCommand[Unit] =
    val self  = leaf(node)
    val kids  = node.children.map(commandOf) ++ node.flags.map(flagCommand)
    val built =
      if kids.isEmpty then self
      else self.subcommands(kids.reduceLeft(_ | _)).as(())
    built.withHelp(node.about)

  private def leaf(node: Node): CliCommand[Unit] =
    if node.args.isEmpty then CliCommand(node.token)
    else CliCommand(node.token, arguments(node.args)).as(())

  private def flagCommand(flag: Flag): CliCommand[Unit] =
    val args = flag.value match
      case Some(slot) => List(Pos(slot, many = flag.many))
      case None       => Nil
    val children = flag.narrow.fold(List.empty[Node])(_.children)
    commandOf(Node(flag.token, flag.about, args = args, children = children))

  private def arguments(slots: List[Pos]): Args[Unit] =
    slots match
      case Nil          => Args.Empty
      case head :: tail =>
        tail.foldLeft(argument(head)): (acc, pos) =>
          (acc ++ argument(pos)).map(_ => ())

  private def argument(pos: Pos): Args[Unit] =
    pos.slot match
      case Slot.Words(items, None) if items.nonEmpty =>
        val pairs      = items.map(word => word.token -> word.token)
        val enumerated = Args.enumeration(pos.name)(pairs*)
        if pos.many then enumerated.*.map(_ => ())
        else if pos.required then enumerated.map(_ => ())
        else enumerated.atMost(1).map(_ => ())
      case _ =>
        val text = Args.text(pos.name)
        if pos.many then text.*.map(_ => ())
        else if pos.required then text.map(_ => ())
        else text.atMost(1).map(_ => ())

  private final case class Ref(token: String, id: String, about: String)

  /** `jump` is the step entered after this flag. `--restart` lands on a node whose only child is `run`. */
  private final case class Armed(
      token: String,
      about: String,
      value: Option[Slot],
      many: Boolean,
      jump: Option[String],
  )

  private final case class Step(
      id: String,
      root: Boolean,
      flags: List[Armed],
      children: List[Ref],
      args: List[Pos],
  )

  private def walk(node: Node, n: Int, top: Boolean): (List[Step], String, Int) =
    val id                          = s"n$n"
    val (childSteps, childRefs, n1) =
      node.children.foldLeft((List.empty[Step], List.empty[Ref], n + 1)):
        case ((steps, refs, k), child) =>
          val (more, cid, k2) = walk(child, k, top = false)
          (steps ++ more, refs :+ Ref(child.token, cid, child.about), k2)
    val (armed, narrowSteps, n2) =
      node.flags.foldLeft((List.empty[Armed], List.empty[Step], n1)):
        case ((flags, steps, k), flag) =>
          flag.narrow match
            case None         => (flags :+ Armed(flag.token, flag.about, flag.value, flag.many, None), steps, k)
            case Some(target) =>
              val (more, cid, k2) = walk(target, k, top = false)
              (flags :+ Armed(flag.token, flag.about, flag.value, flag.many, Some(cid)), steps ++ more, k2)
    val step = Step(id, top, armed, childRefs, node.args)
    (step :: childSteps ++ narrowSteps, id, n2)

  private def render(header: String, helpers: String): String =
    val (steps, _, _) = walk(root, 0, top = true)
    header + helpers + steps.map(function).mkString("\n")

  private def function(step: Step): String =
    val offers = step.args.indices.map(pos => offer(step, pos)).mkString("\n")
    val past   = offerBody(step, step.args.length)
    val arms   = (armsOf(step) ++ globalArms(step) ++ childArms(step) :+ fallback(step)).mkString("\n")
    s"""_${step.id}() {
       |  local i=$$1
       |  local pos=$${2:-0}
       |  local cur listed word extra
       |  if [ "$$i" -ge "$$COMP_CWORD" ]; then
       |    cur=$${COMP_WORDS[$$COMP_CWORD]}
       |    case $$pos in
       |$offers
       |      *)
       |$past
       |        ;;
       |    esac
       |    return
       |  fi
       |  word=$${COMP_WORDS[$$i]}
       |  case $$word in
       |$arms
       |  esac
       |}
       |""".stripMargin

  private def offer(step: Step, pos: Int): String =
    s"""      $pos)
       |${offerBody(step, pos)}
       |        ;;""".stripMargin

  private def offerBody(step: Step, pos: Int): String =
    val listed  = quote(lines(wordsAt(step, pos)))
    val kind    = queryAt(step, pos)
    val plugins = if step.root && pos == 0 then "prefix" else ""
    val fetch   = fetched(kind)
    val handed  = if kind.isEmpty then "\"\"" else "\"$$extra\""
    s"""        listed=$listed
       |        $fetch
       |        _histos_offer "$$cur" "$$listed" $handed "$plugins"""".stripMargin

  /** A static kind is written out, so the script contains `complete projects` and `complete modules`. */
  private def fetched(kind: String): String =
    if kind.isEmpty then "extra="
    else s"""extra=$$("$$_histos_bin" complete $kind 2>/dev/null || true)"""

  private def wordsAt(step: Step, pos: Int): List[Word] =
    val own    = step.flags.map(flag => Word(flag.token, flag.about))
    val common = if step.root then Nil else globals.map(flag => Word(flag.token, flag.about))
    val kids   =
      if pos == 0 then step.children.map(ref => Word(ref.token, ref.about)) else Nil
    val slotted = step.args.lift(pos) match
      case Some(Pos(Slot.Words(items, _), _, _, _)) => items
      case _                                        => Nil
    dedupe(kids ++ own ++ common ++ slotted)

  private def queryAt(step: Step, pos: Int): String =
    step.args.lift(pos) match
      case Some(Pos(Slot.Words(_, Some(kind)), _, _, _)) => kind.token
      case _                                             => ""

  private def dedupe(words: List[Word]): List[Word] =
    words.foldLeft(List.empty[Word]): (acc, word) =>
      if acc.exists(_.token == word.token) then acc else acc :+ word

  private def armsOf(step: Step): List[String] =
    step.flags.map(flag => arm(step.id, flag))

  private def globalArms(step: Step): List[String] =
    if step.root then Nil
    else
      globals
        .filterNot(flag => step.flags.exists(_.token == flag.token))
        .map(flag => arm(step.id, Armed(flag.token, flag.about, flag.value, flag.many, None)))

  private def arm(here: String, flag: Armed): String =
    val pattern = quote(flag.token)
    flag.jump match
      case Some(target) =>
        s"""    $pattern)
           |      _$target $$((i + 1)) 0
           |      ;;""".stripMargin
      case None =>
        flag.value match
          case Some(Slot.Paths) =>
            val next =
              if flag.many then s"_histos_paths $$((i + 1))"
              else s"_$here $$((i + 2)) \"$$pos\""
            s"""    $pattern)
               |      $next
               |      ;;""".stripMargin
          case Some(slot) =>
            val listed = quote(lines(slotWords(slot)))
            val kind   = slot match
              case Slot.Words(_, Some(query)) => query.token
              case _                          => ""
            val handed = if kind.isEmpty then "\"\"" else "\"$$extra\""
            s"""    $pattern)
               |      if [ $$((i + 1)) -ge "$$COMP_CWORD" ]; then
               |        cur=$${COMP_WORDS[$$COMP_CWORD]}
               |        listed=$listed
               |        ${fetched(kind)}
               |        _histos_offer "$$cur" "$$listed" $handed ""
               |      else
               |        _$here $$((i + 2)) "$$pos"
               |      fi
               |      ;;""".stripMargin
          case None =>
            s"""    $pattern)
               |      _$here $$((i + 1)) "$$pos"
               |      ;;""".stripMargin

  private def slotWords(slot: Slot): List[Word] = slot match
    case Slot.Words(items, _) => items
    case Slot.Paths           => Nil
    case Slot.Free            => Nil

  private def childArms(step: Step): List[String] =
    step.children.map: ref =>
      val pattern = quote(ref.token)
      s"""    $pattern)
         |      _${ref.id} $$((i + 1)) 0
         |      ;;""".stripMargin

  private def fallback(step: Step): String =
    val plugin =
      if step.root then """    *)
          |      _histos_plugin "$word" $((i + 1))
          |      ;;""".stripMargin
      else
        val next = step.args.zipWithIndex.map { (pos, index) =>
          val dest = if pos.many then index else index + 1
          s"""      $index)
             |        _histos_here $$((i + 1)) $dest
             |        ;;""".stripMargin
        }
        s"""    *)
           |      case $$pos in
           |${next.mkString("\n")}
           |        *)
           |          _histos_here $$((i + 1)) "$$pos"
           |          ;;
           |      esac
           |      ;;""".stripMargin
    plugin.replace("_histos_here", s"_${step.id}")

  private def lines(words: List[Word]): String =
    words.map(word => s"${word.token}:${desc(word.about)}").mkString("\n")

  private def desc(about: String): String =
    about.replace(':', ',').replace('\n', ' ').replace('\'', ' ')

  private def quote(text: String): String =
    "'" + text.replace("'", "'\\''") + "'"

  private val bashHeader: String =
    """_histos() {
      |  _histos_bin=${COMP_WORDS[0]}
      |  _n0 1 0
      |}
      |complete -F _histos histos
      |complete -F _histos ./histos
      |""".stripMargin

  private val zshHeader: String =
    """#compdef histos ./histos
      |_histos() {
      |  _histos_bin=${words[1]}
      |  typeset -a COMP_WORDS
      |  COMP_WORDS=("${words[@]}")
      |  COMP_CWORD=$CURRENT
      |  _n0 2 0
      |}
      |""".stripMargin

  private val bashHelpers: String =
    """_histos_offer() {
      |  local cur=$1
      |  local listed=$2
      |  local extra=$3
      |  local plugins=$4
      |  local words
      |  words=$(printf '%s\n' "$listed" | awk -F: '{print $1}')
      |  if [ "$plugins" = prefix ] && [ -n "$cur" ]; then
      |    extra="$extra $("$_histos_bin" complete plugins 2>/dev/null || true)"
      |  fi
      |  COMPREPLY=( $(compgen -W "$words $extra" -- "$cur") )
      |}
      |_histos_paths() {
      |  local i=$1
      |  if [ "$i" -ge "$COMP_CWORD" ]; then
      |    COMPREPLY=( $(compgen -f -- "${COMP_WORDS[$COMP_CWORD]}") )
      |    return
      |  fi
      |  _histos_paths $((i + 1))
      |}
      |_histos_plugin() {
      |  local name=$1
      |  local i=$2
      |  local extra
      |  if [ "$i" -ge "$COMP_CWORD" ]; then
      |    extra=$("$_histos_bin" complete flags "$name" 2>/dev/null || true)
      |    _histos_offer "${COMP_WORDS[$COMP_CWORD]}" "" "$extra" ""
      |    return
      |  fi
      |  _histos_plugin "$name" $((i + 1))
      |}
      |""".stripMargin

  private val zshHelpers: String =
    """_histos_offer() {
      |  local cur=$1
      |  local listed=$2
      |  local extra=$3
      |  local plugins=$4
      |  local -a specs found
      |  local line token
      |  specs=()
      |  for line in ${(f)listed}; do
      |    [ -n "$line" ] && specs+=("$line")
      |  done
      |  for token in ${(f)extra}; do
      |    [ -n "$token" ] && specs+=("${token}:${token}")
      |  done
      |  if [ "$plugins" = prefix ] && [ -n "$cur" ]; then
      |    found=(${(f)"$($_histos_bin complete plugins 2>/dev/null)"})
      |    for token in $found; do
      |      specs+=("${token}:${token}")
      |    done
      |  fi
      |  _describe 'histos' specs
      |}
      |_histos_paths() {
      |  local i=$1
      |  if [ "$i" -ge "$COMP_CWORD" ]; then
      |    _files
      |    return
      |  fi
      |  _histos_paths $((i + 1))
      |}
      |_histos_plugin() {
      |  local name=$1
      |  local i=$2
      |  local extra
      |  if [ "$i" -ge "$COMP_CWORD" ]; then
      |    extra=$("$_histos_bin" complete flags "$name" 2>/dev/null || true)
      |    _histos_offer "${COMP_WORDS[$COMP_CWORD]}" "" "$extra" ""
      |    return
      |  fi
      |  _histos_plugin "$name" $((i + 1))
      |}
      |""".stripMargin

end CompletionSurface
