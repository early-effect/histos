package histos.cli

import histos.scaffold.{Provision, ScaffoldKind}

import zio.*
import zio.test.*

/** `new` and `init` ask through `Console` only when standard input and output are both a terminal. */
object CliPromptLawSpec extends ZIOSpecDefault:
  private val person     = Stdio(Attached.Terminal, Attached.Terminal)
  private val unattended = List(
    Stdio(Attached.Terminal, Attached.Redirected),
    Stdio(Attached.Redirected, Attached.Terminal),
    Stdio(Attached.Redirected, Attached.Redirected),
  )
  private val kindPrompt   = "Write CI, a lock, .gitignore, and ./histos? [Y/n] "
  private val branchPrompt = s"CI branch [${Provision.DefaultBranch}]: "

  def spec = suite("cli prompt laws")(
    test("a person's n, no, or bare is a bare project, and any other answer or none is full"):
      val rows = List(
        List("n")    -> ScaffoldKind.Bare,
        List(" No ") -> ScaffoldKind.Bare,
        List("BARE") -> ScaffoldKind.Bare,
        List("")     -> ScaffoldKind.Full,
        List("y")    -> ScaffoldKind.Full,
        Nil          -> ScaffoldKind.Full,
      )
      ZIO
        .foreach(rows)((lines, want) => heard(lines)(Cli.kindOf(person, false, false)).map((lines, want, _)))
        .map: found =>
          val misses = found.collect {
            case (lines, want, (kind, shown, left)) if kind != want || shown != Vector(kindPrompt) || left.nonEmpty =>
              lines
          }
          assertTrue(misses == Nil)
    ,
    test("nobody is asked the kind: a flag or a redirected stream is its default, and the answer stays unread"):
      val rows =
        List((person, true, false) -> ScaffoldKind.Bare, (person, false, true) -> ScaffoldKind.Full) ++
          unattended.map(stdio => (stdio, false, false) -> ScaffoldKind.Full)
      ZIO
        .foreach(rows) { case (row @ (stdio, bare, yes), want) =>
          heard(List("n"))(Cli.kindOf(stdio, bare, yes)).map((row, want, _))
        }
        .map: found =>
          val misses = found.collect {
            case (row, want, (kind, shown, left)) if kind != want || shown.nonEmpty || !left.contains("n") => row
          }
          assertTrue(misses == Nil)
    ,
    test("a person names the CI branch, and a blank or missing answer is the default"):
      val rows = List(
        List("trunk")     -> "trunk",
        List(" release ") -> "release",
        List("  ")        -> Provision.DefaultBranch,
        Nil               -> Provision.DefaultBranch,
      )
      ZIO
        .foreach(rows)((lines, want) => heard(lines)(Cli.askBranch(person, false, None)).map((lines, want, _)))
        .map: found =>
          val misses = found.collect {
            case (lines, want, (branch, shown, left))
                if branch != want || shown != Vector(branchPrompt) || left.nonEmpty =>
              lines
          }
          assertTrue(misses == Nil)
    ,
    test("nobody is asked the CI branch: a flag or a redirected stream is its default, and the answer stays unread"):
      val rows =
        List((person, true, None) -> Provision.DefaultBranch, (person, false, Some("dev")) -> "dev") ++
          unattended.map(stdio => (stdio, false, None) -> Provision.DefaultBranch)
      ZIO
        .foreach(rows) { case (row @ (stdio, yes, branch), want) =>
          heard(List("trunk"))(Cli.askBranch(stdio, yes, branch)).map((row, want, _))
        }
        .map: found =>
          val misses = found.collect {
            case (row, want, (named, shown, left)) if named != want || shown.nonEmpty || !left.contains("trunk") => row
          }
          assertTrue(misses == Nil),
  ) @@ TestAspect.silent

  /** Feed `lines`, run `asked`, and keep its answer, what it printed, and the first line it left unread. */
  private def heard[A](lines: List[String])(asked: UIO[A]): UIO[(A, Vector[String], Option[String])] =
    for
      _      <- TestConsole.clearInput *> TestConsole.clearOutput
      _      <- TestConsole.feedLines(lines*)
      answer <- asked
      shown  <- TestConsole.output
      left   <- Console.readLine.option
    yield (answer, shown, left)
end CliPromptLawSpec
