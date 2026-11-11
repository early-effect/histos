package histos.cli

import histos.cache.ContentStore
import histos.jdk.{Machine, Os}
import histos.load.{LoadError, Loader}
import histos.native.NativeOrigin
import histos.resolve.Resolve
import histos.toolchain.Architecture
import histos.LawDir
import slayer.{Stubbed, any, stub, stubbed}

import java.nio.file.{Files, Path}

import zio.*
import zio.test.*

object ExecuteLawSpec extends ZIOSpecDefault:
  private val apiJar     = Path.of("api.jar")
  private val homeText   = "no user home, so the api jar cannot be cached"
  private val homeFix    = s"$homeText\nfix  histos help"
  private val pinText    = "histos.version=9.9.9\n"
  private val helpHeader =
    "histos help\nhistos help <command>\n\ninfo\n  histos version\n"
  private val helpOut =
    val text = Help.grouped
    if text.startsWith(helpHeader) then s"$text\n" else helpHeader
  private val jdkExplain =
    "Histos runs on JDK 21 or newer. The home is the absolute path in " +
      ".histos/toolchain.local when that file exists. Otherwise it is java.home, " +
      "the JVM that launched this process. Histos does not fetch a JDK and does not " +
      "fall through from a toolchain.local that is too old.\n\n" +
      "Install a JDK 21 (or newer). Write its home, one absolute path, to " +
      ".histos/toolchain.local. Remove that file to use java.home."
  private val previewRoots =
    List(
      ".histos/classes",
      ".histos/analysis",
      ".histos/test-classes",
      ".histos/test-analysis",
      ".histos/test-work",
      ".histos/test-quick",
      ".histos/tests",
      ".histos/jars",
      ".histos/link",
      ".histos/run",
      ".histos/provider",
      ".histos/provider-src",
    ).mkString("\n")

  private final case class Law(args: List[String], exit: CliExit, out: String, err: String, playback: Playback)

  def spec = suite("execute laws")(
    test("a parse failure names the command and the fix"):
      batch("parse", parseLaws)
    ,
    test("help prints one command and explain prints the check"):
      open("help"): repo =>
        for
          printed <- run(repo, helpLaws)
          named   <- Cli.execute(List("--explain", "nope"), repo, apiJar)
        yield printed && unknownCheck(named)
    ,
    test("version and the launcher print the pin or its absence"):
      pins
    ,
    test("reload and reboot with no server say no histos server is running"):
      batch(
        "absent",
        List(
          Law(List("reload"), CliExit.LoadFailed, "", "no histos server is running", Playback.Held),
          Law(List("reboot"), CliExit.LoadFailed, "", "no histos server is running", Playback.Held),
        ),
      )
    ,
    test("cancel names the run that is not live"):
      batch(
        "cancel",
        List(
          Law(
            List("cancel", "run-1"),
            CliExit.TaskFailed,
            "",
            "no run 'run-1'\nNo live run is named run-1.\nfix  histos help cancel",
            Playback.Heard,
          )
        ),
      )
    ,
    test("portal and promote refuse missing sonatype credentials"):
      batch("portal", credentialLaws)
    ,
    test("a command that runs here reports the load fix as heard"):
      batch("heard", heardLaws)
    ,
    test("a command that holds the result reports the load fix"):
      batch("held", heldLaws)
    ,
    test("affected, coverage, bsp, and an unknown root keep the loader sentence"):
      batch("rawload", rawLoadLaws)
    ,
    test("doctor and deps keep the loader sentence and fail the task"):
      batch("rawtask", rawTaskLaws)
    ,
    test("cache, clean, preview, setup, and observe print their own sentences"):
      files
    ,
    test("bare new and init print the directory and histos test"):
      scaffold,
  ).provideSomeLayerShared[TestEnvironment & Scope](LawDir.layer)

  private def pins: ZIO[Path & TestEnvironment & Scope, Nothing, TestResult] =
    open("pins") { repo =>
      val absent  = repo.resolve("build.properties")
      val missing =
        Law(
          List("version"),
          CliExit.ResolveFailed,
          "",
          s"launcher pin is absent: $absent\nfix  histos help",
          Playback.Held,
        )
      val shown   = "histos 9.9.9\nserver 9.9.9\nno local assembly"
      val release =
        "launcher --update follows snapshots, and 9.9.9 is a release pin. Run 'histos update'."
      for
        _       <- failLoad
        before  <- Cli.execute(missing.args, repo, apiJar)
        _       <- ZIO.attemptBlocking(Files.writeString(absent, pinText)).orDie
        version <- Cli.execute(List("version"), repo, apiJar)
        latest  <- Cli.execute(List("launcher", "--latest"), repo, apiJar)
        update  <- Cli.execute(List("launcher", "--update"), repo, apiJar)
        latestLaw = Law(List("launcher", "--latest"), CliExit.Success, "histos.version=9.9.9", "", Playback.Held)
        updateLaw = Law(List("launcher", "--update"), CliExit.ResolveFailed, "", fix(release), Playback.Held)
      yield matches(missing, before) &&
        matches(Law(List("version"), CliExit.Success, shown, "", Playback.Held), version) &&
        matches(latestLaw, latest) &&
        matches(updateLaw, update)
    }

  private def files: ZIO[Path & TestEnvironment & Scope, Nothing, TestResult] =
    open("files") { repo =>
      val store   = repo.resolve("cache-store")
      val tally   = repo.resolve(".histos").resolve("observation.json")
      val observe =
        s"no observation at $tally\nCompile once so the tally is written.\nfix  histos compile"
      val forgotten = "content-store entries for this repo would be forgotten"
      val laws      = List(
        Law(List("cache", "path"), CliExit.Success, store.toString, "", Playback.Held),
        Law(List("cache", "list"), CliExit.Success, "", "", Playback.Held),
        Law(List("cache", "clean", "--dry-run"), CliExit.Success, "would remove nothing", "", Playback.Held),
        Law(List("cache", "clean"), CliExit.Success, "removed nothing", "", Playback.Held),
        Law(List("clean"), CliExit.Success, "cleaned", "", Playback.Held),
        Law(List("cleanFull"), CliExit.Success, "cleaned\nremoved nothing", "", Playback.Held),
        Law(List("preview", "clean"), CliExit.Success, previewRoots, "", Playback.Held),
        Law(List("preview", "cleanFull"), CliExit.Success, s"$previewRoots\n$forgotten", "", Playback.Held),
        Law(List("setup", "supply-chain"), CliExit.Usage, "histos.scala is missing\n", "", Playback.Held),
        Law(List("observe"), CliExit.TaskFailed, "", observe, Playback.Heard),
      )
      run(repo, laws)
    }

  private def scaffold: ZIO[Path & TestEnvironment & Scope, Nothing, TestResult] =
    for
      root <- ZIO.service[Path]
      repo <- LawDir.child(root, "scaffold")
      _    <- TestSystem.putEnv("HISTOS_VERSION", "9.9.9")
      here    = repo.toAbsolutePath.normalize
      created = repo.resolve("demo").toAbsolutePath.normalize
      store   = repo.resolve("cache-store")
      done <- wrote(repo, here, created).provideSomeLayer[TestEnvironment & Scope](services(repo, store))
    yield done

  private def wrote(repo: Path, here: Path, created: Path) =
    val initLaw = Law(List("init", "--bare"), CliExit.Success, s"$here\nhistos test", "", Playback.Held)
    val newLaw  = Law(List("new", "--bare", "demo"), CliExit.Success, s"$created\nhistos test", "", Playback.Held)
    for
      _    <- failLoad
      init <- Cli.execute(initLaw.args, repo, apiJar)
      made <- Cli.execute(newLaw.args, repo, apiJar)
    yield matches(initLaw, init) && matches(newLaw, made)

  private val parseLaws: List[Law] = List(
    usage(List("publish"), fix("publish is snapshots, release, assets, promote, or drop")),
    usage(List("run"), fix("run needs one name")),
    usage(List("--explain"), fix("--explain needs a check id")),
    usage(List("--explain", "jdk.source", "node.floor"), fix("--explain takes one check id")),
    usage(List("update"), fix("histos update is run by the installed shim\nhistos help update")),
    usage(List("self"), fix("histos self update is run by the installed shim\nhistos help self")),
    usage(
      List("toolchain", "install"),
      fix("histos toolchain install <version> is run by the installed shim\nhistos help toolchain"),
    ),
    usage(List("toolchain", "list"), fix("histos toolchain list is run by the installed shim\nhistos help toolchain")),
    usage(List("cache", "zzzzzzzzzz"), unknown("zzzzzzzzzz")),
    usage(List("deps", "zzzzzzzzzz"), unknown("zzzzzzzzzz")),
    usage(List("preview", "zzzzzzzzzz"), unknown("zzzzzzzzzz")),
    usage(List("setup", "zzzzzzzzzz"), unknown("zzzzzzzzzz")),
    usage(List("~"), fix("watch needs a command")),
    usage(List("~", "bsp"), "usage: histos help\nhistos help lists the commands.\nfix  histos help"),
  )

  private val helpLaws: List[Law] = List(
    Law(List("help"), CliExit.Success, helpOut, "", Playback.Held),
    Law(
      List("help", "version"),
      CliExit.Success,
      "histos version\nPrint the version this checkout runs.\n",
      "",
      Playback.Held,
    ),
    Law(
      List("help", "zzzzzzzzzz"),
      CliExit.Usage,
      "",
      "unknown command 'zzzzzzzzzz'\nhistos help\nhistos help <command>",
      Playback.Held,
    ),
    Law(List("--explain", "jdk.source"), CliExit.Success, jdkExplain, "", Playback.Held),
  )

  private val checkIds: Set[String] = Set(
    "jdk.source",
    "node.floor",
    "node.shadowed",
    "clang.source",
    "runner-tools.cover",
    "runner-tools.agree",
  )

  /** `ToolchainExplain.known` is map-key order, so the sentence is a set of ids. */
  private def unknownCheck(done: Completed): TestResult =
    val prefix = "no explanation for 'nope'. Known checks: "
    val listed =
      if done.err.startsWith(prefix) && done.err.endsWith(".") then
        done.err.drop(prefix.length).dropRight(1).split(", ").toList.filter(_.nonEmpty)
      else Nil
    assertTrue(
      done.exit == CliExit.Usage,
      done.exit.code == 2,
      done.out.isEmpty,
      done.playback == Playback.Held,
      listed.toSet == checkIds,
      listed.size == checkIds.size,
    )

  private val credentialLaws: List[Law] =
    val required = "SONATYPE_USERNAME and SONATYPE_PASSWORD are required"
    List(
      Law(
        List("portal"),
        CliExit.PublishFailed,
        "",
        required + "\nNeither variable is set. This is not a rejected login.\n" +
          "fix  Export SONATYPE_USERNAME and SONATYPE_PASSWORD.",
        Playback.Held,
      ),
      Law(
        List("publish", "promote", "deploy"),
        CliExit.PublishFailed,
        s"promoting deploy\n$required",
        s"$required\nfix  histos help publish",
        Playback.Heard,
      ),
    )

  private val heardLaws: List[Law] = List(
    heard("compile"),
    heard("test", "core"),
    heard("testOnly", "demo.Spec"),
    heard("testQuick"),
    heard("run", "inspect"),
    heard("run", "demo"),
    heard("run", "demo/site"),
    heard("client"),
    heard("assembly"),
    heard("publishLocal"),
    heard("publish", "snapshots"),
    heard("publish", "release"),
    heard("publish", "drop", "deploy"),
    heard("publish", "assets"),
  )

  private val heldLaws: List[Law] = List(
    held("graph"),
    held("workflow"),
    held("toolchain"),
    held("server"),
    held("resolve"),
    held("preview", "compile"),
    held("preview", "test"),
    held("preview", "publishLocal"),
    held("preview", "publish", "snapshots"),
    held("preview", "run", "inspect"),
    held("preview", "run", "demo/site"),
    held("preview", "clean", "core"),
    held("clean", "core"),
  )

  private val rawLoadLaws: List[Law] = List(
    raw("affected", "--base", "all"),
    raw("coverage"),
    raw("bsp"),
    raw("zzznone"),
  )

  private val rawTaskLaws: List[Law] = List(
    Law(List("doctor"), CliExit.TaskFailed, "", homeText, Playback.Held),
    Law(List("deps", "check"), CliExit.TaskFailed, "", homeText, Playback.Held),
  )

  private def heard(args: String*): Law =
    Law(args.toList, CliExit.LoadFailed, "", homeFix, Playback.Heard)

  private def held(args: String*): Law =
    Law(args.toList, CliExit.LoadFailed, "", homeFix, Playback.Held)

  private def raw(args: String*): Law =
    Law(args.toList, CliExit.LoadFailed, "", homeText, Playback.Held)

  private def usage(args: List[String], err: String): Law =
    Law(args, CliExit.Usage, "", err, Playback.Held)

  private def fix(text: String): String =
    s"$text\nfix  histos help"

  private def unknown(token: String): String =
    "unknown command '" + token + "'\n" +
      "histos help lists the commands.\n" +
      "histos help <command> prints one.\n" +
      "fix  histos help"

  private def batch(prefix: String, laws: List[Law]): ZIO[Path & TestEnvironment & Scope, Nothing, TestResult] =
    open(prefix)(repo => run(repo, laws))

  private def run(
      repo: Path,
      laws: List[Law],
  ): ZIO[Loader & Stubbed[Loader] & Resolve & ContentStore & Machine & Scope, Nothing, TestResult] =
    for
      _       <- failLoad
      results <- ZIO.foreach(laws)(law => Cli.execute(law.args, repo, apiJar).map(done => matches(law, done)))
    yield every(results)

  private def matches(law: Law, done: Completed): TestResult =
    val want = Completed(law.exit, law.out, law.err, law.playback)
    assertTrue(shown(law.args, done) == shown(law.args, want), done.exit.code == status(law.exit))

  private def status(exit: CliExit): Int =
    exit match
      case CliExit.Success         => 0
      case CliExit.Usage           => 2
      case CliExit.Program(0)      => 0
      case CliExit.Program(2)      => 2
      case CliExit.Program(_)      => 1
      case CliExit.TaskFailed      => 1
      case CliExit.Interrupted     => 1
      case CliExit.LoadFailed      => 1
      case CliExit.ToolchainFailed => 1
      case CliExit.ResolveFailed   => 1
      case CliExit.WorkflowFailed  => 1
      case CliExit.TestFailed      => 1
      case CliExit.CompileFailed   => 1
      case CliExit.PublishFailed   => 1

  private def shown(args: List[String], done: Completed): String =
    args.mkString(" ") + "\n" + done.exit.toString + " " + done.exit.code.toString + " " + done.playback.toString +
      "\n" + done.out + "\n---\n" + done.err

  private def every(results: List[TestResult]): TestResult =
    results match
      case head :: tail => tail.foldLeft(head)(_ && _)
      case Nil          => assertTrue("batch" == "had a law")

  private def failLoad: URIO[Stubbed[Loader], Unit] =
    stub[Loader](_.load(any[Path], any[Path]))(ZIO.fail(LoadError.NoHome)) *>
      stub[Loader](_.loadLocked(any[Path], any[Path], any[histos.resolve.DependencyLock]))(
        ZIO.fail(LoadError.NoHome)
      )

  private def open[A](prefix: String)(
      use: Path => ZIO[Loader & Stubbed[Loader] & Resolve & ContentStore & Machine & Scope, Nothing, A]
  ): ZIO[Path & TestEnvironment & Scope, Nothing, A] =
    for
      root <- ZIO.service[Path]
      repo <- LawDir.child(root, prefix)
      store = repo.resolve("cache-store")
      result <- use(repo).provideSomeLayer[TestEnvironment & Scope](services(repo, store))
    yield result

  private def services(repo: Path, store: Path) =
    stubbed[Loader] ++
      stubbed[Resolve] ++
      ZLayer.succeed(ContentStore.in(store)) ++
      ZLayer.succeed(machine(repo, store))

  private def machine(repo: Path, store: Path): Machine =
    Machine(Map.empty, "/home", Os.Mac, Architecture.X86_64, store, repo, None, NativeOrigin.Central)
end ExecuteLawSpec
