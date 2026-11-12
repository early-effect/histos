# Test audit

This is the ledger for the Test audit item of [cli-work-plan.md](cli-work-plan.md) (row 49). Eight read-only reviewers read 1,401 laws in `api`, `wire`, `central`, `engine`, and `app`, and flagged 368. Each row names a finding, the fix, and the pull request that owns it. A row is checked only when that pull request's proof earned it.

## The bar

A law earns its place when it:

- asserts a property a user or this repository depends on
- fails when that property breaks (shown against a broken implementation where that is cheap)
- is deterministic: no wall-clock race, no yield-count wait, no port that another process can take
- is hermetic: no network, no write outside the law's own directory, no read of the developer's home, git config, or PATH

A law that tests ZIO, zio-json, the JDK, or Scala is deleted. A law that restates the code it calls (the same `match`, a constructor's arguments read back, an enum's `values`) is deleted or rewritten against literals. A duplicate is merged into the law that keeps the stronger assert.

Verdicts: **DELETE**, **MERGE**, **REWRITE**, **WEAK** (the assert cannot fail on the property in the name), **HOST** (reads or writes the machine outside the law), **FLAKY** (passes or fails on unchanged code).

## Pull requests

Each is one layer of the stack, in this order.

| PR | Scope |
| --- | --- |
| api | `api/test`, and the api sources its rows change |
| wire | `wire/test`, and `wire/src` |
| central | `central/test`, and `central/src` |
| engine A-L | `engine/test` specs A to L, the shared compiler fixture (S1), the ivy root (S2), and the dead code they test |
| engine M-Z | `engine/test` specs M to Z |
| app | `app/test`, and `app/src` |

## Product bugs

Each bug gets a law that fails on today's code, then the fix.

- [x] B1 `Cond.Not` renders `!github.event_name == 'push'`. GitHub reads that as `(!github.event_name) == 'push'`, which is always false, so a job that negates an event, input, or branch never runs. `Not` renders `!(…)` always. Owner: engine A-L (`JobCondLawSpec`), and api checks `JobCheckLawSpec:108`.
  - Red: `JobCondLawSpec` rendered `if: "!github.event_name == 'push'"`. Green: `if: "!(github.event_name == 'push')"`, 3 passed. This repository's build renders no `Not`, so its workflows did not move, and `./histos workflow --check` is clean.
- [x] B2 `JobYaml.lift` ends in `case other => CiError.UnpinnedWorkflowRun(other.message)`, so a `BadName` or a `DuplicatePlugin` reports as an unpinned workflow run. `CiError` gets an honest case for a non-CI build failure. Owner: engine A-L, and engine M-Z points `WorkflowLawSpec:293` at it.
  - The loader adds the wiring's plugins after `Projects.build` checked its jobs, so a reserved or repeated wiring job first fails in `JobYaml.render`. `lift` now takes a `CiFault`, and every other arrange fault stays the `BuildError` it is (`Either[CiError | BuildError, _]` through `Workflow`). `Calls.workflowWork` reports it as `LoadError.BrokenGraph`, so no new `CiError` case was needed. Red: `JobYamlLawSpec` got `UnpinnedWorkflowRun("job test is reserved")` and `UnpinnedWorkflowRun("job nightly is declared twice")`. Green: `PluginGraph(ReservedJob(test))` and `PluginGraph(DuplicateJob(nightly))`, 11 passed.
- [x] B3 `Decisions.scala:133` names a cross-version split's cells with the coordinate (`split.toList.map(CellName.trusted)` over the groupBy keys), and calls `split.head`. The cells are the rows' `cell`. Owner: engine M-Z (`SupplyDecisionLawSpec:135`).
  - Red: a split of `org.example:demo` named the cells `List(org.example:demo)`, and a lock that split two coordinates named both coordinates as cells of the one it reported. Green: `CrossVersionSplit(demo, List(core, other))`, and the first coordinate in lock order with its own cells. The fault is a `collectFirst`, so the `split.head` is gone.
- [x] B4 `VersionOrder` collects every digit run, so `1.2.3-RC1` sorts above `1.2.3`, and `NodeToolchain.highest` prefers `v25.0.0-rc.1` over `v25.0.0`. A pre-release sorts below its release. Owner: engine M-Z (`ToolchainLawSpec:13`).
  - Red: `compare("1.2.3-RC1", "1.2.3")` was 1, `atLeast("25.0.0-rc.1", "25")` was true, and `shadow` chose `/rc/bin/node` in either PATH order. Green: a version is the numbers before `-`, the pre-release numbers, then the `+` build, so a pre-release sorts below its release and above the one before it, `RC10` sorts above `RC2`, and `shadow` chooses `25.0.0`. A JDK floor reads `JAVA_VERSION`, which carries no pre-release, so the JDK check does not move. A Node `25.0.0-rc.1` is now below the Node floor of 25.
- [x] B5 `DeploymentId.from` fails a mistyped id with `PortalError.Unnamed`, the portal-response error, so the operator reads "the portal upload did not name a deployment". It gets its own error case. Owner: central, and engine M-Z updates `ReleaseRunLawSpec:168`.
  - Proof: `PublishIdentityLawSpec` "a deployment id names an empty or spaced id" was red on the old parser (`"  "` and `" a b "` both read "the portal upload did not name a deployment"). Green with `DeploymentId.Error` (`Empty`, `Spaced(text)`). `ReleaseError.Deployment` carries it, so `publish drop "has space"` prints "deployment id 'has space' contains whitespace" (`ReleaseRunLawSpec`, edited only to keep engine green). The portal client still maps an unparseable upload body to `PortalError.Unnamed`.
- [x] B6 The daemon outbox `close` is `outbound.shutdown *> fiber.join`. A shut-down queue does not hand its remaining lines to the sender, so the BSP exit `Marker` that `Daemon.scala` offers just before `close` can be dropped. Prove the queue behavior in a miniature spec first. Owner: app (`DaemonOutboxLawSpec`).
  - Miniature (scala-cli, deleted): with the first send held on the test clock and two lines queued behind it, `fromQueueWithShutdown` then `shutdown *> join` sent `Chunk("one")`, 50 of 50 runs, and a queue of `Option[String]` ended by `None` sent all three, 50 of 50. The outbox now offers `Some(line)`, `close` offers `None` and joins, and the scope shuts the queue. Red: `DaemonOutboxLawSpec` got `Chunk("one")` for `Chunk("one", "two", "marker")`. Green, and green under `nonFlaky(50)`.
- [x] B7 The MCP `run` tool accepts an empty id (`runCall("", None, empty) == Right(Call.Run("", None))`), while CLI `run ""` is `BadTaskId` and MCP preview is `MissingRunName`. Parity: reject it. Owner: app (`SessionLawSpec:22`).
  - `Session.runCall` parses the id with `TaskId.from` first and refuses an empty one with `Rejection.MissingRunName`, the MCP preview's refusal, with or without a platform or arguments. Red: the rows got `Right(Run("", None))`, `Right(Run("", Some(Jvm)))`, and `TaskArguments("")`. Green.
- [x] B8 `NO_COLOR=""` turns colour off. no-color.org disables colour only for a present, non-empty value. `Paint` also reads `System.getenv` outside ZIO. Owner: app (`PaintLawSpec:14`).
  - `Paint.ink(Stdout, Option[String]): Ink` decides, `Paint.chosen` reads `NO_COLOR` through `zio.System`, and `paint` takes the `Ink`, so the boolean and the string parameter are gone. Red: `paint(line, tty = true, Some(""))` was the plain line. Green: the table over `Stdout` and `NO_COLOR`, and `TestSystem.putEnv` of `""` and `"1"`. The terminal probe was still a lazy val that started `/bin/sh` once. P51 moved it into a layer, and `Stdout` is now `Attached`.
- [x] B9 `Perform.jdkMissed` has a `Test` branch that is the same as every other call, and its scaladoc claims a distinction that does not exist. Delete it. Owner: app (`PerformLawSpec:46`).
  - `Faults.testStopped(ModuleTestError.Compile(e))` is `Faults.compileStopped(e)` by definition, so the branch had no behaviour of its own and no law could fail on it. The table law passed before the deletion and passes after it, over test, preview test, compile, format, drop, and version. The scaladoc now says what the function does.
- [x] B10 `JobCheck` reads `Not(c)` as reachable exactly when `c` is. `Not(Event(Schedule))` in `ci.yml`, which has no schedule and so is always true, is refused as `UnreachableCond`, and `Not(True)`, which never runs, is accepted. `Not(c)` is reachable when `c` can be false for the file's events, and refused when `c` is always true. The same model found `DefaultBranch` refused on a schedule, which always runs on the default branch. Rendering is B1. Owner: api (`JobCheckLawSpec`).
- [x] B11 `PortalServerLawSpec` runs a real heddle server on TestClock, and the suite can hang forever (PR #165 CI hit its 30-minute timeout). heddle halts a server on the Clock it was installed under (`ServerPlatform.scala:36`). The halt polls a busy connection with `Schedule.spaced(10.millis)` up to `gracefulShutdownTimeout` (`LiveConnections.scala:62`). A connection clears `busy` only after `writeResponse` returns (`Http1.scala:56-75`), so the client can read the last response and close the scope first. The uninterruptible finalizer then sleeps on a TestClock nobody advances. heddle documents that contract (`ServerHaltSpec`), so the fault is the law's. Owner: central.
  - Proof: an idle machine ran `nonFlaky(200)` clean. Under 36 CPU burners on 12 cores it hung at iteration 81 with the CI warnings, and the fiber dump showed the halt in `LiveConnections.waitUntilIdle`. A miniature that holds the server's write open after the client has the whole response stayed open on TestClock after the connection went idle until `TestClock.adjust(10.millis)`, and returned on the live clock. With `TestAspect.withLiveClock` on the suite, `nonFlaky(200)` and `nonFlaky(1000)` ran clean under the same load.
- [x] B12 `ShipLink` checked the catalog rules with positions, then `Catalog.from` checked them again with none, and `asBuild` turned that second refusal into a made-up error: `SourcePos("<catalog>", 1, 1)`, a `DuplicateShip` with one position twice, or a `BadShipName` for a duplicate. `Catalog.from` takes each row with the place it was written and is the one check, so every refusal carries a real position and the fallback cannot be written. Owner: api (`CatalogLawSpec`).
- [x] B13 A wiring or plugin-module compile failure is the bare compiler message (`Not found: law`), with no file or line, and `compileSources` claimed the message kept its path. Each placed diagnostic is now `file:line:column: error: message` through `Lines.located`, with the wiring's label. Owner: engine A-L (`LoaderProjectLawSpec`, `LoaderPluginLawSpec`).
  - Red: `Not found: law`, `Illegal start of toplevel definition`, and `Found: ("no" : String) Required: Int`, unplaced. Green: `histos.wiring.scala:4:…: error: Not found: law`, `histos.wiring.scala:1:…: error: Illegal start of toplevel definition`, and `…/law/Mark.scala:4:…`, 10 passed.
- [x] B14 `CompileMemo` stamped the lock by modified time and size, so a rewrite of the same size in the same tick served the old lock. `CompileMemoLawSpec` failed that way in a full engine run. The stamp is now the sha256 of the bytes it parses. Owner: engine A-L (`CompileMemoLawSpec`).
  - Red: with the modified time put back, the reread lock was the first one. Green: the second lock, with a new stamp, 3 passed. The source digest memo keys on modified time and size the same way. That is D7.
- [x] B15 `DepsSurface.githubName` fell back to `early-effect/histos` when neither `GITHUB_REPOSITORY` nor `origin` named a GitHub repository, so `deps pr`, `deps alert`, and `deps snapshot --submit` in any other checkout wrote to this repository. With no name, each one fails with "no GitHub repository: set GITHUB_REPOSITORY or add a github.com origin" and sends nothing. Owner: engine M-Z (`SurfaceExchangeLawSpec`).
  - Red: `snapshot` returned `submitted early-effect/histos` and POSTed `repos/early-effect/histos/dependency-graph/snapshots`. Green: the snapshot and the pull request are that refusal, and no URL is sent.
- [x] B16 `NodeVersion.from` refused `v25.0.0-rc.1`, so a Node release candidate failed as an unreadable version before B4's ordering applied. A version takes an optional pre-release and build, and sorts below its release. Owner: engine M-Z (E82).
- [x] B17 A client that exits does not end its run on the server. A hung suite kept running after its client was killed, held the writer permit, and blocked the next run. A connection's call now races a read of its socket, which ends when the peer closes, so the run is interrupted and its locks are released. `bsp` reads the socket itself, and `reboot` starts its successor after the client heard it, so those two are not raced. Owner: app (`DaemonOpenLawSpec`).
  - Red: a law opens a server whose first build load runs until it is interrupted, changes `histos.scala`, sends `graph`, waits for the load, and closes its socket. The load was never interrupted, and the law hit its 30 s timeout. Green: the load's interrupt fires, and the next `graph` loads again and answers `daemonlaw jvm 3.10.0`, under `nonFlaky(10)`.
- [x] B18 The MCP tools fail with `CallProblem`, and its schema titled two shapes `SessionError`: the publish session's error (`Fault`, through `BuildFault.Version` and `VersionError.Session`) and the client's call error (`Transport`). A reader of the error schema could not tell them apart. The client's type is `Session.TransportError`, the error of a call that reached no result. Owner: app (`McpLawSpec`, P53).
  - Red: "a title in the tool schemas and the errors the tools fail with names one shape" walks `tools/list` and the error schemas of `CallProblem` and `ObserveError`, skips a sum case's own title (a case name such as `Io` is not a type), and got `List("SessionError")`. Green: no title names two shapes.
- [x] B19 `setup supply-chain` ran `SetupSurface.run`, which reads and writes files, inside `ZIO.succeed`. A build it could not read died with `java.nio.charset.MalformedInputException` and a stack trace. The run is `ZIO.attemptBlocking`, refined to `IOException`, and a failure is the line `setup supply-chain could not read or write a file under <repo>` with the failed status. Owner: app (`ExecuteLawSpec`, P50).
  - Red: "setup supply-chain on a build it cannot read fails with a line", on a `histos.scala` of the bytes `ff fe`, died with `MalformedInputException` at `SetupSurface.run`. Green: the failed line, exit 1.

## Decisions for Russ

- [x] D1 `fmt` rewrites sources a compile reads. Does it hold the writer permit? The recommendation is yes. Today `Call.Format` is missing from the permit and `Perform` tables (`DaemonWatchLawSpec:37`, `PerformLawSpec:42`). Owner: app.
  - Russ: fmt holds the writer permit.
  - Proof: `Call.Format` is in `Daemon.writesOutputs`, and both tables have a format row. `DaemonWatchLawSpec` "a format and a compile in one checkout run one after the other" leases a format that holds for a second of test clock and then a compile. Red: `List("compile", "format")`. Green: `List("format", "compile")`, under `nonFlaky(50)`.
- [x] D2 `~ publishLocal <x>` watches every module, and `preview publishLocal <x>` watches only the build file, because `PublishLocal` and `PublishSnapshots` fall through `case _ => Every` in `Watching`. The recommendation is to route both through `publish(graph, tokens)`. Owner: app (`DaemonWatchLawSpec:41`).
  - Russ: the watch follows the selection.
  - Proof: `Watching` routes `PublishLocal` and `PublishSnapshots` through `publish(graph, tokens)`. `DaemonWatchLawSpec` "a publish watch follows the selection the preview shows" compares both calls with their preview over no tokens, a module, a ship, a module in no ship, two modules, an unknown token, and a ship with an unknown token. Red: every non-empty selection watched every module. Green, with `publishLocal api` watching api and the shared roots, and `publish snapshots lib` watching the ship's two modules.
- [x] D3 The rehearsal portal reports no finding for an empty bundle, so it marks an empty upload `VALIDATED`. The recommendation is that an empty bundle is a layout finding. Owner: central (`PortalReportLawSpec:42`).
  - Russ: an empty bundle is a layout finding, and the rehearsal portal must not mark an empty upload `VALIDATED`.
  - Proof: `PortalReportLawSpec` "a non-zip, an empty zip, ..." was red (`Report(Chunk(), Chunk())`), and `PortalServerLawSpec` "an empty bundle fails in either mode" was red (`VALIDATED` in hold, `PUBLISHED` in automatic). Both are green with a layout finding for the whole upload when a zip has no files, which also covers a zip of directory entries only. C13 made that finding `Finding.Layout(LayoutPlace.Upload)`.
- [x] D4 The rehearsal portal accepts a `.sha256` file with a wrong digest. The recommendation is that a checksum file that is present is checked. Owner: central (`PortalReportLawSpec:89`).
  - Russ: any checksum file that is present is checked. `.md5` and `.sha1` stay required. `.sha256` and `.sha512` are optional, and one that is present must match.
  - Proof: `PortalReportLawSpec` "a sha256 or sha512 that is present and wrong is a checksum" was red (the wrong `.sha256` passed, and `.sha512` was a layout finding), and "sha256 and sha512 are allowed" was red on `.sha512`. Both are green.
- [x] D5 `JobCheck` read tag and label conditions from the triggers alone. A `workflow_dispatch` run from a tag never made `Event(Tag)` or `TagMatches` reachable, and `HasLabel` needed a labeled pull-request type, so `ci.yml` refused both. Owner: api (`JobCheckLawSpec`).
  - Russ: reachable means GitHub can make it true. A dispatch runs from any ref, so a ref condition (`TagMatches`, `Event(Tag)`, which renders as a ref test, and `DefaultBranch`) is reachable in a file with dispatch. An event condition is the event name the start sets, so dispatch never makes a push condition true. A label condition reads `github.event.pull_request`, so it is reachable in a file with any `pull_request` trigger.

## Shared seams

A law that touches the machine because the product does needs a seam in the product.

- [x] S1 Compiler closure. About 30 laws compile with Zinc through `Resolve.offline(CacheDefaults.location)` and this checkout's `dependencies.lock`, found by walking up from the working directory, with the Scala version written as a literal. A compile-cache hit fetches nothing, so on a cold Coursier cache these pass or fail with run order (row 48). The fix: one shared fixture in `engine/test` reads the lock's compiler tool rows offline, takes the Scala version from the lock, and fails with a named precondition ("run `./histos resolve`") when a jar is missing. `./histos resolve` is the documented step, and the setup action runs it in CI. Owner: engine A-L. engine M-Z and app move their copies onto the same shape.
  - `histos.CompilerClosure` (`engine/test/CompilerClosure.scala`): `CompilerClosure.layer: ZLayer[Any, CompilerClosure.Missing, Resolve & CompilerClosure]` is Coursier's cache read offline, the checkout's lock, and every jar of its compiler rows fetched before the first law. `scalaVersion` is the newest locked Scala 3 compiler, `older` the newest of an older series, `pinning(modules*)` the lock with empty JVM cells for a law's modules, and `held(role)` the same offline check for another tool. `Scenario.tools` and the seven per-spec walks in A-L are gone. Proof: `CompilerClosureLawSpec`, where an empty cache is `Missing.Jar` on a compiler row and its message ends with `./histos resolve`. The seven specs and `ScenarioSpec` pass on it.
- [x] S2 `IvyLocal` writes Coursier's static `LocalRepositories.ivy2Local`, so `FilePublishLawSpec`, `IvyLocalLawSpec`, and `PublishInstallLawSpec` write and wipe the real `~/.ivy2/local`, and two worktrees delete each other's install. `IvyLocal` and `LocalStock.layer` take the root. Owner: engine A-L.
  - `IvyLocal.install(root, module)`, and `LocalStock.layer(note): URLayer[IvyRoot, LocalPublish]`. `PublishRun.local` needs `IvyRoot`, and `Calls.publishLocal` provides `IvyRoot.ivy2Local`. Red: `IvyLocalLawSpec` wrote `user.home/.ivy2/local/histos.law`. Green: the three laws install under `IvyRoot.at` their own directory, and `~/.ivy2/local` has no `histos.law`, `histos.filelaw`, or `histos.runlaw` after the run.
- [x] S3 `Cli.launch` provides `ReleaseFeed.live` inline. Once a law writes a pin, every launch fetches `repo1.maven.org` and writes `$HOME/.histos/upgrade`. `launch` requires `ReleaseFeed` and `Main` provides the live layer. Owner: app.
  - `Cli.launch` requires `ReleaseFeed`, and `Main` provides `ReleaseFeed.live` (beside `Resolve`, P54, and `Stdio`, P51). The four specs that launch (`CliClasspathLawSpec`, `CliGapLawSpec`, `CliPlanLawSpec`, `CliReadyLawSpec`) provide `stubbed[ReleaseFeed]`, which dies if it is read, and set `user.home` to the law's directory through `TestSystem`. The two that launched with a pin under `withLiveSystem` (`CliLaunchLawSpec`, `CliLocalLawSpec`) are gone (P12 to P15), so no law reads the feed or the real home.
- [x] S4 `SonaStock`'s snapshot repository is fixed to `SnapshotRepository.central`, so only validation order keeps `SonaPublishLawSpec` off central.sonatype.com. The repository becomes a seam, and the law points it at a loopback that fails any request. Owner: engine M-Z.
  - `SonaStock.layer(env, note): URLayer[SnapshotRepository, SonaPublish]`, and `PublishRun` provides `SnapshotRepository.central`. `RefusingLoopback` (`engine/test`) holds a loopback port for the law's scope and closes every connection unread. The sona laws assert the loopback saw no connection, and a staged module that passes every check is a `PublishFault.Transport` with a connection counted, which fails if the stock ignores its repository. `SignerLawSpec` and `ReleaseRunLawSpec` use the same loopback where they named `127.0.0.1:9`. The red run was not taken: a stock that ignores the layer would send that request to Central.
- [x] S5 The supply surface reads `sys.env` (`GITHUB_REPOSITORY`, `HISTOS_DRY_RUN`) and runs `git` synchronously outside ZIO (`Surface.scala`). The laws test whichever branch the shell selects. Both go through `zio.System` and a git reader. Owner: engine M-Z.
  - `GitRead` (`origin`, `head`, `tip`, `changed`) is a service with `GitRead.live`, and `DepsSurface.pullRequest`, `alert`, `snapshot`, and `DoctorSurface.runWith` need it. Every variable, `GITHUB_SHA` and `GITHUB_REF` too, is read through `zio.System`, and the advisory cache reads `user.home` through it and writes nothing without one. `DoctorSurface` read the host's `sys.env` for the central credentials check while `runWith` was handed an environment; it reads that environment now. `pullRequest` lost its `files` parameter, which only laws passed: they stub `changed`. `GitAnswers` (`engine/test`) answers git from fixed values. Proof: `SurfaceExchangeLawSpec` and `SupplySurfaceLawSpec` assert the live and the dry branch under `TestSystem` (an empty token is `GH_TOKEN is not set` live and the plan dry, and a dry pull request prints its calls and sends none), the `GITHUB_REPOSITORY` and `GITHUB_SHA` a job sets, and the repository `origin` names. B15 came out of this seam.
- [x] S6 `SnapshotId.current` runs the real `git` on JVM and Native with the ambient environment and global config. The HEAD read becomes a dependency the law stubs. Owner: wire.
  - `SnapshotId.current(repo, head)` takes the HEAD reader, `ApiClock` and `ToolchainStamp` pass `SnapshotId.head`, and `ApiClockLawSpec` stubs it with a recording `Ref` (HEAD is read only for an `app` ship). No wire law starts a process; `./histos test wire` is green on JVM and Native.
- [x] S7 `FormatterLawSpec` uses `Resolve.retained`, which downloads a missing scalafmt jar from Central, and reads this checkout's `.scalafmt.conf`. The scalafmt jars are read offline like S1, and the law writes its own config. Owner: engine A-L.
  - The suite runs on `CompilerClosure.layer`. `held(ScalafmtDynamic)` and `held(ScalafmtCore)` read both rows offline, and the configuration's version is the `scalafmt-core` root the lock pins. Proof: `FormatterLawSpec`, 6 passed.
- [x] S8 Laws read `TestClock.instant` and discard it (`_ <- Clock.instant`), which only starts the "not advancing the test clock" warning. Delete each line: `PackLawSpec`, `PreviewLawSpec`, `PublishLawSpec` (and its helpers), `ScaffoldLawSpec`, `SignerLawSpec`. `TaskLawSpec:62` sleeps an hour on an unadjusted clock where `ZIO.never` is the intent. Owner: engine M-Z.
  - Every read is gone, `engine/test` reads no `Clock`, and the cancelled task is `ZIO.never`.

## api

- [x] A1 `BuildNameLawSpec:52` MERGE. The same parsers as `PinTextLawSpec:65`. One ProviderLibrary, TestProvider, FrameworkClass law.
- [x] A2 `BuildNameLawSpec:65` WEAK. L73 compares `Resolver.Central` to itself, L74 tests enum `!=`. Delete L73-74.
- [x] A3 `BundleLawSpec:81, :85, :89` WEAK. `errors.nonEmpty` accepts any compile error. Assert the macro's message ("has no layer", "is not an object", "is not a HistosPlugin and has no layer").
- [x] A4 `CiArrangeLawSpec:78` REWRITE. Copies `Combine.on` and `Combine.cond`. Assert through `JobCheck.arrange` that a `Ci ++ Dispatch` job carries both event lists, or delete.
- [x] A5 `GraphOpenLawSpec:17` MERGE. The ship asserts and helpers repeat `GraphShapeLawSpec:206`. Drop them here.
- [x] A6 `GraphShapeLawSpec:43` WEAK. Passes `Nil` modules, so `chain` is empty for any implementation. Pass `List(module)`.
- [x] A7 `GraphShapeLawSpec:141` WEAK. `nonEmpty` on any snippet error. Assert the missing `<:< P` evidence text, with `@implicitNotFound` if needed.
- [x] A8 `GraphShapeLawSpec:228` DELETE. Restates `def graph = Right(found)`.
- [x] A9 `OrganizeLawSpec:28` (and the `:40` overloads) WEAK. `Uses(Nil)` and `Shape` never reads plugins. Use `use(Echo)` and add plugin ids to `Shape`.
- [x] A10 `PinTextLawSpec:36` WEAK. Enum inequality, `val empty`, and case-class `copy`. Keep the message rows only.
- [x] A11 `PluginGrammarLawSpec:58` MERGE. The same `Cron.from` cases as `PluginSpec:229`. One Cron table.
- [x] A12 `PluginMessageLawSpec:60` REWRITE. Pins `"plugin task cycle "` for `Cycle(Nil)`, which the engine never builds. `Cycle(path: ::[PluginTaskId])` and delete the row.
- [x] A13 `PluginSpec:119` REWRITE. Copies the `reserved` literals. Assert `JobCheck.generated(g) ⊆ JobName.reserved` here. app asserts every Grammar built-in word is in `CommandName.reserved`.
- [x] A14 `PluginSpec:247` DELETE. Asserts the test's own `linkName` helper. Add the `EngineLeak(Published)` and `Defect(Module)` rows to `PluginMessageLawSpec:22` if they are missing.
- [x] A15 `PluginSpec:296` DELETE. Reads back `PluginFixture` defaults and repeats `ArgsLawSpec:15`.
- [x] A16 `PluginSpec:535` WEAK. `schedule.nonEmpty` on any error, and a fixture named `cron` with no cron. Assert the error names `cron`, and rename the fixture.
- [x] A17 `PluginSpec:570` MERGE. The same `JobCheck` arms as `JobCheckLawSpec:46`. One law with the exact `CiFault` values.
- [x] A18 `PomLawSpec:40` DELETE. Copies the `word` match, proven in full at `LocalPublishLawSpec:80`.
- [x] A19 `PublishLawSpec:23` WEAK. Reads back stub arguments. Read `secretNames` and `followUp` from `Sona.layer(mode)`.
- [x] A20 `PublishLawSpec:50` DELETE. `fromEffect` returns its arguments.
- [x] A21 `SessionLawSpec:64` REWRITE. Nine fixed asserts rerun inside `check`. Keep the two generated round trips there and move the fixed rows out.
- [x] A22 `ShipDraftLawSpec:17` DELETE. Reads back `trusted` constructor arguments.
- [x] A23 `ShipDraftLawSpec:46` DELETE. Tests case-class `copy`.
- [x] A24 `SupplyDiagnosticLawSpec:172` WEAK. L209 repeats `Sight.values`. Delete L209.
- [x] A25 `SupplyNameLawSpec:85` MERGE. The same CVSS vectors as `CvssBandLawSpec`. Move the 3.0 refusal and the new vectors into that table.
- [x] A26 `SupplyPolicyLawSpec:60` DELETE. One-line forwarders.
- [x] A27 `SupplyPolicyLawSpec:97` MERGE. Add `ServiceDown(_, FailOpen)` to `SentenceLawSpec:39`.
- [x] A28 `SupplyPolicyLawSpec:110` MERGE. Add the two-sha `HumanCommitsOnBranch` row to `SentenceLawSpec:39`.
- [x] A29 `ToolPinLawSpec:32` WEAK. Uses `JobTimeout.admits` as its own oracle. The oracle is the literal `1 <= raw && raw <= 360`.
- [x] A30 `ToolPinLawSpec:66` WEAK. L82-84 repeat enum `values`. Delete them.
- [x] A31 `VersionDecisionLawSpec:147` WEAK. `Gen.int(0, 3)` cannot tell string order from number order. Widen, and add `1.9.0 < 1.10.0`.
- [x] A32 `VersionOpenLawSpec:99` REWRITE. Builds the expected text from `ShipAdvice`'s own parts. Assert literal sentences.
- [x] A33 `JobCheckLawSpec:108` Check `Not(Event(Schedule))` against B1. Closed by B10: the row asserted the reachability rule, not the rendering, and that rule was wrong.
- [x] A34 B10.
- [x] A35 `ShipAdvice.incomplete` runs its parts together ("default or other export HISTOS_SESSION=…"). Each part is its own sentence, and exactly one period ends each, even when a `SessionError` message already ends with one. Pin the literal text.
- [x] A36 WEAK. Table laws assert `forall` and never name the failing row: `PluginMessageLawSpec`, `PluginGrammarLawSpec`, `ToolPinLawSpec`, `PublishLawSpec`, `SupplyPolicyLawSpec`, `SupplyNameLawSpec`, `SupplyDiagnosticLawSpec`, and also `DoctorModelLawSpec`, `GraphLawSpec`, `HygieneLawSpec`, `StepLawSpec`, `VersionLawSpec`. Collect the misses and assert `misses == Nil`.
- [x] A37 WEAK. `JobCheckLawSpec` compares `productPrefix` strings through its `fault` helper. Assert exact error values.
- [x] A38 WEAK. `GraphShapeLawSpec` helpers fall back to `getOrElse("")` and `TestStore.Off`, so a missing module can pass. A missing module fails the law.
- [x] A39 DELETE. `ShipDraftLawSpec` `asked.description == row.description` reads back `copy`.
- [x] A40 `Sona.described` has no production caller. Move it into the engine test sources that use it, and delete it from production.
- [x] A41 `SupplyFault.Advisory` prints a severity through enum `toString`. Render it with an explicit, total match, and pin the text.
- [x] A42 `JobCheck` uses `isInstanceOf` for its event tests. Use a pattern match or an exhaustive function.
- [x] A43 `BuilderMacro.module` widens its `Expr` with `asInstanceOf`. `asExprOf` checks the type and keeps the tree, so `module(...)` still carries its platforms.
- [x] A44 WEAK. `GraphShapeLawSpec`'s `fault` helper and `GraphLawSpec`'s `kind` helper compare error names as `productPrefix` strings, so the wrong project, platform, or position passes. Assert exact error values, as A37 did.
- [x] A45 B12.
- [x] A46 D5. `JobCheck` evaluates the rendered expression on each start GitHub can give the file (its event name and the refs that run can be on), and a condition is reachable when it is not false on every start. One run has one event, so `All` of two event names, or of a label and a dispatch input, is refused.

## wire

- [x] W1 `JsonLaw.enumLaw` (113 call sites in `CodecLawSpec`, `ReportLawSpec`) REWRITE. Asserts zio-json's error text. Keep `covered` and the round trip, delete the rejection asserts (also `rejectedText`, `OpaqueLawSpec`, `ReportLawSpec:199, :299, :309`, `FrameLawSpec:117, :128`), and add golden documents for the sums the native client decodes.
  - A refusal that a Histos codec decides (a parser behind `parsed`, a token table) stays as `isLeft`, with no zio-json text. `GoldenLawSpec` pins one document per case of `Message`, `BuildEvent`, both feeds, `TestStatus`, `KeyMiss`, `CallResult`, `BuildFault`, `Rejection`, and `WorkflowFault`, the severity, position, and io tokens they carry, and the calls `Batch` sends.
- [x] W2 `CodecLawSpec:119-129`, `ReportLawSpec:321` DELETE. Thirteen field-less derived enums. A token that is a wire contract becomes a golden string, as `OpaqueLawSpec:109` does.
- [x] W3 `CodecLawSpec:130, :133, :134` MERGE. `reached` at L135 already carries all three.
- [x] W4 `OpaqueLawSpec:233-306` WEAK. L265 is `x == x`. L269 and L281 compare `Left` with `Left`. `Dep`'s `::` and `:::` forms are never round-tripped, and `binaryDep` and `fullDep` are unused. Assert each parse is `Right`, round-trip both forms.
- [x] W5 `SuitePattern` has no literal, so `CodecLawSpec:155`, `MessageLawSpec:1405-1469`, and `OpaqueLawSpec:236` fall back to `== Left(Empty)` and skip their law when `from` regresses. Add the literal and delete the fallbacks and the `toOption.toList` in `CallSamples`.
- [x] W6 `CliLawSpec:55, :72, :87, :161` REWRITE. The expected values come from copies of `CliExit`'s helpers. Assert `.code` per fault family against literals.
- [x] W7 `CliLawSpec:307-339` REWRITE. Re-implements `Lines` tally rendering. Assert the properties plus two literal lines.
- [x] W8 `CliLawSpec:403-442` WEAK. `show` only gets the first event, so the stderr branch never runs. Run every event and assert both refs equal `Show.transcript`.
- [x] W9 `CliLawSpec:32-53` MERGE. Constants rerun inside `check`, and the split repeats L22-30. One property.
- [x] W10 `ApiClockLawSpec:128` HOST. Real `git` (S6).
- [x] W11 `ReportLawSpec:213-251` WEAK. "Only four failures name a cell" has no coverage gate. Drive it from `CodecSamples.graphErrors`.
- [x] W12 `FrameLawSpec:23` REWRITE. `alphaNumericChar` makes chars equal bytes. Generate with `Gen.string`.
- [x] W13 `OpaqueLawSpec:54-70` REWRITE. "Rejects anything else" rests on three fixed strings rerun 100 times. Generate wrong lengths and non-hex, move fixed rows out of `check`.
- [x] W14 `JsonLaw.textLaw` is never called. Delete. `wire/test/.bsp/scala-cli.json` is tracked. Delete.
- [x] W15 `CliExit` has twelve cases and the call-dependent helpers `resolveExit`, `selectExit`, `compileExit`, `previewExit`, `ioExit`, and `failedCall`, but only `.code` leaves the process (`histos.cli.Main`, the native client) and `Watching.restarts` reads only `== Success`. Collapse it to `Success`, `Failed`, and `Usage`, and drop the call from `CliExit.from`.
  - The call that only fed `CliExit.from` is gone from `Watching.restarts`, `Cli.held`, `Cli.completed`, `Cli.recorded`, `Cli.pinned`, `Cli.launcherFailed`, the preview plan helpers, and the client's `frames` and `finish`.
- [x] W16 `WireJson`'s `given JsonCodec[NonEmptyChunk[A]]` is never used. A derived codec summons a `JsonDecoder` and a `JsonEncoder` per field, and zio-json's own `nonEmptyChunk` instances win. Delete it.
  - A scala-cli miniature with the given imported decoded `{"Cells":{"errors":[]}}` through a derived codec as zio-json's `Chunk was empty`. Only a direct summon in `WireSchemaLawSpec` reached it. The `BuildFault` golden now pins a `Cells` document on JVM and Native.
- [x] W17 `RunId` and `ClangTarget` have no literal, so wire laws build them with `trusted` or a `from` fallback. Add the literal.
- [x] W18 `MessageLawSpec.fixText` has `None` branches for a `::`. Delete them.

## central

- [x] C1 `PortalClientLawSpec:28, :49, :69, :131, :147, :163`, `PreflightReadLawSpec:60` WEAK. The `Left` arm accepts the parser's only failure, so a parser regression skips the law. Parse inside the effect with `ZIO.fromEither`.
- [x] C2 `PublishIdentityLawSpec:144` MERGE. The same 11 `TransportFaults.from` rows as `PortalClientLawSpec:199`. Keep one.
- [x] C3 `PublishIdentityLawSpec:163` DELETE. `MessageLawSpec:796` pins it.
- [x] C4 `LayoutLawSpec:166` WEAK. The "quote" fixture `a&b` has no quote. Use `a"&b` and assert the `<groupId>` element.
- [x] C5 `PortalReportLawSpec:42` REWRITE. The title says an empty zip is a layout finding, and the assert says no finding (D3).
- [x] C6 `PortalReportLawSpec:89` WEAK. The `.sha256` fixture is a wrong digest (D4).
- [x] C7 `PortalFlowLawSpec:10` REWRITE. Copies `beat`, and the 30th read is never pinned. Assert `beat(29, Same) == (Say, 0)` literally.
- [x] C8 `PortalClientLawSpec:88, :103`, `PreflightReadLawSpec:36, :59`, `PortalStoreLawSpec:13` WEAK. Rows reduce to `Boolean` and `forall(identity)`, which names no row. Collect mismatches and assert `misses == Nil`.
- [x] C9 `case Left(_) => assertTrue(false)` in `LayoutLawSpec`, `PortalFlowLawSpec:85, :98, :101`, `PortalReportLawSpec:30, :195`, and the `"unused"` placeholders in `LocalPublishLawSpec:40, :55`, drop the error. Use `ZIO.fromEither` steps.
- [x] C10 B5.
- [x] C11 `DeploymentId` has no compile-time literal, so laws parse constant ids with `ZIO.fromEither(DeploymentId.from(...))`. `DeploymentId("...")` is a macro literal over `from`, which keeps `DeploymentId.Error`.
  - Proof: `PublishIdentityLawSpec` "a deployment id literal is checked when it compiles" pins the compile errors for a spaced id, a blank id, and a non-literal. The central laws use the literal for every constant id. The engine laws that still parse a constant (`ReleaseReportLawSpec:13`, `PublishRunLawSpec:192`, `ReleaseRunLawSpec:259`) belong to engine M-Z.
- [x] C12 `Gav.from` and `Finding.from` return `Option`, and `PortalStore` turns any bad record into `Io(dir)`. `GavError` names a line that is not three fields or has no group, artifact, or version. `FindingError` names an unknown token, a kind with no detail, or a kind that takes none. `PortalStore.read` fails with `Unreadable(path, Fault)`, and the fault is the state, the report line, or the gav line. `PortalReport.parse` fails with `StatusError`. The rehearsal portal answers an unreadable deployment with "the rehearsal portal could not read the deployment" instead of the store sentence.
  - Proof: `PortalReportLawSpec` pins each `GavError` and `FindingError` with its message, and the `StatusError` for a bad state, list, and token. `PortalStoreLawSpec` pins the three faults, and `PortalServerLawSpec` pins the new sentence. A gav line with a fourth field used to put a `:` into the version, and is now `Fields`.
- [x] C13 `Finding.Layout(path: String)` names an entry path or the whole upload in one string. It is `Layout(LayoutPlace)`: `Upload`, or `Entry(EntryPath)`, where `EntryPath` is a non-empty literal type. An entry keeps the token `layout:<path>`. The upload was `layout:zip`, the same text a root entry named `zip` renders, so the store read one back as the other. The upload's token is now `layout`. A zip entry with no name is the upload's finding.
  - Proof: `PortalReportLawSpec` "a finding token is its kind and detail" pins every token's text and round trip, including `layout` and `layout:zip` as two findings. "a non-zip, an empty zip, a nameless entry" pins the upload finding.
- [x] C14 `PortalServer.accept` parses `UUID.randomUUID.toString` with `DeploymentId.from` and answers a `Left` that cannot happen with a 500. `DeploymentId.of(uuid)` is total, and `accept` takes the UUID from `Random.nextUUID`, so the arm and its sentence are gone.
  - Proof: the hold, automatic, not-a-zip, and empty-bundle laws each create deployments through `accept`. `PublishIdentityLawSpec` pins the text `of` gives a UUID.
- [x] C15 `PortalServerLawSpec` creates its roots in the system temp directory, and `deployment(...)` drops the parse error with `.toOption`. The suite takes `LawDir`, `deployment` fails with `PortalError` or `DeploymentId.Error`, `report` keeps the `StatusError`, and the `staged` helper's `Any` error is the union it raises.
  - Proof: `./histos test central` (84 passed).
- [x] C16 `LocalPublishLawSpec.sidecar` reads files inside `map` and guards `getFileName` against null. The laws read through `ZIO.attemptBlocking` with a typed `Unread(path)`, name each sidecar by its literal path, and compare the written bytes instead of asking whether a file exists.
  - Proof: `./histos test central` (84 passed).
- [x] C17 `PortalReport.artifactFile` takes `zip: Boolean`, and all three calls pass `true`. The parameter is gone, so a jar entry that is missing or is not a zip is one file-set finding. `pomFile` also returned a gav that no caller read, and that is gone too.
  - Proof: `PortalReportLawSpec` "a missing or non-zip jar is a file set" pins a javadoc jar that is not a zip, which no law covered before.
- [x] C18 `FileSet`, `Checksum`, `PomFields`, `SnapshotVersion`, `SnapshotDependency`, and `AlreadyPublished` carry strings. `FileSet` and `Checksum` carry an `EntryPath`, built with the total `child` and `suffixed`. `PomFields` carries a `PomField` enum. `SnapshotVersion` carries a `SnapshotRevision`, which now has a `from` and a literal, and `SnapshotMetadata.parse` builds on that `from`. `SnapshotDependency` and `AlreadyPublished` carry a `Gav`. Every token keeps its text. The only new text is `pom-fields:parent` and `pom-fields:dependency`, for a snapshot parent or dependency with a `:` inside a part. That coordinate used to render a token `Gav.from` could not read back. `FindingError` names a field, a version, or a coordinate that does not parse.
  - Proof: `PortalReportLawSpec` "a finding token is its kind and detail" pins every token's text and round trip, with a coverage check over all ten `PomField` values, and each new decode error with its message. "a snapshot parent or dependency is named by its gav" pins the two new field findings and leaves a released coordinate alone.
- [x] C19 `Fingerprint` has no literal. `Fingerprint("...")` is a macro literal over `from`, and it splices the normalized key. `PortalFlowLawSpec` and `PreflightReadLawSpec` use it, and the keyserver law pins the upper-case key in the request URL. The engine laws that still parse a constant (`SignerLawSpec:49, :145`, `ReleaseRunLawSpec:228`) belong to engine M-Z.
  - Proof: `PortalFlowLawSpec` "a fingerprint is 16, 40, or 64 hex digits" pins the normalized literal and the compile errors for a bad key and a non-literal.
- [x] C20 `PortalStore.Error.Io` covers reads and writes, and the rehearsal portal answers both with "could not store the deployment". It is `Read(path)` or `Write(path)`. An id or a gav that would leave the store fails on the side of its call. The portal answers a read with "the rehearsal portal could not read its store", and a write keeps "could not store the deployment". A status for `..` now says the portal could not read.
  - Proof: `PortalStoreLawSpec` pins a state file that cannot be read as `Read(state)`, a folder that cannot be made as `Write(state)`, and a bad id as `Write` on create and `Read` on read. `PortalServerLawSpec` pins both sentences over HTTP.

## engine A-L

- [x] E1 `ApiJarLawSpec:19` DELETE. `ApiJar.locate` has no caller. Delete `ApiJar`.
- [x] E2 `ArchiveLawSpec:13` DELETE. `RuntimeCache.prepare` has no caller. Delete it.
- [x] E3 `AssetLawSpec:45` WEAK. `isRight` and `isLeft` only. Assert `bases(Map.empty) == Right(Bases(...))`, each override, and the `Left`.
- [x] E4 `AssetsLawSpec:12` MERGE. The same refusals as `AssetLawSpec:24`. One `check` over generated parts plus the loopback cases.
- [x] E5 `AssetSyncLawSpec:58` WEAK. Matches message text. Assert `Left(AssetError.Collides(...))`.
- [x] E6 `CacheLawSpec:117, :139` DELETE. `StoreScope` has no caller, and `:139` reads `published` before the commit can run. Delete `StoreScope`.
- [x] E7 `CacheLawSpec:157` DELETE. `histos.cache.Scratch` has no caller. Delete it.
- [x] E8 `ChannelLawSpec:8` DELETE. `ChannelRefresh` has no caller, and the shim has its own TTL in shell. Delete it.
- [x] E9 `CiBranchLawSpec:13` HOST. Reads this checkout, can run `git remote set-head` and `git ls-remote`. Build a temp repo with a local bare `origin`.
- [x] E10 `ClangLawSpec:15` HOST. Needs clang on the host PATH. Prove `resolve` on the scripted `pair` directory only.
- [x] E11 `ClassShapeLawSpec:14` HOST. Copies an earlier build's `.histos/classes/api/3.10.0/...`. Emit the class with ASM.
- [x] E12 `CleanLawSpec:32` WEAK. Plants files only under two roots and repeats the root list. Plant under every root and assert each is gone.
- [x] E13 `CoverageLawSpec:263` DELETE. A copy of `CompileLawSpec:43`. Merge `CompileLawSpec` into `CompileIdentityLawSpec`.
- [x] E14 `DoctorProbeLawSpec:228` MERGE. Repeats `DoctorRenderLawSpec:98, :108`. Delete.
- [x] E15 `DoctorProbeLawSpec:351` MERGE. Keep only the `Registry.collect` ordering assert.
- [x] E16 `AssembleLawSpec:66`, `CoverageOrderLawSpec:41`, `DocLawSpec:39, :84` HOST. S1.
- [x] E17 `FilePublishLawSpec:28` HOST. S2. Also L81-82 cannot fail, and L126-135 copies Coursier's ivy-home lookup. Fold the overwrite case into `IvyLocalLawSpec`.
- [x] E18 `FormatterLawSpec` (6 laws) HOST. S7.
- [x] E19 `GitHubCredentialLawSpec:84` DELETE. Runs the real `gh auth token`. L28 proves `resolve` with an injected `gh`.
- [x] E20 `GraphCellLawSpec:34`, `GraphTastyLawSpec:36` HOST. S1.
- [x] E21 `GrpcCacheLawSpec:34`, `GrpcChannelLawSpec:27` FLAKY. Bind port 0, close, and hope, with no deadline. One loopback gRPC server for the law that answers `UNAVAILABLE` and `NOT_FOUND`, and a channel deadline.
  - `GrpcLoopback` serves on 127.0.0.1 for the law's scope, as a store or as one fixed status. The bound is each suite's `TestAspect.timeout` on the live clock. `GrpcChannel` still sets no call deadline: picking one for a large blob write is a product choice for Russ.
- [x] E22 `GrpcChannelLawSpec:12` DELETE. Restates the method descriptors. Replace with a loopback `store` then `find` through `GrpcCache`.
- [x] E23 `IvyLocalLawSpec:20` HOST. S2. It also hard-codes `user.home/.ivy2/local`, which ignores `ivy.home`.
- [x] E24 `JdkLawSpec:37` WEAK. L45-55 restate constants. Keep the Os rows.
- [x] E25 `JobCondLawSpec:33` REWRITE. Pins B1. Assert `!(github.event_name == 'push')`, and the pages check gets its own law.
- [x] E26 `JobYamlLawSpec:56` REWRITE. Pins B2, and the 14-row table restates the match.
- [x] E27 `JobYamlLawSpec:90` WEAK. Restates `uses`. Assert the literal `name@sha # tag` line.
- [x] E28 `JvmCompileLawSpec:28` DELETE. Unreachable input: `ScalaJs.compile` refuses it first.
- [ ] E29 `LauncherScriptLawSpec:33` REWRITE. Pins `-ge 21` against the floor of 25. Owner: CI setup item (row 29), which renders the floor from `Toolchain.Floor`.
- [x] E30 `LoaderPluginLawSpec:26` WEAK. Any diagnostic passes. Assert the unresolved `law` reference.
- [x] E31 `LoaderProjectLawSpec:48` DELETE. Passes when the loader ignores the wiring file.
- [x] E32 `LoaderProjectLawSpec:56` WEAK. Assert the diagnostic points at the wiring source. The diagnostic had no source (B13).
- [x] E33 `LoaderPublishLawSpec:38`, `LocalStageLawSpec` (6 laws) HOST. S1.
- [x] E34 `LockLawSpec:22` REWRITE. One example. `check(genLock)` with plugin rows and evictions, `parse(render(lock)) == Right(lock)`.

## engine M-Z

- [x] E35 `PublishLawSpec:282, :294, :319` WEAK. The fallback arm accepts `NothingToPublish`. Assert the one expected `Left`.
- [x] E36 `PublishInstallLawSpec:26` HOST. S2. The ivy root was already the law's own (S2); the compile now runs on `CompilerClosure`.
- [x] E37 `ProgramLawSpec:37` WEAK. No positive case, and the `$$anon` file is garbage bytes. Write real classes with ASM: a `Main` that is found and a `Foo$$anon` that is not.
  - ASM writes `demo/Main` and `demo/Main$` with a public static main, `demo/Foo$$anon` with one, a private `main`, a class with none, and garbage bytes. Only `demo.Main` is found, once.
- [x] E38 `PluginAspectLawSpec:66` WEAK. Only checks a value passes through. Read the context loader inside `ZIO.succeed` and `ZIO.attemptBlocking` and assert `eq` to a fresh loader.
- [x] E39 `NodeChooseLawSpec:18` HOST. Reads the host PATH under `withLiveSystem`, and runs a 2 s timeout on an unadjusted clock. Use `goodHome/bin` as `PATH`.
  - `PATH` is `goodHome/bin`, and the chosen node is that binary at 26.0.0. `withLiveSystem` stays because the `node --version` child is launched with the running JVM's `java.home`. The suite runs on the live clock, because a PATH sight bounds that real child with a timeout.
- [x] E40 `ModuleMissLawSpec:35` WEAK. Assert the test class exists after the run.
  - The row was wrong about the product: a test main is its module's one suite, so a pattern that does not name it is a miss before the tests compile. The fixture also wrote its probe outside the module's sources. The law now asserts the miss, the main classes, and no test classes, and that `Probe` itself runs, passes, and compiles `Probe.class`.
- [x] E41 `NativeRequestLawSpec:52` WEAK. `forall` on lists that may be empty, and `.head`. Assert the names, and `headOption.contains`.
- [x] E42 `PublishRecordLawSpec:71` WEAK. `contains("recordlaw")`. Assert `List("recordlaw_3")` and the revision.
- [x] E43 `ModuleTestLawSpec:36`, `ProgramRunLawSpec:37`, `NativeLawSpec:116`, `ProcessLawSpec:14`, `PublishLawSpec:124, :205` DELETE. Subsets of another law, or restatements. `BundleLawSpec` gained the `..` path the deleted bundle-path law had.
- [x] E44 `PluginRunLawSpec:42` MERGE into `PluginOutcomeLawSpec:104`.
- [x] E45 `PreviewLawSpec:112` MERGE into `PlanShowLawSpec:17` with an `idle` module.
- [x] E46 `PublishLawSpec:365` MERGE. Move `summed` and `sized` into `BundleLawSpec`.
- [x] E47 `PublishReleaseLawSpec:47` MERGE. Move `promote` next to `ReleaseRunLawSpec:206`.
- [x] E48 `ProviderInstallLawSpec:46` REWRITE. `libs.head`. `libraries` returns a `NonEmptyChunk`.
- [x] E49 `ProvisionLawSpec:34` MERGE. Keep the `made.out` asserts.
- [x] E50 `ProvisionLawSpec:129` REWRITE. `withLiveClock` does nothing and its comment is false. Remove both.
- [x] E51 S8 lines in `PackLawSpec`, `PreviewLawSpec`, `PublishLawSpec`, `ScaffoldLawSpec`, `SignerLawSpec`, and `TaskLawSpec:62`.
- [x] E52 `SupplyDecisionLawSpec:135` REWRITE. Pins B3.
- [x] E53 `SurfaceExchangeLawSpec:78, :121, :138`, `SupplySurfaceLawSpec:124` HOST. S5. The audit law also wrote the developer's `~/.cache/histos/advisories`; it now sets `user.home` under `TestSystem` and asserts the cached body.
- [x] E54 `SurveyLawSpec:69, :93` HOST. `withLiveSystem` where `Machine.load` already reads `zio.System`. Use `TestSystem`, and drop the JDK assert. The laws build their `Machine` with no PATH and no java home, so nothing reads the host.
- [x] E55 `ZincCompileLawSpec` (4 laws) HOST. S1. With it, `ModuleMissLawSpec`, `ModuleReportLawSpec`, `ModuleTestLawSpec`, `PluginCompileCellLawSpec`, `ProgramRunLawSpec`, `ProviderInstallLawSpec`, `ProviderLawSpec`, `PublishInstallLawSpec`, `PublishRecordLawSpec`, `PublishReleaseLawSpec`, and `PublishSnapshotLawSpec` run on `CompilerClosure` at its locked Scala version, and their checkout walks are deleted.
- [x] E56 `ScheduleLawSpec:394, :412` FLAKY. `observe` yields 32 times and reads. Wait on the terminal-event Promise, as `watch` does.
  - `observe` waits for the event that ends the goal's own step, which is the last event the run emits. The `exclude` helper still yields 32 times before it reads the peak. That is E81.
- [x] E57 `ScheduleLawSpec:227` WEAK. Renders only `url.absolute`. Assert the url renders back to the input. A bare host renders with its root path, `http://cache.example/`.
- [x] E58 `ScheduleLawSpec:131` (and L619) DELETE. Enum declaration order.
- [x] E59 `ScheduleLawSpec:46` MERGE into L41 over generated `Need` subsets.
- [x] E60 `ScheduleLawSpec:355, :362` MERGE. One law over both `Promote` values.
- [x] E61 `ScheduleLawSpec:689` DELETE. L660 and L676 cover it.
- [x] E62 `SuiteLawSpec:268` DELETE. `TestCompile.distinct` is `List.distinct`. Replace the helper.
- [x] E63 `SuiteLawSpec:289` REWRITE. Assert `analysisDirOf(testClasses(...))` is the sibling `test-analysis`.
- [x] E64 `SuiteLawSpec:76, :95`, `SuiteFilterLawSpec:42, :52, :91` MERGE into `SuiteNamesLawSpec` and `SuiteLawSpec`'s unique cases.
  - `SuiteNamesLawSpec` took the transitive reach, the missing parent, sjsir beside a class, and the ZIO framework's built-in mark. `SuiteLawSpec` took the finished cell, the link failure, `classBinaries`, the `.class` name, and the file that stays open, and its patterns are `SuitePattern` literals.
- [x] E65 `ResolutionServiceLawSpec:226` REWRITE. Assert no `dependencies.lock` exists after a solve, in L105.
- [x] E66 `ReleaseRunLawSpec:168` REWRITE. Pins B5. The law already pinned `DeploymentId.Error.Spaced` after B5. The local is `spaced` (E78).
- [x] E67 `ToolchainLawSpec:13` REWRITE. Pins B4.
- [x] E68 `WorkflowLawSpec:214` MERGE. Repeats `WorkflowFileLawSpec:71`.
- [x] E69 `WorkflowLawSpec:293` REWRITE. B2. Deleted: `JobYamlLawSpec` pins B2 through `JobYaml.render`, and E80 removed `lift`, the only thing the law called.
- [x] E70 `WorkflowLawSpec:338` WEAK. No overlapping keys, so a shortest-prefix bug passes. Add an overlapping pin.
- [x] E71 `SignerMessageLawSpec:8` DELETE. Restates `SignerError.message` without coverage.
- [x] E72 `SonaPublishLawSpec:31, :46` HOST. S4.
- [x] E73 `ReapiLawSpec:13` REWRITE. Hand-picked ASCII values. `check` over long and non-ASCII strings, digests, and payloads.
  - Two `check` laws: every request and response round-trips for a Unicode instance up to 300 characters, any digest, size, and payload, and frames of any length up to three chunks join back to the payload with the resource once and the offsets counted. The service names and the omitted empty fields stay as fixed rows.
- [x] E74 `Sha256DigestLawSpec:8` REWRITE. Tests the JDK. Keep the parse round trip.
- [x] E75 `.histos/doc` and `.histos/coverage` are not product roots, so `clean` leaves them, although the README says clean wipes every product root. Owner: engine M-Z (`CleanLawSpec`).
  - `doc`, `coverage/classes`, and `coverage/analysis` are module roots, and `coverage`, which holds the report for every module, is a shared root. Red: the full wipe left seven planted files and the module wipe left three. Green: both laws pass, and the README names the scaladoc and coverage output.
- [x] E76 `Workflow.check` uses `extra.head` behind a `nonEmpty` guard. It is `foreachDiscard(extra.headOption)`, and `WorkflowFileLawSpec` still pins the stale-file drift.
- [x] E77 A `histos.scala` compile failure (`BuildFile.compile`) is message-only in `LoadError.CompileFailed`. Owner: engine M-Z (`LoaderProjectLawSpec`).
  - `BuildFile.compile` is gone, and the loader compiles the build file through `compileLabeled` with the label `histos.scala`, the same path B13 gave the wiring. Red: `Found: ("no" : String) Required: Int`, unplaced. Green: `histos.scala:5:…: error: Found: …`, and the temp file's name is not in the text.
- [x] E78 `ReleaseReportLawSpec:13` uses `case Left(_) => assertTrue(false)`, and `ReleaseReportLawSpec:13`, `PublishRunLawSpec:192`, and `ReleaseRunLawSpec:259` parse a constant deployment id. Each uses the `DeploymentId("…")` literal, and `ReleaseRunLawSpec` parses its portal parts with `ZIO.fromEither` steps.
- [x] E79 The `SuitePattern.from(...) match` fallbacks in `ModuleMissLawSpec:44` and `SuiteFilterLawSpec:53` are `SuitePattern("…")` literals. The `SuiteFilterLawSpec` law moved into `SuiteLawSpec` (E64).
- [x] E80 `CiError` had 13 cases that only mirrored `CiFault`. They are one case, `CiError.Fault(fault: CiFault)`, whose sentence is the fault's, and `JobYaml.lift` is gone. `DuplicateSite` stays: it comes from `PluginGraphFault`.
  - The wire shape changed, and the native client still links. `CodecSamples` carries one `Fault` sample, `GoldenLawSpec` pins `{"Files":{"error":{"Fault":{"fault":{"UnknownNeed":{"job":"docs","need":"release"}}}}}}` on JVM and Native, `MessageLawSpec` and `WireSchemaLawSpec` name the 14 cases, and `docs/ci-job-trait.md` says the wire carries the fault.
- [x] E81 FLAKY. `ScheduleLawSpec`'s `exclude` helper (the three "one permit per need" exclusion laws) yields 32 times after the first step enters, then reads the peak. A broken permit can let the second step in after that read, so the law can pass on the bug. The second step's `Started` event is offered before it takes the permit, so no event marks it blocked. The deterministic form is `TestClock.adjust` with `TestAspect.fibers`, so the adjust waits until every Histos fiber is suspended; prove that in a miniature spec before the law moves onto it. Owner: engine M-Z follow-up.
  - Miniature (scala-cli, zio-test 2.1.27): the first holder waits a second on a `Promise` the second holder completes, and the law adjusts the clock a second. Every `ZIOSpecDefault` already runs under `TestAspect.fibers`, so the adjust returns only once every fiber is suspended or done. That shape passed a `Semaphore` permit, also under `nonFlaky(500)`, and was red on a permit that admits both at once, one that admits the second after 200 yields, and one after 200,000 yields. The 32-yield shape was red on the first two and passed the third. `exclude` is that shape: the steps never meet and both finish. Red: with `Histos` taking no permit, the three exclusion laws failed (35 passed, 3 failed). Green: 38 passed.
- [x] E82 B16 `NodeVersion.from` matched only `major.minor.patch`, so `v25.0.0-rc.1` was `UnreadableVersion` before `VersionOrder` saw it (B4), and a Node release candidate on `HISTOS_NODE` was refused as unreadable. Owner: engine M-Z (`NodeLawSpec`, `NodeChooseLawSpec`), and wire (`OpaqueLawSpec`).
  - Red: `from("v25.0.0-rc.1")` and `from("v26.0.0-nightly20260101abc+7")` were `Bad`, and `choose` died with `Rejected(…/candidate/bin/node, UnreadableVersion(Bad(v26.0.0-rc.1)))`. Green: a version takes an optional `-` pre-release and `+` build. `HISTOS_NODE` at `v26.0.0-rc.1` is chosen, `v25.0.0-rc.1` is `TooOld` against the floor of 25, and on a PATH with both `26.0.0-rc.1` and `26.0.0` the shadow is the release. `25.0.0-` and `25.0.0 rc` stay bad, and the wire decodes `"v25.0.0-rc.1"`.
- [x] E83 `Cli.deps` read `GH_TOKEN` and `GITHUB_TOKEN` through `sys.env` and passed the token into `DepsSurface.pullRequest`, `alert`, and `snapshot`. Each reads it through `zio.System` with `GitHubCredential.fromEnv` for github.com, the order doctor uses, and the parameter is gone. Owner: engine M-Z (`SurfaceExchangeLawSpec`, `SupplySurfaceLawSpec`).
  - The laws put the token with `TestSystem.putEnv`. A new law asserts the bearer the stub receives: an empty `GH_TOKEN` and no `GITHUB_TOKEN` is `GITHUB_TOKEN is not set`, `GITHUB_TOKEN=actions` sends `Bearer actions`, and `GH_TOKEN=person` then wins. Red: with the read on the host's `sys.env`, the token law and every live pull request, alert, and snapshot law failed. Green: 28 passed with E84 and E85.
- [x] E84 `Cli.deps` read today from `LocalDate.now`, so no law could set the day a cooldown counts to. `DepsSurface.check` and `update` read it from `zio.Clock` in UTC, and the `today` parameter is gone. Owner: engine M-Z (`SurfaceExchangeLawSpec`).
  - The release Maven names is from 2023-11-14 and the cooldown is three days. Under `TestClock.setTime`, 2023-11-16T23:00Z holds it (`deps plan` with the note only) and 2023-11-17T00:00Z applies it. Red: on the wall clock both days applied it.
- [x] E85 `EntryPoints.hasMain` set a `var` from an ASM visitor that returned `null`. It reads the class into an asm-tree `ClassNode` and asks whether any method is a public static `main(String[])`, and an unreadable class has none. Owner: engine M-Z (`ProgramLawSpec`, E37).
  - E37's law gained a public instance `main`. Red: with the static bit not checked, `demo.Instance` was found beside `demo.Main`. Green: only `demo.Main`.
- [x] E86 `PublishRunLawSpec`'s `blow(error: Any)` turned a `BuildError` or a `VersionError` into a defect. It is gone: `load` is `IO[BuildError, ProjectGraph]` and `jdk` is `IO[VersionError, JdkRuntime]`, and each law carries that error. Green: 5 passed.
- [x] E87 `ScenarioSpec`'s program-output law compiled a child that polled for a gate file with `Thread.sleep(20)`. The law listens on a loopback port, and the child prints `ready`, then blocks on a read from that port. The law answers once it sees `ready`, and the child prints `done`. Owner: engine M-Z (`ScenarioSpec`).
  - A `WatchService` was not the answer: on macOS the JDK's is `sun.nio.fs.PollingWatchService`. Red: with the read before `ready`, the law never answered and the run timed out to `None` after a minute, and no child was left running. Green: 4 passed in 15 s.
- [x] E88 `ShipLink`'s scaladoc linked `[[Catalog.from]]`, which scaladoc cannot resolve from `histos`, so `./histos run doc` warned at `ShipDef.scala:50`, and the CI scaladoc job fails on a warning. The link is `[[histos.version.Catalog.from]]`. Owner: api. The api layer on main (74aa1d7) carries the fix, so the engine M-Z commits do not touch `ShipDef.scala`.
  - Red: `./histos run doc` on the engine M-Z tree before that rebase printed `Couldn't resolve a member for the given link query: Catalog.from`. Green: no warning.
- [x] E89 `DepsSurface` read `dependencies.lock` and `histos.scala` with `Files.readString` outside an effect (`lock`, `read`), `check` and `update` kept a `known` parameter that no caller passed, and `Date.from` (api) parsed with `try`. Closed by E91, E92, and E93.
- [x] E90 `DoctorSurface.run` provided `ReleaseFeed.live` and `GitRead.live` inline, the S3 shape. `run` is `URIO[ReleaseFeed & GitRead, (Int, String)]`, and so is the doctor `Provision` runs after a full `histos new` or `init`, which also read the live feed inline. The edges provide the live layers: `Cli.doctor`, `Cli.fresh`, and the MCP doctor tool. E96 added `Terminal`, and the three are `DoctorSurface.Needs`. Owner: engine M-Z (`DoctorSurfaceLawSpec`, `ProvisionLawSpec`).
  - A new law runs `DoctorSurface.run` on a checkout pinned at `0.2.0` with a ship, a token under `TestSystem`, and a feed that dies if read. The preflight finishes and names neither `ship.pin` nor `ship.should-bump`, and with `newest` stubbed to `9.9.9` the full run names both and `9.9.9`. Red: with preflight keeping the registry checks, the run died with `NoStub` for `newest` from `ShipChecks.listing`, so `run` reads the feed its environment provides. `ProvisionLawSpec` provides a feed and a git reader that die if read. Green: 3 doctor surface laws and 3 provision laws.
- [x] E91 `DepsSurface.lock` and `read` were `Files.readString` outside an effect, so a lock that was not text threw out of `check` itself. Both are `Workspace.readIfPresent` with its `IoFailure`. A private `Stop` (`NoLock`, `Unread`, `Fault`) carries the reason a command ends early, and each command prints it as its result. `alert` still treats a missing lock as no packages, and an unreadable one is the read failure. Owner: engine M-Z (`SupplySurfaceLawSpec`).
  - A new law writes bytes that are not UTF-8. Red: `check` threw `MalformedInputException` from `DepsSurface.lock`. Green: `check`, `update`, `audit`, `alert`, and `snapshot` each return `could not read …/dependencies.lock`, a build file that is not text is `could not read …/histos.scala`, and nothing is sent.
- [x] E92 `DepsSurface.check` and `update` defaulted a `known` map that no caller passed. The parameter is gone, and `update` and `check` share one `propose`. Green: the deps laws compile and pass unchanged. E95 then removed the map from `Plan.propose`.
- [x] E93 `Date.from` parsed with `LocalDate.parse` inside `try`. It matches `yyyy-mm-dd`, checks the month and the day of that month with the Gregorian leap rule, and fails with `DateError.Bad`. The api no longer needs `java.time` for a date. Owner: api (`SupplyNameLawSpec`).
  - The law pins `2024-02-29` and `2000-02-29` as days, a literal `Date("2026-12-31")`, and refuses thirteen texts, among them `2023-02-29`, `1900-02-29`, `2026-04-31`, `+12026-01-10`, a timestamp, Arabic-Indic digits, and the empty text. `Date("2023-02-29")` does not compile. Red, on JVM and Native: `+12026-01-10` was a date. Green on both.
- [x] E94 `SignerLawSpec` (twice) and `ReleaseRunLawSpec` parsed a constant fingerprint with `ZIO.fromEither(Fingerprint.from(…))`. Each uses the `Fingerprint("0123456789abcdef")` literal. Green: 6 signer laws and 5 release run laws.
- [x] E95 Decision for Russ. `Plan.propose` takes `knownRepos` to refuse a pinned action whose repository id moved (`ActionRepoMoved`), and nothing records that id, so the map is always empty and the refusal cannot fire. Either the action pins record the repository id, or `knownRepos` and that branch of `fetchAction` are deleted. Owner: engine M-Z.
  - Russ: delete it. Nothing records a repository id, so the refusal can never fire, and a promise the product does not keep is worse than none.
  - Gone with it: the `GET /repos/{owner}` read that only fed the id, `Plan.repoId`, and `Bump.verify`. `fetchAction` was the one caller of `verify` and passed `reachable = true`, so `SupplyFault.TagNotReachable` could not fire either. `ActionRepoMoved`, `TagNotReachable`, `RepoId`, `RepoIdError`, and their laws are deleted. A new law pins what the action plan does: a pinned `v1.0.0` moves to the cooled `v1.1.0`, and GitHub is asked for the tags and each tag's commit only. Red: the old plan sent `https://api.github.com/repos/example/demo`. Green.
- [x] E96 `DoctorSurface.readFile` read `dependabot.yml` and the files a fix edits with `Files.readString` outside an effect, `DepsSurface.changed` died on a changed file it could not read, and `DoctorSurface.run` read `System.console()` directly. Owner: engine M-Z (`DoctorVenueLawSpec`, `SupplySurfaceLawSpec`, `DoctorSurfaceLawSpec`), and api (`SupplyDiagnosticLawSpec`).
  - `readFile` is `Workspace.readIfPresent`, which is `ZIO.attemptBlocking` with `IoFailure`. A `dependabot.yml` that is not text is `SupplyDiagnostic.DependabotUnread`, a `supply.dependabot-overlap` warning that names the file, and the overlap is not checked. A local edit whose file is not text is `not applied: could not read <path>`, its bytes stay, and the other edits land. Red: both threw `MalformedInputException` out of the doctor. Green.
  - `changed` reads each file with `Workspace.readIfPresent`, and a failure is `Stop.Unread`. Red: `deps pr` died with `MalformedInputException` from `changed`. Green: `could not read …/histos.scala`.
  - `run` already read the environment through `zio.System.envs`. The console is the `Terminal` service (`live` asks `System.console()`, and `detached` answers no for the MCP tool, whose standard streams are the protocol), part of `DoctorSurface.Needs`. Only a `--fix` run asks it. A law runs `--fix` on an unpinned workflow: a detached terminal is `not applied (no terminal)`, an attached one prompts on `TestConsole`, and `n` is `skipped` and `y` is `applied`. Red: with `run` reading `System.console()`, the test JVM has none, so nothing prompted. The no-fix laws provide a terminal that dies if read.
- [x] E97 `TimeZone.from` (api) parsed with `try` around `ZoneId.of`, which also took offsets (`+01:00`, `Z`, `UTC+01:00`, `GMT+2`) that are not IANA zones. It is a lookup in `ZoneId.getAvailableZoneIds`, with `TimeZoneError.Unknown`. Owner: api (`SupplyNameLawSpec`).
  - The law keeps `America/Chicago`, `Europe/Berlin`, `Etc/UTC`, and `UTC`, refuses the four offsets, `Mars/Base`, `america/chicago`, ` UTC`, and the empty text, and `TimeZone("+01:00")` does not compile. Red on JVM and Native: the four offsets were zones, and the literal compiled. Green on both.
- [ ] E98 `HistosMcp.launched` returns `Either[String, List[Module]]`, flattening `ProjectNameError` and the launch fault to text before the tool prints them. Owner: app.
- [x] E99 `LoadedBuild` had a test-only `apply(graph, plugins)` that filled in `HistosBuild.fixed(graph)`, and an `apply(graph, plugins, build)` that repeated the case class's own. The companion is gone, and the four app laws that used the short form name `HistosBuild.fixed(graph)`. Green: the app suite.
- [ ] E100 `DoctorSurface.applyReported` writes a local edit inside `ZIO.attempt(…).orElseSucceed(Nil)`, so a failed write prints nothing. A law needs a file it can read and cannot write, which is not deterministic under root. Owner: engine M-Z follow-up.
- [ ] E101 `DoctorSurface.run` still wires `Net.live`, `gh auth token`, and the `sh -c` that runs a `--fix` command inline, the S3 shape. The E96 law chose an App author so no fix runs a shell. Owner: engine M-Z follow-up.
- [ ] E102 `Cli.kindOf` and `Cli.askBranch` read `System.console()` directly. `Terminal` (E96) is the seam. Owner: app.

## app

- [x] P1 Checkout fixture. `BspRemainLawSpec:37`, `BspSuiteLawSpec:38`, `BspTargetLawSpec:42, :98`, `CallsAssemblyLawSpec:39`, `CallsCompileLawSpec:38`, `CallsRunLawSpec:38`, `CallsTaskLawSpec:38`, `CallsTestLawSpec:38`, `DaemonOpenLawSpec:38, :112, :135`, and `DaemonMoreLawSpec:38` each copy the `opened` and `repository` block: this checkout's lock, the developer's Coursier cache, `Machine.load` under `withLiveSystem`, and a fixed `/tmp/hd-store`. Laws that compile use one shared fixture on the S1 shape. Laws that never compile (`BspRemainLawSpec`, `CallsTaskLawSpec`, and the refusals in `CallsAssemblyLawSpec`, `CallsRunLawSpec`, `CallsPublishLawSpec`) use stubs and a fixture JDK.
  - `app/test/CompilerClosure.scala` is the S1 shape for app (engine's test sources are not on app's classpath): the lock, the newest Scala 3 compiler, `pinning(modules*)`, and a `Missing` precondition that names `./histos resolve`. `app/test/LawMachine.scala` is the machine: the test JVM's own JDK, OS, and architecture, an empty environment, and a home and store under the law's directory, as a value (`at`) and through `TestSystem` (`system`) for product code that calls `Machine.load`. No app spec copies a lock walk or runs under `withLiveSystem`. The daemon laws run under a `/tmp` directory of their own, with `HISTOS_HOME`, the store, the ivy root, and an empty offline Coursier cache inside it.
- [x] P2 `BspSuiteLawSpec:113` WEAK. `forall` over a list that is empty on a cold cache. Assert the named sources jar.
  - The session's `Resolve` answers `org.scala-lang:scala-library` sources with a jar under the law's directory, and the law asserts that one URI. The old law passed on whatever the developer's cache held.
- [x] P3 `CallsAssemblyLawSpec:39` MERGE. The jar-entry asserts repeat `AssembleLawSpec:66`.
- [x] P4 `CallsCompileLawSpec:86`, `CallsTestLawSpec:93` WEAK. `contains("missing")`. Assert `'missing' is not a project`.
  - Exact values: `Compile(Target(SelectError.Unknown(missing)))` for compile and `Select(SelectError.Unknown(missing))` for test.
- [x] P5 `CallsProductLawSpec:34` WEAK. The cleanFull leg runs on an empty store and compares the formatter to itself. Seed two projects and assert only one is named and gone.
- [x] P6 `CallsPublishLawSpec:43` HOST and WEAK. Runs `git` and copies this checkout's lock, and the receipt is never compared. Split the refusals onto a fixture JDK, and assert the receipt.
  - The refusals run on stubs and the fixture JDK. The publish leg writes `closure.pinning(...)`, commits through `LawGit`, and asserts the receipt line the build's publisher returned. A local publish reads HEAD before the organization, so both legs commit.
- [x] P7 `CallsResolveLawSpec:20` HOST. Solves against the developer's cache and hard-codes resolver facts. Seed a temp offline cache.
  - A seeded cache would have to fake the compiler's whole closure, which `resolve --update` solves. The law runs on `CompilerClosure.layer` instead, so the cache is read offline behind the named precondition, and the hard-coded eviction lines are replaced by the written lock's one cell and its compiler rows, which must equal this checkout's.
- [x] P8 `CallsSnapshotLawSpec:23` WEAK. `reported()` fields are always empty. Assert the `Out` lines, and drop the promote leg.
- [x] P9 `CliCancelLawSpec:10` DELETE. `ExecuteLawSpec:76` proves it with the exact exit.
- [x] P10 `CliCoverLawSpec:35` HOST. Retain mode downloads on a miss. Use an offline resolve, and assert the statements line.
  - The law runs `Cli.execute` on `CompilerClosure.layer` and pins `statements 0/4 0.00` and `branches 0/0 0.00`.
- [x] P11 `CliGapLawSpec:46` HOST. `git commit` honours the developer's `commit.gpgsign` and hooks. One shared git helper with `--no-gpg-sign` and `GIT_CONFIG_GLOBAL=/dev/null`.
  - `app/test/LawGit.scala`, also used by `CallsPublishLawSpec` and `CallsWorkflowLawSpec`, sets `GIT_CONFIG_GLOBAL=/dev/null` and `GIT_CONFIG_NOSYSTEM=1` and commits with `--no-gpg-sign --no-verify`.
- [x] P12 `CliLaunchLawSpec:13` HOST, MERGE. S3. Every leg has an exact-text law elsewhere. Delete, or move the setup legs into `ExecuteLawSpec`.
- [x] P13 `CliLaunchLawSpec:48` HOST, MERGE. Writes the real `~/.cache/histos`. Fold into `CliPlanLawSpec:25`.
- [x] P14 `CliLocalLawSpec:14` DELETE. Reads the real content store with asserts that fit any store. `ExecuteLawSpec:140` covers it.
- [x] P15 `CliLocalLawSpec:43` HOST, MERGE. S3. Move the pin legs into `ExecuteLawSpec.pins`.
  - The absent and pointer-pin `launcher --latest` legs are in `pins`. The `test --affected` and `~ mcp` legs are `ExecuteLawSpec` rows. `CliLocalLawSpec` and `CliLaunchLawSpec` are deleted.
- [x] P16 `CliParseLawSpec:15` REWRITE. `.head` at L107 and L111, echo rows, and forwarding rows. Pattern-match, keep the literal sentences.
- [x] P17 `CliRouteLawSpec:52` MERGE. One `CliExitLawSpec` row.
  - Deleted. Wire's `CliLawSpec` already has the program-status-2 row, and `CliExitLawSpec` is gone (P47).
- [x] P18 `ConnectionLawSpec:13` WEAK. Echoes constructor arguments, and `Connection.ensure` has no law. A law that `ensure` writes the decoded file, writes nothing without a pin, and leaves an equal file untouched.
- [x] P19 `ConnectionLawSpec:27` MERGE. `LauncherPinTextLawSpec:10` has the table. Keep one missing and one bad row.
- [x] P20 `DaemonOutboxLawSpec:9` WEAK. B6.
- [x] P21 `DaemonOpenLawSpec:112` WEAK. `waiting.isEmpty` can never fail. Relaunch completes an `entered` promise.
- [x] P22 `DaemonMoreLawSpec:38` MERGE into `DaemonOpenLawSpec` on the shared fixture.
- [x] P23 `DaemonWatchLawSpec:37` WEAK. D1.
- [x] P24 `DaemonWatchLawSpec:41` REWRITE. D2.
- [x] P25 `FmtGrammarSpec:47` WEAK. `isLeft`. Pin the exact `CliParseError`, and move the exit assert.
  - The exit of an unformatted check is wire's `CliLawSpec` row for an error line, so the assert is deleted here.
- [x] P26 `GrammarLawSpec:220` MERGE. `ExecuteLawSpec:187` pins the shim messages.
- [x] P27 `GrammarLawSpec:269` MERGE. `HelpLawSpec:30`.
- [x] P28 `McpLawSpec:97` WEAK. `isLeft`, and `Call` round trips test zio-json. Assert `Rejection.ProjectName`.
- [x] P29 `McpLawSpec:124` WEAK. Counts a literal. Build `HistosMcp.tools(...)` and assert its names equal `toolNames`.
  - The law sends `tools/list` to the built server through `Mcp.handle` and compares the listed names with `toolNames`.
- [x] P30 `McpLawSpec:154` WEAK. Decodes what the server only encodes. Assert `Level.Info.toJson` and `Level.Error.toJson`.
- [x] P31 `OperationLawSpec:16` WEAK. Either of two `Left`s passes. Rows of (command, exact `Left`).
- [x] P32 `PaintLawSpec:14` REWRITE. B8.
- [x] P33 `PerformLawSpec:42` MERGE. Derive `readers` from `needed`, add `Format` (D1).
- [x] P34 `PerformLawSpec:46` WEAK. B9.
- [x] P35 `PerformRouteLawSpec:49` MERGE. `ExecuteLawSpec:155`.
- [x] P36 `RebootLawSpec:23` FLAKY, WEAK. Real `sh`, 5 s of wall clock, and `Right(None) == Right(None)`. Require `Right(Some(_))`, and inject the spawn.
  - `Cli.bootCall(repo, spawn)` takes the spawn, and every caller passes `Daemon.spawn`. The law's spawn counts and signals, the law writes the record itself, and the wait for it runs on the test clock: the call is still waiting after one poll, then returns the recorded socket, with one spawn. A missing script spawns nothing.
- [x] P37 `ReportLawSpec:19` DELETE. `WireSchemaLawSpec:1881` has the `required` lists.
- [x] P38 `RestartLawSpec:18` FLAKY, WEAK. Real processes, unbounded polling, a live sleep, and `!gone` that cannot fail. A Promise-backed fake child under `TestClock`.
  - Each child is a fiber that offers `Started(n)` and offers `Stopped(n)` when it is interrupted, into a queue the law reads. The law waits for the watch's baseline before it writes, and `TestClock.adjust(Poll + Settle)` drives each poll. Green under `nonFlaky(30)`.
- [x] P39 `RpcLawSpec:95` WEAK. Decodes with the same codecs, and nothing pins `"jsonrpc"`. Compare to literal JSON.
- [x] P40 `SessionLawSpec:22` REWRITE. B7.
- [x] P41 `SessionLawSpec:94` DELETE. Derived round trips. Replace with WireSchema laws for `CallProblem`, `ServerError`, and `SessionError`.
  - `WireSchemaLawSpec` "a sum lists every case" now covers `ServerError`, `ServerIo`, `Session.TransportError` (named `Session.SessionError` until B16), and `Session.CallProblem` with the client's codecs.
- [x] P42 `WireSchemaLawSpec:1968-1986` REWRITE. Strip the zio-json text, keep the schema-document asserts.
  - A document of the wrong shape is `refused` with no text. A refusal a Histos parser words (`"(invalid path …)"`, `"(empty list)"`, and each text type's own message) keeps its text.
- [x] P43 A13's app half: every Grammar built-in word is in `CommandName.reserved`.
  - `GrammarLawSpec` reads the root words from `Grammar.builtIns` and asserts they are all reserved, and that only `acp`, `update`, and `self` are reserved beyond them. The literal copy of `CommandName.reserved` in `api/test/PluginSpec.scala` is gone.
- [x] P44 Killing the client does not stop the server's run. A hung suite kept running after its client was killed and blocked the next run. B17.
- [x] P45 `CallsPublishLawSpec` still installs into the real `~/.ivy2/local`, because `Calls.publishLocal` provides `IvyRoot.ivy2Local` itself.
  - `IvyRoot` is in `Perform.run`'s environment and the server's (`Daemon.Needs`), and in `Build.here`'s, the MCP session's, and `Cli.execute`'s. The command line's wiring (`Cli.withLocal`) provides `IvyRoot.ivy2Local`. Every law provides `IvyRoot.at` a path of its own.
- [x] P46 `SuitePattern.from(...) match` fallbacks in `McpLawSpec:35`, `OperationLawSpec:79`, `CallsTestLawSpec:55`, `GrammarLawSpec:331`, and `WireSchemaLawSpec:1686`. Use the `SuitePattern("…")` literal.
  - Also `RunId("…")`, `ClangTarget("…")`, and `TaskId.Inspect` where a literal exists. A value with no literal is parsed with `ZIO.fromEither`, so a regression fails with its own error instead of `assertTrue(false)`.
- [x] P47 `CliExitLawSpec` overlaps wire's `CliLawSpec` almost entirely after `CliExit` collapsed to `Success`, `Failed`, and `Usage`. Merge (P17) and delete the duplicates.
  - Every row had a wire row with the same code, so the spec is deleted and wire is unchanged.
- [x] P48 `Cli.kindOf` and `Cli.askBranch` decided a prompt with `java.lang.System.console() == null` outside ZIO, then read `System.console().readLine`, so no law could drive either answer.
  - `Stdio(input: Attached, output: Attached)` is the process's standard streams, from the probe P51 runs once. A prompt reads `Console.readLine` only for `Stdio(Terminal, Terminal)`, the test `System.console()` makes. `fresh` reads `Stdio` from its environment. Red, with the guard put back to `System.console() != null`: in the test JVM no person was asked, and every fed answer stayed unread. Green: `CliPromptLawSpec` under `TestConsole`, where n, no, or bare is a bare project, a blank or missing branch is `main`, and a flag or a redirected stream asks nobody and leaves the fed line unread.
- [ ] P49 `Cli.deps` read `LocalDate.now` for the cooldown day. Owner: engine M-Z, which owns `Cli.deps` and its `GH_TOKEN` read. Its branch drops `today` from `Cli.deps` and `DepsSurface` reads the day from `Clock`.
- [x] P50 `setup` ran `SetupSurface.run` synchronously inside `ZIO.succeed`. B19.
- [x] P51 `Paint` probed the terminal in a lazy val that started `/bin/sh` the first time a line was painted.
  - `Stdio.probed: ULayer[Stdio]` runs `test -t 0` and `test -t 1` on the inherited descriptors once, and a probe that cannot start is redirected. `Main` provides it, `Paint.render` reads the output from the environment, and every law provides `LawMachine.redirected`. Red, with `render` ignoring its `Stdio`: a terminal output got `1 passed` with no colour. Green: `PaintLawSpec` "render paints for the output of the Stdio it is given, whatever the input is".
- [x] P52 `Daemon.open` defaulted `plugins`, `machine`, `relaunch`, and `assembly`, and the two defaults `Calls.quiet` and `Calls.idleMachine` existed only for them and for laws.
  - `open` takes the `LoadedBuild` in place of its graph, build, and plugins, and names `machine`, `relaunch`, and `assembly` at both callers: `Cli.boot` passes `Daemon.spawn` and the assembly mark, and `DaemonOpenLawSpec` its own relaunch and `None`. `Calls.quiet` and `Calls.idleMachine` are gone from production, and the laws use `LawPlugins.none` and `LawMachine.idle`. Proof: `./histos test app`.
- [x] P53 `histos.version.SessionError` and `Session.SessionError` were both titled `SessionError` in the MCP error schema. B18.
- [x] P54 The four `Cli.launch` specs (`CliClasspathLawSpec`, `CliGapLawSpec`, `CliPlanLawSpec`, `CliReadyLawSpec`) loaded through `Resolve.coursier`, which `Cli.withLocal` and `Cli.withLoader` built inline in retain mode, so a miss could download.
  - `Cli.launch` requires `Resolve`, `Main` provides `Resolve.coursier`, and no other line of `Cli` builds one. The four specs provide `stubbed[Resolve]`, which fails a law that reads it: a build with no plugins resolves nothing for `graph`, `affected`, `toolchain`, `new --bare`, or `preview compile`.
- [x] P55 `DriverLawSpec` ran under `withLiveClock` in three `/tmp` directories.
  - The old driver records the replacement before it answers the reboot, so `Attach.replace` finds it on its first read and never sleeps. The suite runs on the test clock with no clock warning. Each law has one directory under `/tmp`, removed after it, because a Unix socket path must stay short.
- [x] P56 `ReportLawSpec` "a diagnostic decodes without a path" matched zio-json's missing-field text.
  - The text asserts are deleted. The required fields are `WireSchemaLawSpec`'s `Diagnostic` row, and the `unknown severity` text is its `Severity` token row. The law pins the document with no path both ways.
