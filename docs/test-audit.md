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

- [ ] B1 `Cond.Not` renders `!github.event_name == 'push'`. GitHub reads that as `(!github.event_name) == 'push'`, which is always false, so a job that negates an event, input, or branch never runs. `Not` renders `!(…)` always. Owner: engine A-L (`JobCondLawSpec`), and api checks `JobCheckLawSpec:108`.
- [ ] B2 `JobYaml.lift` ends in `case other => CiError.UnpinnedWorkflowRun(other.message)`, so a `BadName` or a `DuplicatePlugin` reports as an unpinned workflow run. `CiError` gets an honest case for a non-CI build failure. Owner: engine A-L, and engine M-Z points `WorkflowLawSpec:293` at it.
- [ ] B3 `Decisions.scala:133` names a cross-version split's cells with the coordinate (`split.toList.map(CellName.trusted)` over the groupBy keys), and calls `split.head`. The cells are the rows' `cell`. Owner: engine M-Z (`SupplyDecisionLawSpec:135`).
- [ ] B4 `VersionOrder` collects every digit run, so `1.2.3-RC1` sorts above `1.2.3`, and `NodeToolchain.highest` prefers `v25.0.0-rc.1` over `v25.0.0`. A pre-release sorts below its release. Owner: engine M-Z (`ToolchainLawSpec:13`).
- [ ] B5 `DeploymentId.from` fails a mistyped id with `PortalError.Unnamed`, the portal-response error, so the operator reads "the portal upload did not name a deployment". It gets its own error case. Owner: central, and engine M-Z updates `ReleaseRunLawSpec:168`.
- [ ] B6 The daemon outbox `close` is `outbound.shutdown *> fiber.join`. A shut-down queue does not hand its remaining lines to the sender, so the BSP exit `Marker` that `Daemon.scala` offers just before `close` can be dropped. Prove the queue behavior in a miniature spec first. Owner: app (`DaemonOutboxLawSpec`).
- [ ] B7 The MCP `run` tool accepts an empty id (`runCall("", None, empty) == Right(Call.Run("", None))`), while CLI `run ""` is `BadTaskId` and MCP preview is `MissingRunName`. Parity: reject it. Owner: app (`SessionLawSpec:22`).
- [ ] B8 `NO_COLOR=""` turns colour off. no-color.org disables colour only for a present, non-empty value. `Paint` also reads `System.getenv` outside ZIO. Owner: app (`PaintLawSpec:14`).
- [ ] B9 `Perform.jdkMissed` has a `Test` branch that is the same as every other call, and its scaladoc claims a distinction that does not exist. Delete it. Owner: app (`PerformLawSpec:46`).
- [x] B10 `JobCheck` reads `Not(c)` as reachable exactly when `c` is. `Not(Event(Schedule))` in `ci.yml`, which has no schedule and so is always true, is refused as `UnreachableCond`, and `Not(True)`, which never runs, is accepted. `Not(c)` is reachable when `c` can be false for the file's events, and refused when `c` is always true. The same model found `DefaultBranch` refused on a schedule, which always runs on the default branch. Rendering is B1. Owner: api (`JobCheckLawSpec`).
- [x] B12 `ShipLink` checked the catalog rules with positions, then `Catalog.from` checked them again with none, and `asBuild` turned that second refusal into a made-up error: `SourcePos("<catalog>", 1, 1)`, a `DuplicateShip` with one position twice, or a `BadShipName` for a duplicate. `Catalog.from` takes each row with the place it was written and is the one check, so every refusal carries a real position and the fallback cannot be written. Owner: api (`CatalogLawSpec`).

## Decisions for Russ

- [ ] D1 `fmt` rewrites sources a compile reads. Does it hold the writer permit? The recommendation is yes. Today `Call.Format` is missing from the permit and `Perform` tables (`DaemonWatchLawSpec:37`, `PerformLawSpec:42`). Owner: app.
- [ ] D2 `~ publishLocal <x>` watches every module, and `preview publishLocal <x>` watches only the build file, because `PublishLocal` and `PublishSnapshots` fall through `case _ => Every` in `Watching`. The recommendation is to route both through `publish(graph, tokens)`. Owner: app (`DaemonWatchLawSpec:41`).
- [ ] D3 The rehearsal portal reports no finding for an empty bundle, so it marks an empty upload `VALIDATED`. The recommendation is that an empty bundle is a layout finding. Owner: central (`PortalReportLawSpec:42`).
- [ ] D4 The rehearsal portal accepts a `.sha256` file with a wrong digest. The recommendation is that a checksum file that is present is checked. Owner: central (`PortalReportLawSpec:89`).
- [x] D5 `JobCheck` read tag and label conditions from the triggers alone. A `workflow_dispatch` run from a tag never made `Event(Tag)` or `TagMatches` reachable, and `HasLabel` needed a labeled pull-request type, so `ci.yml` refused both. Owner: api (`JobCheckLawSpec`).
  - Russ: reachable means GitHub can make it true. A dispatch runs from any ref, so a ref condition (`TagMatches`, `Event(Tag)`, which renders as a ref test, and `DefaultBranch`) is reachable in a file with dispatch. An event condition is the event name the start sets, so dispatch never makes a push condition true. A label condition reads `github.event.pull_request`, so it is reachable in a file with any `pull_request` trigger.

## Shared seams

A law that touches the machine because the product does needs a seam in the product.

- [ ] S1 Compiler closure. About 30 laws compile with Zinc through `Resolve.offline(CacheDefaults.location)` and this checkout's `dependencies.lock`, found by walking up from the working directory, with the Scala version written as a literal. A compile-cache hit fetches nothing, so on a cold Coursier cache these pass or fail with run order (row 48). The fix: one shared fixture in `engine/test` reads the lock's compiler tool rows offline, takes the Scala version from the lock, and fails with a named precondition ("run `./histos resolve`") when a jar is missing. `./histos resolve` is the documented step, and the setup action runs it in CI. Owner: engine A-L. engine M-Z and app move their copies onto the same shape.
- [ ] S2 `IvyLocal` writes Coursier's static `LocalRepositories.ivy2Local`, so `FilePublishLawSpec`, `IvyLocalLawSpec`, and `PublishInstallLawSpec` write and wipe the real `~/.ivy2/local`, and two worktrees delete each other's install. `IvyLocal` and `LocalStock.layer` take the root. Owner: engine A-L.
- [ ] S3 `Cli.launch` provides `ReleaseFeed.live` inline. Once a law writes a pin, every launch fetches `repo1.maven.org` and writes `$HOME/.histos/upgrade`. `launch` requires `ReleaseFeed` and `Main` provides the live layer. Owner: app.
- [ ] S4 `SonaStock`'s snapshot repository is fixed to `SnapshotRepository.central`, so only validation order keeps `SonaPublishLawSpec` off central.sonatype.com. The repository becomes a seam, and the law points it at a loopback that fails any request. Owner: engine M-Z.
- [ ] S5 The supply surface reads `sys.env` (`GITHUB_REPOSITORY`, `HISTOS_DRY_RUN`) and runs `git` synchronously outside ZIO (`Surface.scala`). The laws test whichever branch the shell selects. Both go through `zio.System` and a git reader. Owner: engine M-Z.
- [x] S6 `SnapshotId.current` runs the real `git` on JVM and Native with the ambient environment and global config. The HEAD read becomes a dependency the law stubs. Owner: wire.
  - `SnapshotId.current(repo, head)` takes the HEAD reader, `ApiClock` and `ToolchainStamp` pass `SnapshotId.head`, and `ApiClockLawSpec` stubs it with a recording `Ref` (HEAD is read only for an `app` ship). No wire law starts a process; `./histos test wire` is green on JVM and Native.
- [ ] S7 `FormatterLawSpec` uses `Resolve.retained`, which downloads a missing scalafmt jar from Central, and reads this checkout's `.scalafmt.conf`. The scalafmt jars are read offline like S1, and the law writes its own config. Owner: engine A-L.
- [ ] S8 Laws read `TestClock.instant` and discard it (`_ <- Clock.instant`), which only starts the "not advancing the test clock" warning. Delete each line: `PackLawSpec`, `PreviewLawSpec`, `PublishLawSpec` (and its helpers), `ScaffoldLawSpec`, `SignerLawSpec`. `TaskLawSpec:62` sleeps an hour on an unadjusted clock where `ZIO.never` is the intent. Owner: engine M-Z.

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

## central

- [ ] C1 `PortalClientLawSpec:28, :49, :69, :131, :147, :163`, `PreflightReadLawSpec:60` WEAK. The `Left` arm accepts the parser's only failure, so a parser regression skips the law. Parse inside the effect with `ZIO.fromEither`.
- [ ] C2 `PublishIdentityLawSpec:144` MERGE. The same 11 `TransportFaults.from` rows as `PortalClientLawSpec:199`. Keep one.
- [ ] C3 `PublishIdentityLawSpec:163` DELETE. `MessageLawSpec:796` pins it.
- [ ] C4 `LayoutLawSpec:166` WEAK. The "quote" fixture `a&b` has no quote. Use `a"&b` and assert the `<groupId>` element.
- [ ] C5 `PortalReportLawSpec:42` REWRITE. The title says an empty zip is a layout finding, and the assert says no finding (D3).
- [ ] C6 `PortalReportLawSpec:89` WEAK. The `.sha256` fixture is a wrong digest (D4).
- [ ] C7 `PortalFlowLawSpec:10` REWRITE. Copies `beat`, and the 30th read is never pinned. Assert `beat(29, Same) == (Say, 0)` literally.
- [ ] C8 `PortalClientLawSpec:88, :103`, `PreflightReadLawSpec:36, :59`, `PortalStoreLawSpec:13` WEAK. Rows reduce to `Boolean` and `forall(identity)`, which names no row. Collect mismatches and assert `misses == Nil`.
- [ ] C9 `case Left(_) => assertTrue(false)` in `LayoutLawSpec`, `PortalFlowLawSpec:85, :98, :101`, `PortalReportLawSpec:30, :195`, and the `"unused"` placeholders in `LocalPublishLawSpec:40, :55`, drop the error. Use `ZIO.fromEither` steps.
- [ ] C10 B5.

## engine A-L

- [ ] E1 `ApiJarLawSpec:19` DELETE. `ApiJar.locate` has no caller. Delete `ApiJar`.
- [ ] E2 `ArchiveLawSpec:13` DELETE. `RuntimeCache.prepare` has no caller. Delete it.
- [ ] E3 `AssetLawSpec:45` WEAK. `isRight` and `isLeft` only. Assert `bases(Map.empty) == Right(Bases(...))`, each override, and the `Left`.
- [ ] E4 `AssetsLawSpec:12` MERGE. The same refusals as `AssetLawSpec:24`. One `check` over generated parts plus the loopback cases.
- [ ] E5 `AssetSyncLawSpec:58` WEAK. Matches message text. Assert `Left(AssetError.Collides(...))`.
- [ ] E6 `CacheLawSpec:117, :139` DELETE. `StoreScope` has no caller, and `:139` reads `published` before the commit can run. Delete `StoreScope`.
- [ ] E7 `CacheLawSpec:157` DELETE. `histos.cache.Scratch` has no caller. Delete it.
- [ ] E8 `ChannelLawSpec:8` DELETE. `ChannelRefresh` has no caller, and the shim has its own TTL in shell. Delete it.
- [ ] E9 `CiBranchLawSpec:13` HOST. Reads this checkout, can run `git remote set-head` and `git ls-remote`. Build a temp repo with a local bare `origin`.
- [ ] E10 `ClangLawSpec:15` HOST. Needs clang on the host PATH. Prove `resolve` on the scripted `pair` directory only.
- [ ] E11 `ClassShapeLawSpec:14` HOST. Copies an earlier build's `.histos/classes/api/3.10.0/...`. Emit the class with ASM.
- [ ] E12 `CleanLawSpec:32` WEAK. Plants files only under two roots and repeats the root list. Plant under every root and assert each is gone.
- [ ] E13 `CoverageLawSpec:263` DELETE. A copy of `CompileLawSpec:43`. Merge `CompileLawSpec` into `CompileIdentityLawSpec`.
- [ ] E14 `DoctorProbeLawSpec:228` MERGE. Repeats `DoctorRenderLawSpec:98, :108`. Delete.
- [ ] E15 `DoctorProbeLawSpec:351` MERGE. Keep only the `Registry.collect` ordering assert.
- [ ] E16 `AssembleLawSpec:66`, `CoverageOrderLawSpec:41`, `DocLawSpec:39, :84` HOST. S1.
- [ ] E17 `FilePublishLawSpec:28` HOST. S2. Also L81-82 cannot fail, and L126-135 copies Coursier's ivy-home lookup. Fold the overwrite case into `IvyLocalLawSpec`.
- [ ] E18 `FormatterLawSpec` (6 laws) HOST. S7.
- [ ] E19 `GitHubCredentialLawSpec:84` DELETE. Runs the real `gh auth token`. L28 proves `resolve` with an injected `gh`.
- [ ] E20 `GraphCellLawSpec:34`, `GraphTastyLawSpec:36` HOST. S1.
- [ ] E21 `GrpcCacheLawSpec:34`, `GrpcChannelLawSpec:27` FLAKY. Bind port 0, close, and hope, with no deadline. One loopback gRPC server for the law that answers `UNAVAILABLE` and `NOT_FOUND`, and a channel deadline.
- [ ] E22 `GrpcChannelLawSpec:12` DELETE. Restates the method descriptors. Replace with a loopback `store` then `find` through `GrpcCache`.
- [ ] E23 `IvyLocalLawSpec:20` HOST. S2. It also hard-codes `user.home/.ivy2/local`, which ignores `ivy.home`.
- [ ] E24 `JdkLawSpec:37` WEAK. L45-55 restate constants. Keep the Os rows.
- [ ] E25 `JobCondLawSpec:33` REWRITE. Pins B1. Assert `!(github.event_name == 'push')`, and the pages check gets its own law.
- [ ] E26 `JobYamlLawSpec:56` REWRITE. Pins B2, and the 14-row table restates the match.
- [ ] E27 `JobYamlLawSpec:90` WEAK. Restates `uses`. Assert the literal `name@sha # tag` line.
- [ ] E28 `JvmCompileLawSpec:28` DELETE. Unreachable input: `ScalaJs.compile` refuses it first.
- [ ] E29 `LauncherScriptLawSpec:33` REWRITE. Pins `-ge 21` against the floor of 25. Owner: CI setup item (row 29), which renders the floor from `Toolchain.Floor`.
- [ ] E30 `LoaderPluginLawSpec:26` WEAK. Any diagnostic passes. Assert the unresolved `law` reference.
- [ ] E31 `LoaderProjectLawSpec:48` DELETE. Passes when the loader ignores the wiring file.
- [ ] E32 `LoaderProjectLawSpec:56` WEAK. Assert the diagnostic points at the wiring source.
- [ ] E33 `LoaderPublishLawSpec:38`, `LocalStageLawSpec` (6 laws) HOST. S1.
- [ ] E34 `LockLawSpec:22` REWRITE. One example. `check(genLock)` with plugin rows and evictions, `parse(render(lock)) == Right(lock)`.

## engine M-Z

- [ ] E35 `PublishLawSpec:282, :294, :319` WEAK. The fallback arm accepts `NothingToPublish`. Assert the one expected `Left`.
- [ ] E36 `PublishInstallLawSpec:26` HOST. S2.
- [ ] E37 `ProgramLawSpec:37` WEAK. No positive case, and the `$$anon` file is garbage bytes. Write real classes with ASM: a `Main` that is found and a `Foo$$anon` that is not.
- [ ] E38 `PluginAspectLawSpec:66` WEAK. Only checks a value passes through. Read the context loader inside `ZIO.succeed` and `ZIO.attemptBlocking` and assert `eq` to a fresh loader.
- [ ] E39 `NodeChooseLawSpec:18` HOST. Reads the host PATH under `withLiveSystem`, and runs a 2 s timeout on an unadjusted clock. Use `goodHome/bin` as `PATH`.
- [ ] E40 `ModuleMissLawSpec:35` WEAK. Assert the test class exists after the run.
- [ ] E41 `NativeRequestLawSpec:52` WEAK. `forall` on lists that may be empty, and `.head`. Assert the names, and `headOption.contains`.
- [ ] E42 `PublishRecordLawSpec:71` WEAK. `contains("recordlaw")`. Assert `List("recordlaw_3")` and the revision.
- [ ] E43 `ModuleTestLawSpec:36`, `ProgramRunLawSpec:37`, `NativeLawSpec:116`, `ProcessLawSpec:14`, `PublishLawSpec:124, :205` DELETE. Subsets of another law, or restatements.
- [ ] E44 `PluginRunLawSpec:42` MERGE into `PluginOutcomeLawSpec:104`.
- [ ] E45 `PreviewLawSpec:112` MERGE into `PlanShowLawSpec:17` with an `idle` module.
- [ ] E46 `PublishLawSpec:365` MERGE. Move `summed` and `sized` into `BundleLawSpec`.
- [ ] E47 `PublishReleaseLawSpec:47` MERGE. Move `promote` next to `ReleaseRunLawSpec:206`.
- [ ] E48 `ProviderInstallLawSpec:46` REWRITE. `libs.head`. `libraries` returns a `NonEmptyChunk`.
- [ ] E49 `ProvisionLawSpec:34` MERGE. Keep the `made.out` asserts.
- [ ] E50 `ProvisionLawSpec:129` REWRITE. `withLiveClock` does nothing and its comment is false. Remove both.
- [ ] E51 S8 lines in `PackLawSpec`, `PreviewLawSpec`, `PublishLawSpec`, `ScaffoldLawSpec`, `SignerLawSpec`, and `TaskLawSpec:62`.
- [ ] E52 `SupplyDecisionLawSpec:135` REWRITE. Pins B3.
- [ ] E53 `SurfaceExchangeLawSpec:78, :121, :138`, `SupplySurfaceLawSpec:124` HOST. S5.
- [ ] E54 `SurveyLawSpec:69, :93` HOST. `withLiveSystem` where `Machine.load` already reads `zio.System`. Use `TestSystem`, and drop the JDK assert.
- [ ] E55 `ZincCompileLawSpec` (4 laws) HOST. S1.
- [ ] E56 `ScheduleLawSpec:394, :412` FLAKY. `observe` yields 32 times and reads. Wait on the terminal-event Promise, as `watch` does.
- [ ] E57 `ScheduleLawSpec:227` WEAK. Renders only `url.absolute`. Assert the url renders back to the input.
- [ ] E58 `ScheduleLawSpec:131` (and L619) DELETE. Enum declaration order.
- [ ] E59 `ScheduleLawSpec:46` MERGE into L41 over generated `Need` subsets.
- [ ] E60 `ScheduleLawSpec:355, :362` MERGE. One law over both `Promote` values.
- [ ] E61 `ScheduleLawSpec:689` DELETE. L660 and L676 cover it.
- [ ] E62 `SuiteLawSpec:268` DELETE. `TestCompile.distinct` is `List.distinct`. Replace the helper.
- [ ] E63 `SuiteLawSpec:289` REWRITE. Assert `analysisDirOf(testClasses(...))` is the sibling `test-analysis`.
- [ ] E64 `SuiteLawSpec:76, :95`, `SuiteFilterLawSpec:42, :52, :91` MERGE into `SuiteNamesLawSpec` and `SuiteLawSpec`'s unique cases.
- [ ] E65 `ResolutionServiceLawSpec:226` REWRITE. Assert no `dependencies.lock` exists after a solve, in L105.
- [ ] E66 `ReleaseRunLawSpec:168` REWRITE. Pins B5.
- [ ] E67 `ToolchainLawSpec:13` REWRITE. Pins B4.
- [ ] E68 `WorkflowLawSpec:214` MERGE. Repeats `WorkflowFileLawSpec:71`.
- [ ] E69 `WorkflowLawSpec:293` REWRITE. B2.
- [ ] E70 `WorkflowLawSpec:338` WEAK. No overlapping keys, so a shortest-prefix bug passes. Add an overlapping pin.
- [ ] E71 `SignerMessageLawSpec:8` DELETE. Restates `SignerError.message` without coverage.
- [ ] E72 `SonaPublishLawSpec:31, :46` HOST. S4.
- [ ] E73 `ReapiLawSpec:13` REWRITE. Hand-picked ASCII values. `check` over long and non-ASCII strings, digests, and payloads.
- [ ] E74 `Sha256DigestLawSpec:8` REWRITE. Tests the JDK. Keep the parse round trip.

## app

- [ ] P1 Checkout fixture. `BspRemainLawSpec:37`, `BspSuiteLawSpec:38`, `BspTargetLawSpec:42, :98`, `CallsAssemblyLawSpec:39`, `CallsCompileLawSpec:38`, `CallsRunLawSpec:38`, `CallsTaskLawSpec:38`, `CallsTestLawSpec:38`, `DaemonOpenLawSpec:38, :112, :135`, and `DaemonMoreLawSpec:38` each copy the `opened` and `repository` block: this checkout's lock, the developer's Coursier cache, `Machine.load` under `withLiveSystem`, and a fixed `/tmp/hd-store`. Laws that compile use one shared fixture on the S1 shape. Laws that never compile (`BspRemainLawSpec`, `CallsTaskLawSpec`, and the refusals in `CallsAssemblyLawSpec`, `CallsRunLawSpec`, `CallsPublishLawSpec`) use stubs and a fixture JDK.
- [ ] P2 `BspSuiteLawSpec:113` WEAK. `forall` over a list that is empty on a cold cache. Assert the named sources jar.
- [ ] P3 `CallsAssemblyLawSpec:39` MERGE. The jar-entry asserts repeat `AssembleLawSpec:66`.
- [ ] P4 `CallsCompileLawSpec:86`, `CallsTestLawSpec:93` WEAK. `contains("missing")`. Assert `'missing' is not a project`.
- [ ] P5 `CallsProductLawSpec:34` WEAK. The cleanFull leg runs on an empty store and compares the formatter to itself. Seed two projects and assert only one is named and gone.
- [ ] P6 `CallsPublishLawSpec:43` HOST and WEAK. Runs `git` and copies this checkout's lock, and the receipt is never compared. Split the refusals onto a fixture JDK, and assert the receipt.
- [ ] P7 `CallsResolveLawSpec:20` HOST. Solves against the developer's cache and hard-codes resolver facts. Seed a temp offline cache.
- [ ] P8 `CallsSnapshotLawSpec:23` WEAK. `reported()` fields are always empty. Assert the `Out` lines, and drop the promote leg.
- [ ] P9 `CliCancelLawSpec:10` DELETE. `ExecuteLawSpec:76` proves it with the exact exit.
- [ ] P10 `CliCoverLawSpec:35` HOST. Retain mode downloads on a miss. Use an offline resolve, and assert the statements line.
- [ ] P11 `CliGapLawSpec:46` HOST. `git commit` honours the developer's `commit.gpgsign` and hooks. One shared git helper with `--no-gpg-sign` and `GIT_CONFIG_GLOBAL=/dev/null`.
- [ ] P12 `CliLaunchLawSpec:13` HOST, MERGE. S3. Every leg has an exact-text law elsewhere. Delete, or move the setup legs into `ExecuteLawSpec`.
- [ ] P13 `CliLaunchLawSpec:48` HOST, MERGE. Writes the real `~/.cache/histos`. Fold into `CliPlanLawSpec:25`.
- [ ] P14 `CliLocalLawSpec:14` DELETE. Reads the real content store with asserts that fit any store. `ExecuteLawSpec:140` covers it.
- [ ] P15 `CliLocalLawSpec:43` HOST, MERGE. S3. Move the pin legs into `ExecuteLawSpec.pins`.
- [ ] P16 `CliParseLawSpec:15` REWRITE. `.head` at L107 and L111, echo rows, and forwarding rows. Pattern-match, keep the literal sentences.
- [ ] P17 `CliRouteLawSpec:52` MERGE. One `CliExitLawSpec` row.
- [ ] P18 `ConnectionLawSpec:13` WEAK. Echoes constructor arguments, and `Connection.ensure` has no law. A law that `ensure` writes the decoded file, writes nothing without a pin, and leaves an equal file untouched.
- [ ] P19 `ConnectionLawSpec:27` MERGE. `LauncherPinTextLawSpec:10` has the table. Keep one missing and one bad row.
- [ ] P20 `DaemonOutboxLawSpec:9` WEAK. B6.
- [ ] P21 `DaemonOpenLawSpec:112` WEAK. `waiting.isEmpty` can never fail. Relaunch completes an `entered` promise.
- [ ] P22 `DaemonMoreLawSpec:38` MERGE into `DaemonOpenLawSpec` on the shared fixture.
- [ ] P23 `DaemonWatchLawSpec:37` WEAK. D1.
- [ ] P24 `DaemonWatchLawSpec:41` REWRITE. D2.
- [ ] P25 `FmtGrammarSpec:47` WEAK. `isLeft`. Pin the exact `CliParseError`, and move the exit assert.
- [ ] P26 `GrammarLawSpec:220` MERGE. `ExecuteLawSpec:187` pins the shim messages.
- [ ] P27 `GrammarLawSpec:269` MERGE. `HelpLawSpec:30`.
- [ ] P28 `McpLawSpec:97` WEAK. `isLeft`, and `Call` round trips test zio-json. Assert `Rejection.ProjectName`.
- [ ] P29 `McpLawSpec:124` WEAK. Counts a literal. Build `HistosMcp.tools(...)` and assert its names equal `toolNames`.
- [ ] P30 `McpLawSpec:154` WEAK. Decodes what the server only encodes. Assert `Level.Info.toJson` and `Level.Error.toJson`.
- [ ] P31 `OperationLawSpec:16` WEAK. Either of two `Left`s passes. Rows of (command, exact `Left`).
- [ ] P32 `PaintLawSpec:14` REWRITE. B8.
- [ ] P33 `PerformLawSpec:42` MERGE. Derive `readers` from `needed`, add `Format` (D1).
- [ ] P34 `PerformLawSpec:46` WEAK. B9.
- [ ] P35 `PerformRouteLawSpec:49` MERGE. `ExecuteLawSpec:155`.
- [ ] P36 `RebootLawSpec:23` FLAKY, WEAK. Real `sh`, 5 s of wall clock, and `Right(None) == Right(None)`. Require `Right(Some(_))`, and inject the spawn.
- [ ] P37 `ReportLawSpec:19` DELETE. `WireSchemaLawSpec:1881` has the `required` lists.
- [ ] P38 `RestartLawSpec:18` FLAKY, WEAK. Real processes, unbounded polling, a live sleep, and `!gone` that cannot fail. A Promise-backed fake child under `TestClock`.
- [ ] P39 `RpcLawSpec:95` WEAK. Decodes with the same codecs, and nothing pins `"jsonrpc"`. Compare to literal JSON.
- [ ] P40 `SessionLawSpec:22` REWRITE. B7.
- [ ] P41 `SessionLawSpec:94` DELETE. Derived round trips. Replace with WireSchema laws for `CallProblem`, `ServerError`, and `SessionError`.
- [ ] P42 `WireSchemaLawSpec:1968-1986` REWRITE. Strip the zio-json text, keep the schema-document asserts.
- [ ] P43 A13's app half: every Grammar built-in word is in `CommandName.reserved`.
