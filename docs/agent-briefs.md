# Agent briefs

The open work of [cli-work-plan.md](cli-work-plan.md), written so that each brief can be handed to one agent. Each brief names its goal, the decisions that bind it, the ledger rows and bugs it closes, where to start reading, what it waits on, and the proof that closes it. The plan's checklist is the source of truth for done. A brief is finished when its checklist box is checked with a "Done:" list, in the pull request that earned it.

## The prompt

Give this to the main agent:

> Read `AGENTS.md`, then `docs/agent-briefs.md`, including "Russ's answers". You are the main agent. Run the briefs in the order the "Order" table gives. Spawn one zio-architect subagent per brief, each in its own short-path worktree, and run at most three at a time. Give each subagent its brief and the "Every brief" rules. Review its work, prove it on the tip, open its pull request (stacked where a track needs it), read the CI test log, and merge when green, as Russ's answers allow. Fold anything a subagent finds into the ledger with the next free number. Stop for Russ only where a brief or his answers say so.

## Russ's answers

These were given on 2026-11-13. They stand for every brief, and they replace the questions the briefs used to ask.

- **D12, remote cache writes (row 52).** CI writes the remote cache and laptops only read it, as in sbt 2, Mill, Bazel, and Gradle.
  - CI's push job writes by namespace and per the build's `promote` setting. A laptop writes only its local store.
  - Every download is checked against its digest.
  - A compile whose sources changed while it ran is not stored. This is Bazel's `--guard_against_concurrent_changes`.
  - `histos cache forget <namespace|module>` purges a bad entry.
  - `histos cache verify [module ...]` recompiles stored hits in a scratch directory and names any entry whose products differ.
  - `clean --full` stays the local recovery.
- **D13, publishing.** Standing approval for `publish snapshots` from a layer's commit and for pinning that commit channel in the same pull request, by the sequence in "Pin moves". It never covers `publish release`, a tag, or `sonaRelease`.
- **D14, merging.** The main agent merges its pull requests once CI is green on the current head and the test log has no `not advancing the test clock`. Stacks merge in order.
- **D15, Container.** Superseded by D19. A publish does not cross-build the other platform.
- **D19, host client.** A publish links the host only. A Mac names `macos-aarch64`. A Linux machine names `linux-x86_64`. There is no container and no cross-build. Client files already under `dist/clients` are still uploaded, which is how the workflow's macOS job feeds the Linux publish job. A laptop publish therefore names the host. Row 12 closes here. The current pin stays macOS-only, and a Linux job on it runs `java -jar`.
- **D16, Scala Native 3.10.1.** At the start of a session, check whether a Scala Native nightly publishes both `scala3lib` and `nscplugin` for 3.10.1. If one does, move `api`, `wire`, and `client` to 3.10.1, with that nightly's `ToolCatalog` change and a pin move, as one pull request.
- **D17, library gaps.** File one issue on heddle (server shutdown polls the clock, B11) and one on slayer (`stub` forces lambda parameter types). Draft each with its evidence, show Russ the text, and file it after his yes.
- **D18, finished plans.** Read-through deletes a work plan whose items are all done. The README or a reference doc keeps what users need, and git history is the archive. Live plans (`cli-work-plan.md`, `cache-work-plan.md`, `scenario-work-plan.md`, `test-audit.md`, and this file) are made true at the tip.

## Every brief

These apply to every agent, on top of AGENTS.md and the zio-architect ethos.

- **Read first.** AGENTS.md, the brief's own section here, the plan item and its ledger rows, and the "Decisions" section of [cli-work-plan.md](cli-work-plan.md). A cache brief also reads [cache-work-plan.md](cache-work-plan.md), and a brief that adds a scenario reads [scenario-work-plan.md](scenario-work-plan.md).
- **A worktree of its own.** Only one `./histos` runs in a checkout. Create the worktree at a short path, because the pinned toolchain's socket path counts toward the 104-byte macOS limit:

  ```sh
  git worktree add -b histos/<item> ~/.grok/worktrees/early-effect-histos/<item> origin/main
  ```

  Copy `AGENTS.md` into the worktree. Do not link `dist/histos.jar`, and do not write `.histos/assembly.sha256` or `.histos/allow-drift`. `./histos` execs `~/.histos/bin/histos`. The shim uses `.histos/toolchain` when that file names an installed toolchain, and otherwise the channel in `build.properties`. The pin is `e63ffcc`, channel `0.3.0-e63ffcc0142a-SNAPSHOT`. A command that must run a jar packed from the worktree is `java -jar` on that jar. `publish local` installs it and writes `.histos/toolchain`.
- **Bugs first.** A product bug gets a law that fails on today's code, with the red output captured, then the fix, then green. Record it in [test-audit.md](test-audit.md) under "Product bugs". The next free number is B44; take the next free one when you add a row, and say so in the report.
- **Decisions are Russ's.** A change to what the product does that "Russ's answers" do not cover is a question for Russ before it is code. Record the answer in [test-audit.md](test-audit.md) under "Decisions for Russ". The next free number is D20.
- **No wall clock in a law.** Use TestClock, Promise gates, and effects that complete. `withLiveClock` only where real time is the subject, with a comment saying which timer needs it. No `Thread.sleep`, no polling loops, no yield counts.
- **Format once.** Run bare `./histos fmt` after the edits (its ledger skips unchanged files), then `./histos fmt --check`. Do not format file by file.
- **The proof for any pull request.** `./histos test api wire central engine app`, green on JVM and Native, with no `not advancing the test clock` in the output. `./histos workflow --check` and `./histos run doc` clean, because CI runs both, and the docs task fails on any scaladoc warning.
- **After CI.** Read the test job's log for `not advancing the test clock`, and fix any hit in the next pull request.
- **Docs teach the truth.** README, AGENTS.md, the plan, and scaladoc are corrected in the same pull request as the code they describe. `app/test/ReadmeLawSpec.scala` gates the README.
- **Known hazards.**
  - A pin move publishes a snapshot. D13 approves snapshots and pin moves. The sequence is in "Pin moves" below.

## Order

Three tracks can run side by side. Within a track, one brief at a time, because each changes what the next one stands on.

| Track | Briefs, in order | Pin move |
| --- | --- | --- |
| Cache | Repo-relative sources, then Link cache, then Remote cache | no |
| Toolchain | Ship row, then Dist layer, then CI setup | each one |
| Command line | Grammar | only if the rendered workflow changes |
| Small fixes | Closed. B40 closed with Link cache. B41, B42, and row 50 are closed | no |

Read-through is last, after every other brief.

## Cache track

### Repo-relative sources

- **Goal.** The source row of a compile key hashes each source's path relative to the checkout, then its bytes. Two checkouts of the same tree share a compile hit, and the second does not start the compiler. A wiped checkout of either one still restores.
- **Start at.** `engine/src/histos/zinc/CompileInputs.scala`, `CellTools.scala` (the one key function, B23), and `engine/test/ScenarioSpec.scala`, where the law shows one compile hit across two checkouts.
- **Binds.** D7: content, never modified times. B23: recall and compile build the key with one function, so change it there. Absolute paths that Zinc or the compiler embed in outputs stay out of the key.
- **Proof.** `ScenarioSpec`, with the cross-checkout miss assertion replaced by the hit: no miss and one compiler start in total, plus a wiped checkout that restores. Check the item in both plans.

### Link cache

- **Goal.** A Native or Scala.js link whose key (`NativeKey`, `JsKey`, built on product digests) is in the store restores the linked output and does not start the linker.
- **First.** Apply D10. Russ: `ProductDigest.normalize` strips only the checkout root and leaves every other string intact. It used to cut every rooted string constant to its last segment (ledger row 54), and a link that skipped on that digest would restore an old bundle after `/api/v1` became `/api/v2`. The law is two digests that differ only in such a constant differ. Closed with the Link cache item.
- **Also closes.** B40: a Native compile whose `nscplugin` did not load succeeds and is stored without NIR, and later links fail until `cleanFull`. A Native compile that produced no NIR for a source fails with a typed error and stores nothing.
- **Start at.** `engine/src/histos/zinc/ProductDigest.scala`, `NativeKey` in `engine/src/histos/native/NativeLink.scala`, and `engine/src/histos/js/JsKey.scala`.
- **Proof.** A scenario that counts linker starts: a trailing comment keeps the link, a method body relinks, a changed path constant relinks, and a wiped checkout restores without a link.

### Remote cache

- **Binds first.** D12 answers ledger row 52.
  - Only CI writes the remote.
  - Downloads are checked against their digest.
  - A compile whose sources changed while it ran is not stored.
  - `histos cache forget <namespace|module>` and `histos cache verify [module ...]` land with this brief, each with laws, and in MCP parity.
  - A law shows that a laptop run never writes the remote.
- **Goal.** Module compile, package, and link read and write the remote cache through the same namespace walk plugin tasks use. The push job writes `main` per the build's `promote` setting. `purge` is either reachable or deleted.
- **Binds.** D6: a remote timeout or refusal is a miss, never a hang or a failure. `GrpcChannel.Idle` re-arms per frame.
- **Start at.** `engine/src/histos/schedule` (`ScheduleAspects`, `GrpcCache`, `GrpcChannel`), and the plugin task namespace walk.
- **Proof.** A scenario against the loopback store (`engine/test/GrpcLoopback.scala`): a second checkout's compile is a remote hit with no compiler start, a pull-request namespace never writes `main`, and `copy-index` against `rebuild` behaves as the setting says.

## Toolchain track

Each brief here changes text the pin renders or build API the pin compiles, so each ends with a pin move, which D13 approves. They change self-host machinery that today's local runs depend on. Run them on a worktree and keep the main checkout on the pin.

### Ship row

- **Closed.** The row is `Ship("histos").next(0, 3, 0)(api, app).toolchain(launcher = app, client = client)`. The miniature held: a Native module is accepted as `client` with no call-site ascription, and a JVM-only module is refused with `the toolchain client builds Native, and this module does not`. Laws: `ToolchainLawSpec`, `WorkflowInstallLawSpec`, `CompileIdentityLawSpec`, `CallsAssemblyLawSpec`, `AssembleLawSpec`. The red and the counts are on the Ship row in [cli-work-plan.md](cli-work-plan.md). The pin is `c5d402f`, channel `0.3.0-c7d1b280cc0a-SNAPSHOT`.

### Dist layer

- **Closed.** `publish local`, `publish snapshots`, and `publish release` pack the launcher, link the host client, and prove the pair before Maven and before the `dist` upload. The new client runs `version`, then `graph`, against the new jar, in a copy of HEAD. A broken handshake is `PairError.Handshake` and refuses both. `publish local` installs the toolchain under `~/.histos/toolchains/<id>` and writes `.histos/toolchain`. The shim prefers that file over `build.properties`. A stale local toolchain prints one warning line, and `doctor` reports `toolchain.local` as a warning, so the exit stays 0. `HISTOS_ASSEMBLY`, `dist/histos.jar` as a launcher, `.histos/assembly.sha256`, `AssemblyMark`, the `ApiClock` stop, and `allow-drift` are deleted from the product. The identity resource stays. The client accepts `server.json` only when its stamp is strictly newer than the stamp noted before spawn, and the restart error names the version. Laws: `PairProofLawSpec`, `ShimLaunchLawSpec`, `ToolchainLocalLawSpec`, `ApiClockLawSpec`, `ServerStampLawSpec`, `WorkflowInstallLawSpec`, `LauncherScriptLawSpec`. The red and the counts are on the Dist row in [cli-work-plan.md](cli-work-plan.md). The pin is `e63ffcc`, channel `0.3.0-e63ffcc0142a-SNAPSHOT`. `workflow --update` rewrote `./histos`, `histos-snapshots.yml`, and `histos-release.yml`. `ci.yml` was already what this jar renders.

### Container

- **Closed, not built.** D19: a publish links the host only. The cross-build container is dropped. A laptop names the host platform. The snapshot and release workflows still link each platform on its own runner. No pin move.

### CI setup

- **Generator landed. The pin has not moved.** `Projects.ciSetup(CiSetup.Remote)` is the default and renders `early-effect/histos/.github/actions/setup@<full commit of the rendering ship> # <that same commit>`. The comment is that commit, because the pin scan requires one and this action has no release tag. `CiSetup.Wrapper` renders pinned `actions/cache` steps and then the job's `./histos`, with no Histos action. `origin-head` is the inline step `git remote set-head origin --auto`. `shim-<sha256>` is uploaded beside `shim` and `shim.sha256`. The bootstrap wrapper installs that pinned asset, checks the sha256, and refuses a JDK older than `Toolchain.Floor` before the fetch. `init`, `new`, and `workflow --update` write it for every build. The shim sets `HISTOS_VERSION` for `server`, `bsp`, `mcp`, and a client command. E29 and B39 close against that code.
- **Still open.** The committed workflows and `./histos` stay the pin's render, so the checklist box stays open. Live: a remote action pinned at a commit that lives only on a merged branch still has to resolve after that branch is deleted. That check is not a unit test and is not done.

## Command-line track

### Grammar

- **Goal.**
  - The verbs with flags: `test [project ...] [--affected <json>] [--quick] [--only <pattern> ...]`, `clean [--full]`, `publish local`, and the same forms under `preview`.
  - Removed, with no aliases: `testOnly`, `testQuick`, `cleanFull`, `publishLocal`, `client`, the self-host `assembly`, `launcher`, `portal`, `observe`, the server `toolchain` (its report moves into `doctor`'s Toolchain area), and the shim's `--compile-client`.
  - `server` stays in the jar and leaves `histos help`.
- **Closes.** Ledger rows 16 to 24 and 51:
  - typed MCP arguments in place of the stringly `ResolveArgs.action`, `WorkflowArgs.action`, `TestArgs.mode`, `FmtArgs.mode`, and `PreviewArgs.operation`
  - `Help.render` returns a typed error, not `Either[String, _]`
  - `Command.New`, `Doctor`, `Setup`, and `PublishLocal` lose their booleans and strings
  - `publish promote` and `publish drop` parse a `DeploymentId`
- **Binds.** The command line and MCP stay in parity: every command has a tool that sends the same call, and a removal lands on both in the same pull request. A removed word fails with a typed error that names its replacement.
- **Watch.** If the generated `ci.yml` uses a renamed command, `workflow --check` on the current pin renders the old text, and the change needs a pin move.
- **Proof.** `GrammarLawSpec`, `HelpLawSpec`, and `McpLawSpec` cover each new form and refuse each removed word with its typed error. `ReadmeLawSpec` refuses the removed words.

## Small fixes

Each is one pull request with its red law. They touch different code, so they can run beside the tracks.

- **B41.** Closed. `attend` waits for the process, drains its pipes for one second on a blocking thread, ends the process group, and fails with `ChildError.Orphan`. The law is `ChildCommandLawSpec`. The ledger row has the red and green counts.
- **B42.** Closed. Zinc's class-file manager deletes a `.class` and whatever `AuxiliaryClassFiles` names. `JvmCompile.once` passed `IncOptions.of()`, so a deleted source's `.class` went and its `.tasty` stayed. `once` now passes `ProductSiblings`, and that same delete removes `.tasty`, `.nir`, and `.sjsir` with the class. The law is `ScenarioSpec`. The ledger row has the red and green.
- **Row 50.** Closed. `SuiteLaunch` passed no `hear`, so the default listener dropped every test-compile problem. `ModuleTest.heard` offers each `CompileFeed.Diagnostic` while the test compile runs. A warning does not fail the suite. The law is `ScenarioSpec`. The red and green are on the cli-work-plan row.

## Read-through

- **Goal.** README, AGENTS.md, `docs/self-host.md`, [next.md](next.md), [publishing.md](publishing.md), and [release-work-plan.md](release-work-plan.md) read against the code at the tip. Anything found is recorded in the ledger as a miss.
- **Binds.** D18: a work plan whose items are all done is deleted, after the README or a reference doc keeps what users need. Candidates are `coverage-work-plan.md`, `format-work-plan.md`, `plugins-plan.md`, `release-work-plan.md`, `source-work-plan.md`, `site-publish-modes.md`, and `next.md`. Check each for open items before deleting it. Live plans are made true, and links to a deleted plan are fixed.
- **Closes.** Ledger row 44: `cache-work-plan.md` still checks "TestStore" with the deleted `ModuleTestQuickSpec` as its proof.

## Outside the plan

- **Scala Native 3.10.1 (D16).** `api`, `wire`, and `client` stay on Scala 3.10.0 until a Scala Native nightly publishes `scala3lib` and `nscplugin` for 3.10.1. Check for one at the start of each session. When it ships, move them with the `ToolCatalog` change and a pin move.
- **heddle issue (D17).** Filed as early-effect/heddle#109 and released in 0.15.0. Halt completes when the in-flight set empties. Histos depends on that release.
- **slayer issue (D17).** Filed as early-effect/slayer#5 and released in 0.3.0. A handler lambda is checked against the selected method, so it needs no parameter ascription. Histos depends on that release. The laws still carry ascriptions.

## Pin moves

This is the sequence that worked twice. It needs Russ's yes in that session.

1. Commit the layer. The snapshot coordinate is HEAD.
2. Run `./histos publish snapshots`. That command packs the launcher, links the host client, and proves the pair before the Maven publish and before the `dist` upload. A failed proof is `PairError.Handshake` and nothing uploads.
3. Check that the uploaded `<timestamp>-manifest.json` names the jar's sha256 and the host client's. A platform that was not linked is omitted. None at all is refused.
4. Set `histos.version` in `build.properties` to the commit channel `0.3.0-<sha12>-SNAPSHOT`. Stop the server, run one command so the server rewrites `.bsp/histos.json`, and run `./histos workflow --update`.
5. Commit the pin on top of the layer, then stack the next layer on it.

The pin is `e63ffcc`, channel `0.3.0-e63ffcc0142a-SNAPSHOT`. That toolchain packs the launcher, links the host client, and proves the pair before it uploads. `workflow --check` compares the committed workflows and `./histos` to the jar this channel names.
