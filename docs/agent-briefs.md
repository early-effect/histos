# Agent briefs

The open work of [cli-work-plan.md](cli-work-plan.md), written so that each brief can be handed to one agent. Each brief names its goal, the decisions that bind it, the ledger rows and bugs it closes, where to start reading, what it waits on, and the proof that closes it. The plan's checklist is the source of truth for done. A brief is finished when its checklist box is checked with a "Done:" list, in the pull request that earned it.

## Every brief

These apply to every agent, on top of AGENTS.md and the zio-architect ethos.

- **Read first.** AGENTS.md, the brief's own section here, the plan item and its ledger rows, and the "Decisions" section of [cli-work-plan.md](cli-work-plan.md). A cache brief also reads [cache-work-plan.md](cache-work-plan.md), and a brief that adds a scenario reads [scenario-work-plan.md](scenario-work-plan.md).
- **A worktree of its own.** Only one `./histos` runs in a checkout. Create the worktree at a short path, because the pinned toolchain's socket path counts toward the 104-byte macOS limit:

  ```sh
  git worktree add -b histos/<item> ~/.grok/worktrees/early-effect-histos/<item> origin/main
  ```

  In the worktree, link the main checkout's `dist/histos.jar` and `dist/histos-client` as symlinks, copy `.histos/assembly.sha256` and `AGENTS.md`, and touch `.histos/allow-drift`. Drop the client link if the pinned client misbehaves; `./histos` then runs `java -jar`.
- **Bugs first.** A product bug gets a law that fails on today's code, with the red output captured, then the fix, then green. Record it in [test-audit.md](test-audit.md) under "Product bugs". The next free number is B44; take the next free one when you add a row, and say so in the report.
- **Decisions are Russ's.** A change to what the product does is a question for Russ before it is code. Record the answer in [test-audit.md](test-audit.md) under "Decisions for Russ". The next free number is D12.
- **No wall clock in a law.** Use TestClock, Promise gates, and effects that complete. `withLiveClock` only where real time is the subject, with a comment saying which timer needs it. No `Thread.sleep`, no polling loops, no yield counts.
- **Format once.** Run bare `./histos fmt` after the edits (its ledger skips unchanged files), then `./histos fmt --check`. Do not format file by file.
- **The proof for any pull request.** `./histos test api wire central engine app`, green on JVM and Native, with no `not advancing the test clock` in the output. `./histos workflow --check` and `./histos run doc` clean, because CI runs both, and the docs task fails on any scaladoc warning. `./histos client` links if `client` changed.
- **After CI.** Read the test job's log for `not advancing the test clock`, and fix any hit in the next pull request.
- **Docs teach the truth.** README, AGENTS.md, the plan, and scaladoc are corrected in the same pull request as the code they describe. `app/test/ReadmeLawSpec.scala` gates the README.
- **Known hazards.**
  - After a `git checkout` across a commit that moved a source between modules, the old module keeps that source's `.tasty`, and the compile fails ([test-audit.md](test-audit.md) B42). `./histos clean <module>` clears it.
  - A child process a suite leaves behind keeps `./histos test` waiting on its output (B41). If a run stops making progress, look for the orphan.
  - A pin move publishes. It needs Russ's yes in that session. The sequence is in "Pin moves" below.

## Order

Three tracks can run side by side. Within a track, one brief at a time, because each changes what the next one stands on.

| Track | Briefs, in order | Pin move |
| --- | --- | --- |
| Cache | Repo-relative sources, then Link cache, then Remote cache | no |
| Toolchain | Ship row, then Dist layer, then Container, then CI setup | each one |
| Command line | Grammar | only if the rendered workflow changes |
| Small fixes | B40, B41, B42, row 50 | no |

Read-through is last, after every other brief.

## Cache track

### Repo-relative sources

- **Goal.** The source row of a compile key hashes each source's path relative to the checkout, then its bytes. Two checkouts of the same tree share a compile hit, and the second does not start the compiler. A wiped checkout of either one still restores.
- **Start at.** `engine/src/histos/zinc/CompileInputs.scala`, `CellTools.scala` (the one key function, B23), and `engine/test/ScenarioSpec.scala`, where today's law asserts a miss across two checkouts.
- **Binds.** D7: content, never modified times. B23: recall and compile build the key with one function, so change it there. Absolute paths that Zinc or the compiler embed in outputs stay out of the key.
- **Proof.** `ScenarioSpec`, with the cross-checkout miss assertion replaced by the hit: no miss and one compiler start in total, plus a wiped checkout that restores. Check the item in both plans.

### Link cache

- **Goal.** A Native or Scala.js link whose key (`NativeKey`, `JsKey`, built on product digests) is in the store restores the linked output and does not start the linker.
- **First.** Apply D10. Russ: `ProductDigest.normalize` strips only the checkout root and leaves every other string intact. Today it cuts every rooted string constant to its last segment (ledger row 54), and a link that skips on that digest would restore an old bundle after `/api/v1` became `/api/v2`. Write that law first: two digests that differ only in such a constant differ. Then check D10.
- **Also closes.** B40: a Native compile whose `nscplugin` did not load succeeds and is stored without NIR, and later links fail until `cleanFull`. A Native compile that produced no NIR for a source fails with a typed error and stores nothing.
- **Start at.** `engine/src/histos/zinc/ProductDigest.scala`, `NativeKey` in `engine/src/histos/native/NativeLink.scala`, and `engine/src/histos/js/JsKey.scala`.
- **Proof.** A scenario that counts linker starts: a trailing comment keeps the link, a method body relinks, a changed path constant relinks, and a wiped checkout restores without a link.

### Remote cache

- **Ask Russ first.** Ledger row 52: nothing verifies a compile's output before it is stored, so a missed invalidation (B28 was one) is a permanent hit under its key, and `cleanFull` is the only recovery. Does the store verify what it keeps, and how? A remote cache makes this matter more, because a bad entry spreads to every checkout.
- **Goal.** Module compile, package, and link read and write the remote cache through the same namespace walk plugin tasks use. The push job writes `main` per the build's `promote` setting. `purge` is either reachable or deleted.
- **Binds.** D6: a remote timeout or refusal is a miss, never a hang or a failure. `GrpcChannel.Idle` re-arms per frame.
- **Start at.** `engine/src/histos/schedule` (`ScheduleAspects`, `GrpcCache`, `GrpcChannel`), and the plugin task namespace walk.
- **Proof.** A scenario against the loopback store (`engine/test/GrpcLoopback.scala`): a second checkout's compile is a remote hit with no compiler start, a pull-request namespace never writes `main`, and `copy-index` against `rebuild` behaves as the setting says.

## Toolchain track

Each brief here changes text the pin renders or build API the pin compiles, so each ends with a pin move. They change self-host machinery that today's local runs depend on. Run them on a worktree and keep the main checkout on the pin.

### Ship row

- **Goal.** `Ship("histos").next(0, 3, 0)(api, app).toolchain(launcher = app, client = client)` replaces `.clients(...)`, `ClientBuilds`, `ClientBinary.present`, `installShip`, `Dist.buildFile`, and the hardcoded `app`, `api`, and `histos.cli.Main`. `histos assembly [module ...]` becomes the generic verb: it writes each `.assembly` module to its `.asset` path, or to a build output path, prints the path, and has no launcher side effects. The launcher's identity becomes `META-INF/histos/toolchain.json`, written at pack time, so `wire` no longer recompiles per commit.
- **Closes.** Ledger rows 6 to 9.
- **Settle first.** Whether the client's platform can be a type parameter of `.toolchain`, in a `scala-cli` miniature, before the signature is fixed.
- **Proof.** Load errors for a bad launcher and a bad client, renders of the snapshot and release workflows, a compile-identity law that `wire` is unchanged across commits, and `assembly` writing an asset and printing its path. Pin move.

### Dist layer

- **Goal.** `publish local`, `publish snapshots`, and `publish release` pack the launcher, link the host client, and prove the pair (the new client runs `version`, then `graph`, against the new jar in a copy of HEAD). A failed proof refuses the upload with a typed error. `publish local` installs the toolchain under `~/.histos/toolchains/<id>` and writes `.histos/toolchain`, and the shim prefers it over `build.properties`. A stale local toolchain prints one warning line, and `doctor` reports it. `HISTOS_ASSEMBLY`, `dist/histos.jar` as a launcher, `.histos/assembly.sha256`, `AssemblyMark`, the `ApiClock` stop, and `allow-drift` are deleted.
- **Closes.** Ledger rows 10, 11, and 30. Row 30 is the startup race: the client reads `server.json` when the socket accepts, before the new server rewrites it, and the restart error names only the assembly even when the version differs.
- **Binds.** The plan's "The ship's pair is proved at publish", "A dist publisher layer", "Dogfood is a local toolchain", and "A stale local toolchain warns". The publisher is a layer like `sona`.
- **Proof.** The pair proof against a loopback fixture, where a broken handshake refuses the upload with `PairError.Handshake`; the local install layout and the override; the skew warning text; and the workflow renders. Pin move.

### Container

- **Goal.** A Mac links `linux-x86_64` in a container through `docker` or `podman`, held in a scope so the container is removed on interruption. The image is pinned by digest and has a JDK 25. The container gets a copy of HEAD and runs the Linux pair proof. A second main class in the launcher jar does the link, so the grammar gains no verb.
- **Closes.** Ledger row 12: a laptop publish names only `macos-aarch64`.
- **This machine.** Docker 29 through colima is installed, with a `linux/arm64` server. An `x86_64` link needs `--platform linux/amd64` emulation, so check it before designing around it.
- **Proof.** A fake runtime covers the commands it runs, cleanup on interruption, and the `ContainerError` mapping. Manual: a Mac `publish snapshots` whose manifest names both platforms, then pin it.

### CI setup

- **Goal.** `Projects.ciSetup(CiSetup.Remote)` (the default) renders `early-effect/histos/.github/actions/setup@<full commit of the rendering ship>`. `CiSetup.Wrapper` renders pinned `actions/cache` steps and then `./histos`, with no external action. `origin-head` becomes an inline `git remote set-head origin --auto` step. `shim-<sha256>` is a content-addressed `dist` asset. Every build gets the bootstrap wrapper, which installs that shim at a pinned sha256 when it is missing, and `init`, `new`, and `workflow --update` write it.
- **Closes.** Ledger rows 13 to 15, 29, and 55, and E29 and B39 in [test-audit.md](test-audit.md).
  - Row 29 and E29: the rendered wrapper still checks JDK 21, and the floor is 25. Render it from `Toolchain.Floor`.
  - Row 55 and B39: the wrapper's `server`, `bsp`, and `mcp` path keeps whatever `HISTOS_VERSION` its spawner exported.
- **Proof.** Renders for both choices; the wrapper installing the pinned shim on loopback and refusing a checksum mismatch; and `init` and `new` writing it. Live: a remote action pinned at a branch-only commit still resolves after the branch is deleted. Pin move.

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

- **B41.** `./histos test` waits for the end of a suite's output, not for the suite. A child the suite leaves behind holds that output open: one run took 927 s until the child was killed, and B43's leak hung CI the same way. Wait for the suite's exit, drain its output for a bounded time, end its process group, and report an orphan as a typed failure. Start at `engine/src/histos/testrun` (`SuiteLaunch`).
- **B42.** A source that moved to another module leaves its `.tasty` in the old module's classes directory, with no `.class` beside it, and the next compile reads it. Find why the class-file manager removes the `.class` and not the `.tasty`, and make a deleted source's whole product go. The red law is a scenario that moves a source between modules across two compiles.
- **Row 50.** A suite's test compile passes no problem listener, so test-source warnings print nowhere. The CLI shows them only when the compile fails. Pass the listener, as the main compile does, and prove a warning in a test source is printed.

## Read-through

- **Goal.** README, AGENTS.md, `docs/self-host.md`, [next.md](next.md), [publishing.md](publishing.md), and [release-work-plan.md](release-work-plan.md) read against the code at the tip. Each work plan is groomed to match, or marked as history. Anything found is recorded in the ledger as a miss.
- **Closes.** Ledger row 44: `cache-work-plan.md` still checks "TestStore" with the deleted `ModuleTestQuickSpec` as its proof.

## For Russ, outside the plan

- **Scala Native 3.10.1.** `api`, `wire`, and `client` stay on Scala 3.10.0 until a Scala Native nightly publishes `scala3lib` and `nscplugin` for 3.10.1. Moving them changes `ToolCatalog`'s Native revision, which needs a pin move.
- **heddle.** Server shutdown polls a busy connection every 10 ms on the server's clock. A test that runs a real server on `TestClock` therefore hangs (B11). Waking on idle would remove the polling. Discuss before filing an issue.
- **slayer.** `stub` makes laws spell out lambda parameter types, for example `(fingerprint: Fingerprint) =>`. That is a library ergonomics gap to discuss.

## Pin moves

This is the sequence that worked twice. It needs Russ's yes in that session.

1. Commit the layer. The snapshot coordinate is HEAD.
2. Run `./histos assembly`. Then stop the checkout's `histos.jar server` processes, without a reboot, and run any command to start a fresh server.
3. Run `./histos client`, then copy `dist/histos-client` to `dist/clients/client-macos-aarch64/histos-client`.
4. Run `./histos publish snapshots`, and check that the uploaded `<timestamp>-manifest.json` names the local jar's sha256 and the client's.
5. Set `histos.version` in `build.properties` to the commit channel `0.3.0-<sha12>-SNAPSHOT`. Stop the server again, run one command so the server rewrites `.bsp/histos.json`, and run `./histos workflow --update`.
6. Commit the pin on top of the layer, then stack the next layer on it.
