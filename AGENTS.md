# Agents

Use zio-archtect agent ethos when working on or around this project.

Standing rules for this repository. They apply to this agent and to every subagent. Read this file before editing.

## Why this exists

Histos exists because compile, package, and link are expensive, and sbt does not skip them reliably. zipx made that tolerable. It is not the goal. The goal is a fast functional and idiomatic build tool with a cache Russ can trust on his machine and in CI, where he picks local or remote and he picks how a result is pinned, and the tool does the expensive work only when the code it depends on changed. Tests of unchanged code run only when he asks for the full run. `testQuick` fulfills that ask - only testing what changes impact. A result that is wrong, or a skip that hides a real change, is a failure even when the command was fast.

The proof is a bench that builds the same graph with sbt and zipx and with Histos. Histos has to be faster where a change should not rebuild the world. Cold may tie. Caching Node, browsers, and the dependency resolve is part of the want and is not the gate. Mill is not in the comparison.

A scenario is the building block for checking proofs about histos itself inside this repository. One store, several checkouts, the tree written in Scala, the oracle a value or a file. The decisions are [docs/scenario-work-plan.md](docs/scenario-work-plan.md). Read that file before a test that copies a project, shares a cache across checkouts, or decides a skip. Dogfood of this checkout stays `./histos test`. A scenario does not copy this checkout and does not run sbt. The bench still owns the other build tool and the clock. Do not check a bench box because a scenario passed.

The decisions and the checklist are [docs/cache-work-plan.md](docs/cache-work-plan.md). Read that file before changing cache, compile identity, link, package, `testQuick`, or the generated workflow. Finish one checklist item and prove it before starting the next. Do not check a box the proof has not earned. A change that forgets this want is the wrong change.

## Watch the commands

Leave command output on the terminal. Russ reads it and corrects the run from it.

- Do not pipe a command into `tail`, `head`, `grep`, `sed`, or `wc` when that hides the run.
- `tee` is fine when a later step needs the text. The terminal still shows it.
- Pipe only when the stream would print a secret: a key, a token, or a password.

## Pre-release

Histos is brand new - formative and unreleased. A breaking change is expected when the new signature is the honest API. It will change as we discover opportunities and challenges along the way.


- Research hard problems using docs and local checkouts of dependencies either in siblings (early-effect) or ~/projects/forks - if you need to see source code outside this repo - clone that repo into forks.
- Do not keep a shim, an alias, a default parameter, or a second path so an old call still compiles. Fix the call. Make code simpler. Removing complexity is the goal - adding it is counterproductive. We don't want a wall of code.
- A refactor that changes "what the product does" gets an interview with Russ first. Russ's wants decide. Do not pick the product for him.
- Live in the moment - do not assume old comments and/or docs reflect a best practice. They may be obsolete as the product changes - we expect to have to groom them and you should! Every PR at a minimum needs a docs sweep to look for areas that need attention.
- Still prove the change. A test asserts the property, value, the graph, or the typed error. `isSuccess` on a fat effect is not a test that matters.
- Do not commit, push, or open a pull request unless Russ says so in that turn. Humans merge. A green check is not permission to merge.
- Utilize zio-architect sub-agents liberally as Russ values his time - find ways to divide work into individually testable chunks when possible - then review and assemble. Use worktrees when they make sense.
- Histos builds histos and it must be able to do so locally and in ci. This include publishing - don't assume we have to use github workflow.

## How the code is written

- Use the "zio-architect" agent ethos always.
- The service pattern is important! Make sure you apply ZLayers best practices for providing capabilities to histos - and example is artifact publishers (like sona/local etc).
- Scala 3 syntax, braceless, ZIO, ZIO Test. Typed error enums/traits. Opaque values. Exhaustive matches. Effects are values.
- Format code with scalafmt before each commit.
- If you need a library reach for early-effect github org libraries first. For example heddle for http. If you find a feature gap or bug in a library - pause and discuss it - we will likely file a ticket for enhancement or fix and see if waiting for a release of that library is warranted.
- Public APIs pin the hard type first, then take the function. If a call site needs `(x: Foo) =>` or `: Foo` to compile, the builder is wrong.

## ZIO Test

- Use ZIO Test the way the ZIO docs describe it (READ THEM!!!). When a layer, a remainder, or a test service does not compile, read `~/projects/forks/zio/docs/reference/test/` and `~/projects/forks/zio/docs/reference/di/` before changing a `provide`. Use zio-test idomatically especially test services like Clock,Console etc and refactor any tests you encounter that are not idiomatic.
- `provide` closes an effect. Every service in its environment is an argument, in any order. A missing one is the compiler's `ZLAYER ERROR`: "Please provide a layer for the following type". Add that layer. "You have provided more than is required" is a warning about an extra layer. It is not the failure.
- `provideSome[Remainder]` and `provideSomeLayer[Remainder]` keep `Remainder`. Write the remainder types `type R = A & C` where only B is provided in provideSome. The compiler will not infer it. Refer to the zio documentation on ZLayers.
- A suite shares one resource with `provideShared` or `provideSomeLayerShared`. Leave `TestEnvironment & Scope` (`provideSomeLayerShared[TestEnvironment & Scope]`) so the test clock, console, random, system, and the suite scope stay. Do not replace those with a live layer unless the aspect asked for the live service (`TestAspect.withLiveSystem`, and the clock docs before `withLiveClock`).
- A value the test builds (`Ref`, a temp store) is provided on that test when each test must see its own. Do not hide it in the shared suite layer.

## Running Histos

- The proof is `./histos test` on the modules the change touches. `testQuick` is not that proof. `./histos test` with no modules is the full suite.
- Only one `./histos` runs in a checkout. A subagent that must run it uses its own worktree and does not run it here. - Do not open a worktree to dodge a dirty tree - ask Russ what to do with the current tree. We will merge worktrees into the checkout after review and push or PR from the checkout.
- Tests do not call Central, GitHub, or a keyserver. Loopback stands in.
- Read `docs/release-work-plan.md` before changing publish. Finish one checklist item and prove it before starting the next.
- Read `docs/cache-work-plan.md` before changing cache, compile identity, link, package, `testQuick`, or the generated workflow. The same rule: one checklist item, then its proof.
- Read `docs/scenario-work-plan.md` before a test that stands up a project, shares a cache across checkouts, or claims a skip. Finish one checklist item there and prove it. Do not add a command script. Do not shell `./histos` from a scenario. Do not check a bench box because a scenario passed.
