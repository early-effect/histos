# Test overhaul

One item, then its proof. Slayer, Compiler, Process, Machine, Linkers, the trait audit, the scenario helper, and the algebra are done. This file is the checklist.

## Why

Compile, package, and link are expensive. They run only when the inputs they depend on changed. Russ picks local or remote, and he picks how a result is pinned. A wrong result, or a skip that hides a real change, is a failure even when the command was fast. Unchanged tests run only when he asks for the full run.

The suite we have does not serve that. It is about 106 specs and about 35,000 lines. The large files each invent a temp tree, write a tiny source file, and pay for a real compiler, a real child, or the wall clock. A multi-minute timeout is fear, not a budget. The helpers were never the product, so every spec grew its own copy.

The laws are not in those files. The files reached through to Zinc, processes, and the clock because nothing stood in between. Starting fresh means reading the invariant off the product and writing the smallest test that fails when the invariant breaks. An old assertion is not evidence. If the invariant is not obvious from the product, we stop and ask Russ. We do not open the old spec to copy it.

This project is unreleased. A breaking change that makes a boundary honest is expected. No shim, no second path, no compatibility wrapper for a call site we are deleting.

## Decisions

- Every spec file in api, engine, app, and central goes. The replacement is a new test of an invariant, not a port.
- Every capability we do not own sits behind a service trait. Zinc is the example, not a special case. The same rule covers the other compilers, process spawn, and the rest of the list below.
- A test names one service under test and provides that service's live layer. Every other service is a fast stand-in. Real Zinc, a real child, a real linker, and a real store are allowed. They are allowed when that service is the one the invariant is about, and only then.
- The stand-in is slayer, used the way `TestClock` stands in for `Clock`. `stubbed` and `stubbedTraced`. No hand-rolled fake of a trait slayer can stub. A handler returns the method's value or its typed error, so a failure path is injected without the live service failing. A `Gen` picks the request and which failure, and `TestRandom` keeps the draw replayable. Slayer moves into early-effect. Its build stays sbt and zipx until a later item moves it onto Histos. Its Scala is 3.9. A missing stubbing capability is a pull request and a release of that repo, then a pin bump here. Do not copy it into this repo.
- `./histos test` starts scalac only in tests whose service under test is the live compiler, or the live compiler together with the live store. Child processes start only in tests whose service under test is the live process. Live clock only on that list. Warm run in seconds. Cold run under a minute. No single test approaches ten seconds except a named child.
- "Stamp as a scenario" and "repo-relative sources" are the last two product items on this list. They change product behavior. Each one is its own proof. Each one stops to reconfirm the behavior with Russ before the code moves. They moved here from [scenario-work-plan.md](scenario-work-plan.md).
- Docs, the README, and `Agents.md` are archaeology of an earlier how. The why in them is Russ's and stays until he changes it. The how is rewritten in the item that makes it false. A doc sweep with no code change is not a checklist item.

## Service rule

A trait is the capability we use. Its methods speak our types and our error enum. The live layer is the only file allowed to import the foreign library or call the OS. A test provides the live layer for the service under test, and a slayer stub for every other service.

The trait is shaped so slayer can stub it. It is a trait. Methods are `def`, never an abstract `val`. No `final` methods. No `var`, no type member, no self type. Overloads have distinct erased signatures. An unstubbed abstract method raises slayer's `NoStub` and fails the test. We do not catch it. Concrete `def`s with a real body may fall through. Prefer an abstract `def` when the live behavior must not run by accident.

ZIO `Clock`, `Console`, `Random`, and `System` are already those services. `java.lang.System.nanoTime`, `java.lang.System.getProperty`, and `System.getenv` are reach-throughs. They move onto the ZIO service. We do not invent a second clock.

A pure function on our data (a key, a stamp, a glob, a graph) is not a service. Wrapping it in a trait adds a seam with nothing behind it.

`Resolve`, `ContentStore`, `Loader`, `Signer`, `RemoteCache`, `ActionCache`, `Assets`, `Tasks`, `Histos`, and `PluginRuntime` are already traits. An item for one of those is an audit: foreign types do not escape the live layer. If they do, the signature changes. Call sites move. No overload kept for the old shape.

`java.nio` stays inside `Workspace` and the live `ContentStore`. We do not invent a virtual filesystem. `ContentStore` is still a service: when it is not the service under test, it is a slayer stub. When it is, the live layer uses a real directory. Specs do not call `Files.createTempDirectory`. A checkout comes from `Scenario`, and only a test whose invariant is about a checkout opens one.

`GraphCompile` is our orchestration. It is not the Zinc trait. It takes `Compiler` in its environment. A hit does not call `Compiler`. When `GraphCompile` is the service under test, `Compiler` is `stubbedTraced` and the call history is the count.

## Boundaries

Must become a trait. The live layer owns the import. One item each, in this order, because later ones sit on earlier ones.

- `Compiler`. Request, output, typed compile error. `sbt.internal.inc.Analysis` does not appear outside `engine/src/histos/zinc/`. That pulls `JvmCompile`, `CompilerPool`, `ApiDigest`, `SourceStale`, and `CompileReplay` behind the trait. Scala 2.13 and Scala 3 are two requests to the same trait, not two traits.
- `Process`. `events`, `command`, `run`, with today's `ChildRequest`, `ChildEvent`, `ChildOutcome`, `ChildError`. `ChildProcess` becomes the live layer. `ProcessBuilder` stays inside it.
- `Linker` for Scala.js. "Link these inputs to this script." The live layer may spawn, and it is the only place that knows the linker classes. A comment that must not relink stubs the linker and reads the call history. It does not spawn.
- `NativeLinker` for Scala Native, including clang. Same shape. Clang's version probe is a method on this trait or on `Process`, not a raw `ProcessBuilder` in `Clang.scala`.
- `Node` is already an object that probes by spawning. It becomes a trait, or it becomes a function over `Process` if the only capability is "run this argv and parse the version." Pick the function when the trait would only forward. Ask Russ if both shapes still look honest at that item.
- `Machine`. JDK home, cache home, OS, arch, as a value. The live layer reads `zio.System` once. Tests provide the value. `withLiveSystem` is not the default aspect.

Already a trait, audit only, foreign types stay inside the live layer:

- `Resolve` (Coursier)
- HTTP. `heddle.client.Client` is the HTTP service. Cache code takes `Client`. We do not wrap heddle in a second trait. Heddle types do not leak into domain results. Do not edit the heddle clone.
- `Signer` (gpg)
- `RemoteCache` and `ActionCache`
- `Loader` and the build-file compiler (`BuildCompiler`). Compiling `histos.scala` is a `Compiler` request or a `Loader` detail. It is not a third compiler trait unless the request type is actually different. Ask Russ if the item cannot tell.

Not a trait: the filesystem, ZIO's clock and console, pure keys and stamps.

## The service under test

A test names the service whose live behavior it proves, and provides that live layer. Every other service is a fast stand-in. That is the same rule as `TestClock`: time is not what the test is about, so the clock does not tick on the wall.

slayer is the stand-in for our traits. `stubbed[S]` builds the layer. `stub[S](_.method(any[A]))(handler)` sets one method. `stubbedTraced[S]` records `Call(MethodId, args)`. Branch on arguments inside the handler when two calls differ. The last stub for a method wins. There is no matcher DSL and no times DSL. Call history is the assertion.

A stub is not a canned success. The handler's type is the method's type, so it returns the value or fails with that method's error enum. That is how the service under test is shown to cope with a dependency that misses, fails, or returns an awkward value, without starting Zinc, a child, or the network to produce the failure. `Gen` / `check` chooses the request and the failure. `TestRandom` is the random source, so a failure replays. Live `Random` is not. A thrown exception is not a failure we inject. If the law is "any compile error becomes this typed error," the generator produces the error enum and the stub fails with it. One property, not one test per constructor.

The robust suite comes from that combination: one live service, stubs that misbehave on purpose, and generators over the inputs and the errors. A test that only stubs the happy path is unfinished.

Slayer is not a Histos test helper we paste in. It is a library. [early-effect/slayer](https://github.com/early-effect/slayer) is sbt and zipx, Scala 3.9.0, `rocks.earlyeffect::slayer`. The old `io.github.russwyte` coordinate is dropped. A Histos build of this library is a later item.

- The GitHub repo is `early-effect/slayer`. The published coordinate is `rocks.earlyeffect::slayer`.
- The build stays sbt and zipx. zipx writes the plugin list and the release workflow. `sbt testFull` is how slayer is tested in this item. Scala is 3.9.0, the latest 3.9. Histos itself stays on 3.10.0. Slayer on 3.9 is the point: a real library on the Scala line Scala.js forces. Moving the build to `histos.scala` is not this item.
- It is a test dependency of Histos. It is not on the published Histos classpath.
- The macros were compiled for an older Scala. The item that adds the dependency proves a stub expands in Histos's 3.10.0 test compile. If a 3.9 build does not expand here, the fix is a change to slayer, released, then pinned. It is not a silent bump of slayer to 3.10, and it is not a vendored copy.
- When a Histos trait cannot be stubbed, slayer grows the capability. That is a pull request on `early-effect/slayer`, a release, and a pin bump in this repo. Those are separate from the Histos behavior change they unblock. He has already said that path is open. A slayer API change that is not forced by a Histos stub still stops and asks. The work runs in the slayer clone. If that clone is missing or dirty, stop and ask. Do not clone a second copy to dodge it.

Two live services means the invariant is their interaction, and the test says so. A wiped checkout that restores is live `Compiler` and live `ContentStore`, because the invariant is that those two agree. `Process`, `Resolve`, `Linker`, and `Signer` stay stubbed in that test. A third live service is a reason to split the test. Booting the resolver and a child in order to learn that a comment does not compile is the suite we are throwing out.

- Skip on a comment. The service under test is `GraphCompile`. `Compiler` is traced and not called. No scalac.
- Zinc compiles one cell. The service under test is live `Compiler`. The store is live only when the output is stored bytes. Everything else is stubbed.
- Cancel kills the process group. The service under test is live `Process`. `Compiler` is stubbed. This test is on the spawn allow-list because its live layer spawns.
- Two checkouts share a hit. Live `Compiler` and live `ContentStore`. That pair is a scenario. `Process` stays stubbed.

## How a test is written

Four layers. The helpers are the product. A spec is a client.

1. Algebra. No clock, no files, no services. `Gen` and `check`. Milliseconds. An example table is a failed property test.
2. One live service, slayer for the rest, `TestClock` for time, `TestRandom` for draws. The stubs succeed, fail, and vary with the generator. A wait is a promise or `TestClock.adjust`. `ZIO.sleep` is not how a test waits for the product. `.timeout` on `TestClock` does not fire. That is not a reason to reach for `withLiveClock`.
3. Scenario. Used when the invariant needs a checkout. One store per test, live only because the store is part of the invariant. The toolchain layer is shared on that suite (`provideSomeLayerShared[TestEnvironment & Scope]`) and is not provided to tests that stub the compiler. The oracle is a value: `CompileFeed`, `TestFeed`, a digest, a file, slayer's call history. A log line is not the oracle. Most tests never open a scenario.
4. Live process. One suite. The service under test is `Process`, or a linker whose live layer spawns. Live clock only here. The allow-list is: `System.exit` stays in the child, cancel kills the process group, one provider discovery with a suite pattern, one Node run, one Native link, one `java -jar` of the assembly. `Requires` ignores a missing tool. Any other spawn means some other service was left live by mistake.

ZIO Test, the way the ZIO docs describe it. `provide` closes. `provideSomeLayer[Remainder]` names the remainder. A value each test must own (`Ref`, a store, a checkout) is provided on that test. The toolchain is the shared layer. No `TestAspect.sequential`. No `assertTrue` on `isSuccess`. No partial accessors. Aspects sit on the suite val.

Heddle's server install sleeps on `Clock` until the next second. One helper runs that install on the live clock inside a test that otherwise stays on `TestClock`. We do not edit heddle, and we do not put `withLiveClock` on the suite.

## Docs

`docs/cache-work-plan.md`, `docs/scenario-work-plan.md`, `docs/release-work-plan.md`, `docs/source-work-plan.md`, `docs/next.md`, the README, and `Agents.md` describe a how that predates this rule. Treat that how as disposable.

Keep the why. It is Russ's motivation: what he wants to be true, and who is hurt when it is false. When a rewrite would change the why, or when two docs disagree about the why, stop and ask. Do not pick a why for him. Do not "improve" his wording.

Rewrite the how in the same item that changes the code. Do not land a docs-only reconciliation first, and do not leave a paragraph that describes a trait we just deleted. The README and `Agents.md` move when an item makes a paragraph false, not in a prelude.

## Checklist

Finish one item and prove it before the next. The proof is the suite the item names, run with `./histos test` on the modules it touches. `testQuick` is not the proof. One `./histos` in this checkout. A box is checked only when that proof is green. An item that changes product behavior stops and asks Russ before the code moves. An item that cannot state its invariant from the product stops and asks. It does not open the old spec.

- [x] **Slayer on early-effect.** Transfer `russwyte/slayer` to `early-effect/slayer`. The build stays sbt and zipx. Scala 3.9.0. Group `rocks.earlyeffect`. The old `io.github.russwyte` coordinate is dropped. Publish a release through zipx. Histos does not take the dependency in this item. Proof: `sbt testFull` in that repo, and the release. This item edits slayer, not Histos.

- [x] **Compiler.** `Compiler` is a trait, shaped so slayer can stub it. Depend on the `rocks.earlyeffect::slayer` release from the previous item, test scope only. Prove a stub expands in this 3.10.0 test compile. If it does not, fix slayer, release it, bump the pin. Do not vendor it and do not bump slayer to 3.10 without asking. The live layer is the Zinc package. `Analysis` does not leave that package. `GraphCompile` calls `Compiler` only on a miss. Proof: one miniature, `GraphCompile` is the service under test, `Compiler` is `stubbedTraced`. A hit records no call. A miss records one. A generated `CompileError` from the stub surfaces as the caller's typed error. No scalac. `TestRandom` only. If the failure's effect on a retry is not forced by the product, ask before asserting it. `./histos testOnly --module engine` on that suite. Old compile specs are not the proof and are not deleted here.

- [x] **Process.** `Process` is a trait. `ChildProcess` is the live layer. Proof: a miniature whose service under test is the caller, `Process` is `stubbedTraced`, exit and stdout are the stub, cancel is a recorded call. No real spawn. `ProcessBuilder` in old tests is not moved yet.

- [x] **Machine and clock.** `Machine` is the JDK, cache, OS, and arch value. The live layer reads `zio.System` once. `Histos` durations come from `Clock`, not `nanoTime`. Proof: a miniature on `TestSystem` and `TestClock`. No `withLiveSystem` and no `withLiveClock` on that suite.

- [x] **Linkers.** `Linker` and `NativeLinker`. Live layers may spawn, and a test provides that live layer only when the linker is the service under test. Proof of the skip law: the linker is `stubbedTraced`, a comment-sized input change records no call, a body change records one. No Node, no clang. Node's probe is a function of `Process` unless that lies, in which case stop and ask.

- [x] **Trait audit.** `Resolve`, `Signer`, `RemoteCache`, `Loader`, `BuildCompiler`, and the HTTP cache. Foreign types stay in the live layer. Heddle stays `Client`. A signature that has to change is a break, fixed at the call site, no shim. Proof: the miniature for each trait that changed, plus `./histos test` on engine for the traits that did not. Ask Russ if a public api type has to change.

- [x] **Scenario helper.** `Scenario` is how a test opens a checkout when the invariant needs one. It is not the setup for every test. `compile` and `test` take services from the environment, so the caller provides live `Compiler` and live `ContentStore` and stubs the rest. Oracles are values, including slayer call history. New clients do not call `Files.createTempDirectory`. Do not migrate old specs onto it. Proof: the existing two scenario laws, rewritten as clients of the finished helper, `./histos testOnly --module engine 'histos.ScenarioSpec'`. Behavior stays today's behavior. The miss across checkouts stays a miss until the repo-relative item. The compiler is live in this proof because the invariant is a real compile against a real store. `Process` is stubbed unless a law in this file is about a process.

- [x] **Algebra, fresh.** Keys, stamps, globs, graph shape, bundle refusals, suite patterns, error enums. Properties, not tables. Write the law from the type. If the law is not forced by the type, ask Russ before writing a test that guesses. Delete the old pure specs in the same item, after the new suite is green. Proof: `./histos test api` and the engine suites this item added.

- [ ] **Cache and compile laws, fresh.** From the why. The service under test is `GraphCompile`. `Compiler` is `stubbedTraced`. The store is stubbed when the law is about whether we call, and live when the law is about restored bytes. A comment does not call the compiler. A public signature calls the dependent and not an unrelated module. A hit does not call. A miss names the input that changed. Clean leaves the store and the next compile restores. One law, one test. Delete the old compile and zinc specs in this item once each law they were circling has either a new test or an explicit "not an invariant, ask answered." Proof: `./histos testOnly --module engine` on the new suite. No scalac, except the restore law if restored bytes are defined as what the live compiler wrote. If that law can be stated on a stubbed compiler plus a live store, do that. Ask Russ if it cannot.

- [ ] **Test-run laws, fresh.** A quick record lives in the checkout. A second checkout of the same bytes is not a hit. A failure forgets the record. A pattern selects by name. The service under test is the test runner. `Process` is `stubbedTraced`. No provider child yet. Delete the old module-test specs that these laws replace. Proof: the new suite. No spawn.

- [ ] **Counted children.** One suite. The service under test is live `Process`, or the live linker, or the live assembly runner. Every other service is stubbed. Live clock. `Requires` where a tool may be absent. The allow-list is the one above. This is the only suite allowed to spawn, and the only suite allowed `withLiveClock` or `withLiveSystem`. Delete every other `ProcessBuilder` and `ChildProcess.run` in tests. Proof: that suite, then `./histos test engine`.

- [ ] **Live compiler and store.** A handful of scenarios. The services under test are live `Compiler` and live `ContentStore`, and the test says why both are live. Everything else is stubbed. Shared toolchain. One store per test. A real compile. A type error is a typed failure. A wiped checkout restores and does not start the compiler again. These do not re-prove the stubbed laws. Proof: `histos.ScenarioSpec` and `./histos test engine`. Budget: this file is the compiler-start budget for the engine suite.

- [ ] **App, central, publish, BSP.** Same rule. Name the service under test. Stub the rest with slayer. CLI parsing is algebra. A command's effect is a call on a service. BSP protocol is bytes, in process. One sentinel socket if, and only if, the socket is the service under test. Publish and portal talk to loopback, never Central, GitHub, or a keyserver. `Signer` is stubbed, unless the invariant is the live adapter's exit mapping, in which case that one test joins the counted-child allow-list and Russ is asked first. Delete the old specs as their laws land. Proof: `./histos test app` and `./histos test central`.

- [ ] **Stamp as a scenario.** Stop and reconfirm with Russ. Then the upstream-comment and new-public-method laws run through `Checkout.test`. A second checkout of the same test is not cached. The quick spec is deleted. The scenario spec is the proof.

- [ ] **Repo-relative sources.** Stop and reconfirm with Russ. Then the source row hashes the path relative to the checkout, then the bytes. Two checkouts of the same tree share a compile hit and the second does not start the compiler. A wiped checkout of either one still restores. Proof: `histos.ScenarioSpec`, the miss assertion replaced by the hit. This item does not ride along with any other.

- [ ] **Doc sweep.** Read the README, `Agents.md`, and `docs/` against the code that now exists. Fix any how that still describes a deleted call site. Do not edit a why paragraph. Where the why and the code disagree, stop and ask Russ. Proof: the files read, the edits named, no behavior change.

## Budget

Warm `./histos test` in seconds. Cold under a minute, and that minute is the tests whose live service is the compiler, the store, or a child. A timeout measured in minutes is a failed item. The full `./histos test` is the last proof of the children item, the live compiler and store item, and the app and central item. It is not the proof of a stubbed miniature.

## Out of scope

A virtual `java.nio`. A hand-rolled stand-in for a trait slayer can stub. Vendoring slayer, or keeping `io.github.russwyte::slayer` once the early-effect release exists. A published Histos testkit. A command script or a second language. Dogfood inside a scenario. Shelling `./histos` from a test. Copying this checkout. The bench, sbt, and zipx. Editing heddle. Porting an old test line by line. A shim so an old call site still compiles. Checking a bench box because a scenario passed. Rewriting a why paragraph without Russ. Providing a second live service without saying why the invariant needs both.
