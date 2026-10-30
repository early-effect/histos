# Test overhaul

One item, then its proof. Slayer, Compiler, Process, Machine, Linkers, the trait audit, the scenario helper, the algebra, the compile laws, and the test-run laws are done. This file is the checklist.

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

- [x] **Cache and compile laws, fresh.** From the why. The service under test is `GraphCompile`. `Compiler` is `stubbedTraced`. The store is stubbed when the law is about whether we call, and live when the law is about restored bytes. A comment compiles the edited module and does not compile a dependent or an unrelated module. Source bytes are in that module's key, so the edited module is a miss. A public signature calls the dependent and not an unrelated module. A hit does not call. A miss names the input that changed. Clean leaves the store and the next compile restores. One law, one test. Proof: `./histos testOnly --module engine 'histos.zinc.CompileLawSpec'`. No scalac. The restore law is a stubbed compiler and a live store. The old compile and zinc specs are deleted. Laws they circled that this suite does not state are on **Zinc boundaries** and **Live compiler and store**.

- [x] **Test-run laws, fresh.** A quick record lives in the checkout. A second checkout of the same bytes is not a hit. A failure forgets the record. A pattern selects by name. The service under test is the test runner. `Process` is `stubbedTraced`. No provider child. Proof: `./histos testOnly --module engine 'histos.testrun.TestLawSpec'`. No spawn. `engine/test/ModuleTestSpec.scala` and `engine/test/QuickSpec.scala` are deleted. An absent record runs the suite, a hit does not, and a cleared record runs it again: those are the laws above. The rest of what those files circled is on **Counted children**, **Zinc boundaries**, **Live compiler and store**, **App, central, publish, BSP**, and **Stamp as a scenario**.

- [ ] **Counted children.** One suite. The service under test is live `Process`, or the live linker, or the live assembly runner. Every other service is stubbed. Live clock. `Requires` where a tool may be absent. The allow-list is the one above. A compiled 2.13 main runs. That spawn is not on the allow-list. Stop and ask before adding it. This is the only suite allowed to spawn, and the only suite allowed `withLiveClock` or `withLiveSystem`. Delete every other `ProcessBuilder` and `ChildProcess.run` in tests. Proof: that suite, then `./histos test engine`.

Also state, with a live child: a named main runs against main classes, and a module with nothing to run is not a pass. A named js main uses only its overridden linker revision and chosen Node. A js main with no Node to choose is a typed failure before the link. A named native main links and runs the binary. A non-zero main is a failed cell and its output is kept. ZIO Test is the default provider, and a failing spec is not a pass. A suite pattern runs the matching mains. A suite pattern runs one provider suite. A pattern that matches nothing fails the run. A js provider links the Scala.js bridge and runs on node, under jsdom, and under firefox. A native provider links TestMain and runs the binary.

A compiled 2.13 main is not in this suite. Stop and ask before adding it.

These are not on the allow-list, so they are not in the suite. A child the main left running dies with the group when the main returns. Closing a command kills the grandchild it started, and that grandchild is in the command's group. A large output arrives whole. Interrupting the bootstrap compile kills javac. Ask before adding any of them.

A named js main on a linker revision other than the catalog is not in this suite. The offline cache has the catalog linker, and an empty stand-in jar is not a link. The live child proves the chosen Node.

A js provider on node, under jsdom, and under firefox, and a native provider that links TestMain, are still this item. They are not in the suite yet.

`MainClass.from` accepts `demo.Exit` and `demo.Exit$`, and refuses empty and `1bad`. The suite environment keeps `HISTOS_REPO`, `PATH`, `HOME`, and a non-empty `XDG_CACHE_HOME`, and drops every other variable.

An in-flight run keeps one lock generation across compile and launch. An explicit platform runs only that platform. Those two spawn, and they are not on the allow-list.

N1 runtime cache and the launcher updater reject an owned junction behind a shared physical parent. The junction is `mklink /J`. That spawn is not on the allow-list. It does not run on macOS or in CI.

`BazelRemoteSpec` starts docker, `LauncherPinSpec` starts one Java child, and `BootstrapSpec` starts the shell script. Those spawns are not on the allow-list. Ask before folding any of them into this suite. The box stays open while they remain, and while a js provider and a native provider are still absent.

- [x] **Zinc boundaries.** No scalac and no spawn. The service under test is the pure function, or `GraphCompile` with `Compiler` stubbed. One law, one test. These are the laws the deleted compile and zinc specs circled that are not compile-call laws.

Layout and refusals. One module takes sources at the repository root. A module directory wins over that shared root. Sources may live in a directory that is not the module name. A missing lock is refused. A module with no source root is refused. An empty source root is refused. Tests alone compile an empty main, and another platform's tests do not. `src/test` is the suite when the module has no main. A single module uses `test` at the repository root. Test sources without a provider library are refused. A named main without test sources is refused.

Test store, still without scalac and without a spawn. The service under test is the test runner with `Process` stubbed. `Off` is the default. `Remote` with a need does not load, and `Local` with a need does. `test` on `Off` launches again after a passing run. `test` on `Local` skips when the stored suite digest still matches. `test` on `Remote` launches again and does not skip.

Suite stamp and the quick record, still without scalac and without a spawn. One class index stamps a suite the same way. A stored success matches only that digest. A referenced class is in the digest and an unreferenced class is not. Upstream jars, the JDK, the flags, and an API digest each change the digest. A stored success misses when the upstream jar, the JDK, or the flags differ. A class file that cannot be parsed is a stamp error. An unreadable class directory is a stamp error. A missing upstream API file adds no line, and an unreadable file is a gap. A ZIO spec is chosen and a helper is not. An empty suite name is not a suite. A quick record that is not a stored success is an error. A quick record whose digest is not a sha256 is an error. A failed save is the write error and a failed clear is the delete error. A digest mismatch runs the suite. An unreadable quick record fails the run. A suite with no digest runs.

Scheduling, still stubbed. The console finishes an upstream cell before the module that depends on it. A cell may start while an unrelated upstream is still compiling.

Classpath. Runtime, then the lock jars without a second copy of the standard library, then the class directory even when it is not there yet. An upstream built for another Scala version is not on the classpath. A js-only upstream is on the js classpath and not the jvm one. The compile key mixes an upstream's API digest, not a hash of that upstream's class bytes.

Analysis. A missing analysis is every current source, and an unreadable file is unknown. A matching stamp is fresh, a changed source marks every dependent, and a new file is stale. A member-ref child and the grandchild two hops away are stale when the parent changes. Reading the incremental surface leaves a fixture class file and writes no `inc.zip`. That surface exposes `compile` and `compileAllJava`.

Bytes. A line number and an absolute path are not part of the class digest. A path leaf that contains `$` still hashes. Bytes that are not a class file are refused. Jar entry timestamps are not part of the digest. A directory of classes returns both shapes sorted by name. A short class file is an error. A missing directory is empty. A jar returns the class shape. A Scala 3.3 lazy val becomes a VarHandle and still initializes. The assembly writes that class. The LazyVals runtime stays, and a stray call is refused. A short window or a non-string ldc does not throw. A rejected frame computation is a typed error. A companion offset can publish a VarHandle for the module's private field.

Sources, still without scalac. Scala 2.13 keeps its directories and drops Scala 3 and other platforms. A package named `native` is shared, and `src/main` filters still apply. A sources jar entry is the package path. The engine classpath cannot see the Scala 2 compiler. A plugin marker is banned in a jar and in a classes directory. Loader leases close when later construction fails.

Proof: `./histos testOnly --module engine` on the new suite.

- [ ] **Live compiler and store.** A handful of scenarios. The services under test are live `Compiler` and live `ContentStore`, and the test says why both are live. Everything else is stubbed. Shared toolchain. One store per test. A real compile. A type error is a typed failure. A wiped checkout restores and does not start the compiler again. These do not re-prove the stubbed laws.

Also state, with the live compiler: a cached test success cannot bypass a corrupt selected compiler. A 2.13 cell writes semanticdb and compiles its sources. Scala 3 and 2.13 reach the compiler. Scala.js and Scala 2.12 do not. Scala 3.10 compiles a named tuple that Scala 3.3 rejects. A js module compiles to sjsir, and its tests are not on that classpath. A comment and a method body keep the public signature, and a new parameter moves it. A comment keeps the product digest, and a method body moves it. A public descriptor change invalidates the downstream class. Flags, the Scala version, the platform, the JDK, artifact checksums, locked tool bytes, and the local runtime identity are part of the key. Unused tools are not. A corrupt tool is refused before a hit. Inside one compile, an unchanged class stays, a neighbor of a changed source is rebuilt, and a removed source deletes its class. A compile diagnostic is on the console before the cell fails. A declared compiler is a library, and a histos api jar is not. Two compiles on one pool open the compiler once. User code cannot see the compiler. A project compiler does not load the engine Scala library or compiler. Compiled code cannot see the compiler, reflect, jline, or diffutils.

Proof: `histos.ScenarioSpec` and `./histos test engine`. Budget: this file is the compiler-start budget for the engine suite.

- [ ] **App, central, publish, BSP.** Same rule. Name the service under test. Stub the rest with slayer. CLI parsing is algebra. A command's effect is a call on a service. A saved file changes the source snapshot and an untouched tree does not. That service is `SourceWatch`. BSP protocol is bytes, in process. One sentinel socket if, and only if, the socket is the service under test. Publish and portal talk to loopback, never Central, GitHub, or a keyserver. `Signer` is stubbed, unless the invariant is the live adapter's exit mapping, in which case that one test joins the counted-child allow-list and Russ is asked first. A real gpg signs and the public half verifies: that is the exit-mapping test, and it waits for that ask. The runtime jar prints the version text. That spawn is the allow-list's one `java -jar` and the engine suite already runs a stand-in jar, so this checkout's jar is not a second one. The jar still has to contain `histos/cli/Main.class`, name that main in the manifest, and contain no `scala/scalanative/` entry. A second process promotes and drops a held deployment on disk. That spawn is not on the allow-list. Delete the old specs as their laws land. Proof: `./histos test app` and `./histos test central`.

- [ ] **Stamp as a scenario.** Stop and reconfirm with Russ. Then the upstream-comment and new-public-method laws run through `Checkout.test`. A second checkout of the same test is not cached. A stored success for one suite does not skip a different suite. A referenced class change reruns that suite and an unreferenced class does not. Selected JDK content is in the stamp and an unrelated tool is not. `engine/test/ModuleTestSpec.scala` and `engine/test/QuickSpec.scala` are already deleted. The scenario spec is the proof.

- [ ] **Repo-relative sources.** Stop and reconfirm with Russ. Then the source row hashes the path relative to the checkout, then the bytes. Two checkouts of the same tree share a compile hit and the second does not start the compiler. A wiped checkout of either one still restores. Proof: `histos.ScenarioSpec`, the miss assertion replaced by the hit. This item does not ride along with any other.

- [ ] **Doc sweep.** Read the README, `Agents.md`, and `docs/` against the code that now exists. Fix any how that still describes a deleted call site. Do not edit a why paragraph. Where the why and the code disagree, stop and ask Russ. Proof: the files read, the edits named, no behavior change.

## Budget

Warm `./histos test` in seconds. Cold under a minute, and that minute is the tests whose live service is the compiler, the store, or a child. A timeout measured in minutes is a failed item. The full `./histos test` is the last proof of the children item, the live compiler and store item, and the app and central item. It is not the proof of a stubbed miniature.

## Out of scope

A virtual `java.nio`. A hand-rolled stand-in for a trait slayer can stub. Vendoring slayer, or keeping `io.github.russwyte::slayer` once the early-effect release exists. A published Histos testkit. A command script or a second language. Dogfood inside a scenario. Shelling `./histos` from a test. Copying this checkout. The bench, sbt, and zipx. Editing heddle. Porting an old test line by line. A shim so an old call site still compiles. Checking a bench box because a scenario passed. Rewriting a why paragraph without Russ. Providing a second live service without saying why the invariant needs both.
