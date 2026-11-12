# Scenarios

## Why

A scenario is the building block for checking a build. One content store, one or more checkouts, the tree written in Scala, and an assertion on the values and the files the engine returned. That is how this repository checks Histos against itself: two copies of a project share a cache, a clean of one restores from that store, and a skip that hides a real change fails. The same shape is the Histos side of a comparison with another build tool. sbt and zipx stay in the bench. The scenario is what that comparison stands on.

sbt scripted is a text file of `>` and `$` lines because an sbt plugin has no other seam. The only honest test is to launch sbt. Histos is a library. The engine already runs in the test process. A command script would throw away the typed result and then grep the log for `cached`, which is how a wrong skip becomes a pass.

The stamp test that #132 deleted was already a scenario. The copying is the problem, not the missing script language.

Scenarios live in this repository's tests. [Scenario](../engine/test/Scenario.scala) is the code. [ScenarioSpec](../engine/test/ScenarioSpec.scala) is the proof. The same Scala API is what a plugin author gets later, when someone outside this repo needs it. Nothing is published in this work. There is no second language.

## What a scenario is

A scenario is a ZIO test. It opens one content store, then one or more checkouts. Scala writes the tree. The test asserts values and files: `CompileFeed`, `TestFeed`, digests, compiler starts. A log line is not the oracle.

The store belongs to the scenario. It is not [CompileReplay.processStore](../engine/src/histos/zinc/CompileReplay.scala), which is one directory for the whole test JVM. Two scenarios do not share blobs. Two checkouts of one scenario do.

A checkout keeps its compilers and its input memo between compiles and test runs, the way that checkout's server does. A law that edits a file between two compiles sees the memo the daemon would hold.

`./histos clean` on a checkout deletes the products under that checkout's `.histos`, drops the memo and the compilers, and leaves the scenario store. The next compile of the same paths restores from the store.

A quick record is a file under that checkout's `.histos/test-quick`. It is not a blob. A hit in one checkout is not a hit in another.

## What stays out

No `test` script. No `$ exists`. No second language.

Dogfood stays `./histos test` of this repository. A scenario does not shell the jar and does not copy this checkout. Only one `./histos` runs here.

Plugin authors are a later customer of this same API. Nothing is published until a plugin cannot call the engine. That is not this work.

The bench stays in [cache-work-plan.md](cache-work-plan.md). It runs sbt and zipx and it keeps the clock. A scenario is the Histos column of that comparison, and the way this repository checks several instances of one project against one cache. The bench items stay unchecked until that repository exists. A scenario does not run sbt.

## The source path

[JvmCompile.hashSources](../engine/src/histos/zinc/JvmCompile.scala) mixes each source's absolute path into the source row of the compile key. Two checkouts of the same bytes miss. The same checkout, wiped and compiled again, hits, because the paths did not change.

A second checkout sharing a compile hit is the want. It is not the behavior. The checklist item below is the change, and it is a product change: a file with the same repo-relative path and the same bytes becomes the same input, wherever the checkout sits. Do that item on its own, with the scenario spec rewritten from a miss to a hit. Do not slip it into another cache change.

## Checklist

Finish one item and prove it before the next. The proof is the suite the item names.

- [x] **Scenario.** #132 deleted the harness. It is back on today's engine. `Scenario.open` is one store and a compiler-start count. `checkout(build)` is a work tree whose lock pins that build's cells and this repository's locked tools. `write` puts a file in it, and `clean` is `./histos clean` there. `compile` and `test` run that checkout against the scenario store. Only `compile` counts starts: a test run's compiles keep their own count. Two checkouts of the same bytes miss, because the source row still includes the absolute path, and the wiped checkout restores without a third compiler start. A quick hit stays in that checkout: the second checkout stores the same fingerprint and still runs, and that fingerprint is not a blob. Proof: `./histos testOnly --module engine 'histos.ScenarioSpec'` (2 passed).

- [x] **Stamp as a scenario.** The upstream comment and the new public method from the `ModuleTestQuickSpec` that #132 deleted (`git show ba3297b:engine/test/ModuleTestSpec.scala`) run through `Checkout.test`. No suite proves that law today, so the scenario spec is its proof. A second checkout of the same test is not cached. Done, with no production change. Both laws run `core`, which holds `Lib` and `Other`, and `app`, whose suite `demo.Foo` calls `Lib` and never names `Other`. A trailing comment on `Other` recompiles it (its class bytes move) and keeps the API digest, so the next quick run is cached and the record stays. A new public method on `Other` moves the API digest, so the suite runs and stores a new stamp. A second checkout of the same tree runs although the first checkout's record matches, stores the same fingerprint, and that fingerprint is not a blob. The per-checkout law of the item above now runs this two-module test. Both laws were green on today's engine, and each goes red on a broken one: without the `api` lines in the stamp, the suite is skipped after the new method. A stamp of every class in place of the referenced closure reruns it after the comment. A record also kept by digest outside the checkout caches the second checkout. Proof: `./histos testOnly --module engine 'histos.ScenarioSpec'` (9 passed).

- [ ] **Repo-relative sources.** The source row hashes the path relative to the checkout, then the bytes. Two checkouts of the same tree share a compile hit and the second does not start the compiler. A wiped checkout of either one still restores. Proof: `histos.ScenarioSpec`, with the miss assertion replaced by the hit.

## Out of scope

A command script. A subprocess per scenario. Dogfood inside a scenario. A published plugin testkit. The bench repository. Changing the source digest before its own item.
