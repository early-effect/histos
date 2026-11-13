# Scenarios

## Why

A scenario is the building block for checking a build. One content store, one or more checkouts, the tree written in Scala, and an assertion on the values and the files the engine returned. That is how this repository checks Histos against itself: two copies of a project share a cache, a clean of one restores from that store, and a skip that hides a real change fails. The same shape is the Histos side of a comparison with another build tool. sbt and zipx stay in the bench. The scenario is what that comparison stands on.

sbt scripted is a text file of `>` and `$` lines because an sbt plugin has no other seam. The only honest test is to launch sbt. Histos is a library. The engine already runs in the test process. A command script would throw away the typed result and then grep the log for `cached`, which is how a wrong skip becomes a pass.

The stamp test that #132 deleted was already a scenario. The copying is the problem, not the missing script language.

Scenarios live in this repository's tests. [Scenario](../engine/test/Scenario.scala) is the code. [ScenarioSpec](../engine/test/ScenarioSpec.scala) is the proof. The same Scala API is what a plugin author gets later, when someone outside this repo needs it. Nothing is published in this work. There is no second language.

## What a scenario is

A scenario is a ZIO test. It opens one content store, then one or more checkouts. Scala writes the tree. The test asserts values and files: `CompileFeed`, `TestFeed`, digests, compiler starts, linker starts. A log line is not the oracle.

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

[JvmCompile.sourceDigest](../engine/src/histos/zinc/JvmCompile.scala) hashes each source's path relative to the checkout, then its bytes. The checkout's own path is not in the row. Two checkouts of the same tree share a compile hit, and the second does not start the compiler. A wiped checkout of either one restores from that store. A source outside the checkout is `CompileError.Outside`. Absolute paths that Zinc or the compiler embed in outputs stay out of the key.

A file with the same repo-relative path and the same bytes is the same input, wherever the checkout sits. Recall and the compile both call `sourceDigest`, and [CellTools.key](../engine/src/histos/zinc/CellTools.scala) is still the one function that builds the key.

## Checklist

Finish one item and prove it before the next. The proof is the suite the item names.

- [x] **Scenario.** #132 deleted the harness. It is back on today's engine. `Scenario.open` is one store and a compiler-start count. `checkout(build)` is a work tree whose lock pins that build's cells and this repository's locked tools. `write` puts a file in it, and `clean` is `./histos clean` there. `compile` and `test` run that checkout against the scenario store. Only `compile` counts starts: a test run's compiles keep their own count. On that engine two checkouts of the same bytes missed, because the source row included the absolute path, and the wiped checkout restored without a third compiler start. A quick hit stays in that checkout: the second checkout stores the same fingerprint and still runs, and that fingerprint is not a blob. Proof: `./histos testOnly --module engine 'histos.ScenarioSpec'` (2 passed).

- [x] **Stamp as a scenario.** The upstream comment and the new public method from the `ModuleTestQuickSpec` that #132 deleted (`git show ba3297b:engine/test/ModuleTestSpec.scala`) run through `Checkout.test`. No suite proves that law today, so the scenario spec is its proof. A second checkout of the same test is not cached. Done, with no production change. Both laws run `core`, which holds `Lib` and `Other`, and `app`, whose suite `demo.Foo` calls `Lib` and never names `Other`. A trailing comment on `Other` recompiles it (its class bytes move) and keeps the API digest, so the next quick run is cached and the record stays. A new public method on `Other` moves the API digest, so the suite runs and stores a new stamp. A second checkout of the same tree runs although the first checkout's record matches, stores the same fingerprint, and that fingerprint is not a blob. The per-checkout law of the item above now runs this two-module test. Both laws were green on today's engine, and each goes red on a broken one: without the `api` lines in the stamp, the suite is skipped after the new method. A stamp of every class in place of the referenced closure reruns it after the comment. A record also kept by digest outside the checkout caches the second checkout. Proof: `./histos testOnly --module engine 'histos.ScenarioSpec'` (9 passed). Two more laws run the same test: a trailing comment in `Lib`, which the suite calls, keeps the stamp, and a body change in `Lib` moves it, a path constant's directory included. The first was red, a product bug ([test-audit.md](test-audit.md) B33), and both are green on the position-free stamp (11 passed).

- [x] **Repo-relative sources.** The source row hashes the path relative to the checkout, then the bytes. Two checkouts of the same tree share a compile hit and the second does not start the compiler. A wiped checkout of either one still restores. Done:
  - `ScenarioSpec` "a second checkout of the same bytes is a hit, and the wiped checkout restores without a start": the keys match, the second compile is not a miss, the class is on disk, the wipe removes it, the recompile restores it, and the compiler starts once.
  - `ZincCompileLawSpec` "the source row is the path relative to the checkout, then the bytes": two directories with the same relative path and bytes hash equal, list order does not change the digest, a different relative path does, and a file outside the checkout is `CompileError.Outside`.
  - Recall and compile both call `JvmCompile.sourceDigest`. [CellTools.key](../engine/src/histos/zinc/CellTools.scala) is still the one key. The checkout path is not a row.
  - `./histos test api wire central engine app`: api 196 and wire 247 on JVM and Native, central 86, engine 720, app 213. No clock warning. `fmt --check`, `workflow --check`, and `run doc` are clean.

- [x] **Link cache.** A native link whose key is in the scenario store is restored, and the linker child does not start. A trailing comment keeps the binary and the start count. A method body relinks. A path constant, `/api/v1/users` against `/api/v2/users`, relinks. A wiped checkout compiles back from the store and the next link restores the last binary with the start count unchanged. `Checkout.link` counts those starts. The clock on that one law is live because the timer is clang and the Scala Native linker. Proof: `./histos testOnly --module engine 'histos.ScenarioSpec'` (16 passed).

## Out of scope

A command script. A subprocess per scenario. Dogfood inside a scenario. A published plugin testkit. The bench repository.
