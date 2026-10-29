# Scenarios

## Why

A scenario is how a meta-build change is proved. Write a small project, run it, change a file, run it again, and assert the values the engine returned. The same shape covers a stamp, two checkouts on one cache, and the next feature that is wrong only when a build exists.

sbt scripted is a text file of `>` and `$` lines because an sbt plugin has no other seam. The only honest test is to launch sbt. Histos is a library. The engine already runs in the test process. A command script would throw away the typed result and then grep the log for `cached`, which is how a wrong skip becomes a pass.

The stamp test is already a scenario. The copying is the problem, not the missing script language. A bench that stands up several checkouts against one cache is the same shape.

Scenarios live in this repository's tests. The same Scala API is what a plugin author gets later, when someone outside this repo needs it. Nothing is published in this work. There is no second language.

## What a scenario is

A scenario is a ZIO test. It opens one content store, then one or more checkouts. Scala writes the tree. The test asserts values and files: `CompileFeed`, `TestFeed`, digests, compiler starts. A log line is not the oracle.

The store belongs to the scenario. It is not [CompileReplay.processStore](../engine/src/histos/zinc/CompileReplay.scala), which is one directory for the whole test JVM. Two scenarios do not share blobs. Two checkouts of one scenario do.

`./histos clean` on a checkout deletes that checkout's work tree and leaves the scenario store. The next compile of the same paths restores from the store.

A quick record is a file under that checkout's `.histos/test-quick`. It is not a blob. A hit in one checkout is not a hit in another.

## What stays out

No `test` script. No `$ exists`. No second language.

Dogfood stays `./histos test` of this repository. A scenario does not shell the jar and does not copy this checkout. Only one `./histos` runs here.

Plugin authors are a later customer of this same API. Nothing is published until a plugin cannot call the engine. That is not this work.

The bench repository stays in [cache-work-plan.md](cache-work-plan.md). A scenario is how a Histos test simulates the checkouts that bench will compare. It is not the bench.

## The source path

[JvmCompile.hashSources](../engine/src/histos/zinc/JvmCompile.scala) mixes each source's absolute path into the source row of the compile key. Two checkouts of the same bytes miss. The same checkout, wiped and compiled again, hits, because the paths did not change.

A second checkout sharing a compile hit is the want. It is not the behavior. The checklist item below is the change, and it is a product change: a file with the same repo-relative path and the same bytes becomes the same input, wherever the checkout sits. Do that item on its own, with the scenario spec rewritten from a miss to a hit. Do not slip it into another cache change.

## Checklist

Finish one item and prove it before the next. The proof is the suite the item names.

- [x] **Scenario.** `Scenario.open` is one store and a compiler-start count. `checkout` is a work tree. `write` puts a file in it. `compile` and `test` run that checkout against the scenario store. Two checkouts of the same bytes miss, because the source row still includes the absolute path, and the wiped checkout restores without a third compiler start. A quick hit stays in that checkout. Proof: `./histos testOnly --module engine 'histos.ScenarioSpec'` (2 passed).

- [ ] **Stamp as a scenario.** The upstream comment and the new public method from [ModuleTestQuickSpec](../engine/test/ModuleTestSpec.scala) run through `Checkout.test`. The quick spec stays the proof until that move, then the scenario spec is the proof. A second checkout of the same test is not cached.

- [ ] **Repo-relative sources.** The source row hashes the path relative to the checkout, then the bytes. Two checkouts of the same tree share a compile hit and the second does not start the compiler. A wiped checkout of either one still restores. Proof: `histos.ScenarioSpec`, with the miss assertion replaced by the hit.

## Out of scope

A command script. A subprocess per scenario. Dogfood inside a scenario. A published plugin testkit. The bench repository. Changing the source digest before its own item.
