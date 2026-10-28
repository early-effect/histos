# Agents

Standing rules for this repository. They apply to this agent and to every subagent. Read this file before editing.

## Watch the commands

Leave command output on the terminal. Russ reads it and corrects the run from it.

- Do not pipe a command into `tail`, `head`, `grep`, `sed`, or `wc` when that hides the run.
- `tee` is fine when a later step needs the text. The terminal still shows it.
- Pipe only when the stream would print a secret: a key, a token, or a password.

## Pre-release

Histos is unreleased. A breaking change is expected when the new signature is the honest API.

- Do not keep a shim, an alias, a default parameter, or a second path so an old call still compiles. Fix the call.
- A refactor that changes what the product does gets an interview first. Russ's wants decide. Do not pick the product for him.
- Still prove the change. A test asserts the value, the graph, or the typed error. `isSuccess` on a fat effect is not a test.
- Do not commit, push, or open a pull request unless Russ says so in that turn. Humans merge. A green check is not permission to merge.
- `./histos publish snapshots` is the normal publish. It uploads `histos-api_3` and `rocks.earlyeffect:histos` and does not sign, tag, or open a portal deployment. A non-snapshot release, a public key, a tag, or `publish release` waits until Russ says the feature is complete. The catalog stays `manual("histos", manual"0.0.1-SNAPSHOT")` until then.

## How the code is written

Use the "zio-architect" agent ethos.

Scala 3, braceless, ZIO, ZIO Test. Typed error enums. Opaque values. Exhaustive matches. Effects are values.

- No `Either[String, _]`, no `throw`, no `return`, no `null`, no `var` in a contract.
- No partial accessors: `Option.get`, `.head`, `.tail`, `.last`, `.reduce` on a collection that can be empty, `Map.apply`, `Try.get`.
- `asInstanceOf` stays inside macros where the call already pins the concrete type, and inside Scala Native pointer casts. Anywhere else it is a type bug.
- No Cats, no `Future`, no `Await`, no `unsafeRun` in library or domain code.
- `Task` only at an impure edge, and the edge maps it to a typed error.
- HTTP is heddle (`heddle.client.Client`, from `Client.layer`). Test servers are heddle servers on loopback. Do not edit the heddle clone unless Russ names that repo this turn.
- Codecs stay in `app`. Do not add zio-json to `api` or `engine`.
- CLI and test output goes through `zio.Console`. Not `System.out` or `System.err`.
- A comment states a constraint the code cannot say. No version numbers in comments. No narration.
- Format with scalafmt 3.11.5 at `~/Library/Application Support/Coursier/bin/scalafmt`. `maxColumn` is 120. Run `scalafmt` on the files you change.

Public APIs pin the hard type first, then take the function. If a call site needs `(x: Foo) =>` or `: Foo` to compile, the builder is wrong.

## Running Histos

- The proof is `./histos test` on the modules the change touches. `testQuick` is not that proof. `./histos test` with no modules is the full suite.
- Only one `./histos` runs in a checkout. A subagent that must run it uses its own worktree and does not run it here. Do not open a worktree to dodge a dirty tree, and do not push from it.
- Do not copy `dist/` or `.histos` products from another checkout. `./histos assembly` regenerates the launcher in that tree.
- Tests do not call Central, GitHub, or a keyserver. Loopback stands in.
- Read `docs/release-work-plan.md` before changing publish. Finish one checklist item and prove it before starting the next.
