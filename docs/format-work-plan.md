# Format

## Why

A Histos user should not install Coursier or a scalafmt binary to format a build. `histos fmt` formats, `histos fmt --check` fails a pull request that is not formatted, and the scalafmt it runs is pinned in `dependencies.lock` with a sha256 like every other tool Histos runs. The `version` in `.scalafmt.conf` stays the one source of truth, so Metals, the scalafmt CLI, sbt-scalafmt, and Histos agree.

This file is the decision. The checklist below is the work. Top to bottom. Finish one item and prove it before starting the next. Proof runs through the histos MCP tools in this checkout (`histos__test`, `histos__resolve`, `histos__workflow`, `histos__doctor`, `histos__preview`, `histos__reboot`, and `histos__fmt` once it exists). Tests bind loopback. They do not call Central, GitHub, or a keyserver.

## Decisions

- The command is `histos fmt`, `histos fmt --check`, `histos fmt <project>...`, and `histos fmt --file <path>...`.
- A bare `fmt` formats every module's `src` and `test` trees plus `histos.scala`, with the conf's `project.includeFilters`, `project.excludeFilters`, and `project.git` applied. Bare `fmt` already means everything, the same way bare `compile` and `test` do. There is no `--all`, and `fmt --all` is usage.
- `fmt <project>...` formats the named modules (an aggregate expands) and leaves `histos.scala` alone. `fmt --file` formats exactly those files, and the project filters still apply.
- The build turns formatting on: `.scalafmt` (conf at `.scalafmt.conf`) or `.scalafmt(RepoPath("config/scalafmt.conf"))`. In the api that is `enum Formatting { case Off; case Scalafmt(conf: ScalafmtConf) }` on `ProjectGraph`, with `enum ScalafmtConf { case Root; case At(path: RepoPath) }`. `Root` is `.scalafmt.conf`, which a `RepoPath` segment cannot spell. The declaration drives the lock cells, the CI `format` job, and the doctor checks. Without it, `fmt` fails with `FormatError.NotDeclared`.
- `fmt` works through MCP as `histos__fmt`. It sends the same `Call.Format` as the CLI and returns the same report.
- `fmt --check` is the preview. A separate `preview fmt` is cut: it would have to load scalafmt to apply the filters and could not say more than the check already says.
- The engine takes one new compile dependency: `org.scalameta:scalafmt-interfaces:3.11.5`. It is plain Java, about 30 KB, with no transitive dependencies. `scalafmt-dynamic-core`, typesafe config, sysops, `scalafmt-core`, and `scala-reflect` are locked tool cells and never enter `dist/histos.jar`.

## How it runs

Histos runs scalafmt in-process the way sbt-scalafmt does: the stable Java `scalafmt-interfaces` API, the `scalafmt-dynamic-core` implementation, and an injected `RepositoryPackageDownloaderFactory`. sbt-scalafmt's downloader resolves `scalafmt-core` through sbt at format time. The Histos downloader is a pure lookup of jars Histos already fetched from the lock: no second resolver, no unverified jar, and no effect run inside a Java callback.

The loader is `URLClassLoader(dynamic jars, new ScalafmtClassLoader(engine loader))`, so only `org.scalafmt.interfaces` crosses the boundary. The isolated loader has no Scala library of its own, so the dynamic cell's closure carries scala-library (sbt-scalafmt #440).

Interop stays in one adapter. `ScalafmtResult` has nullable fields, wrapped once with `Option`. `PositionException` is an interfaces type, so it is a type test. A `ScalafmtDynamicError` from `createSession` lives in the isolated loader and becomes `FormatError.Config` with its text shown and never branched on. The reporter is silent, as in Metals: every error already comes back through `formatOrError` or the `createSession` failure.

## Types

```scala
// wire
enum FormatMode derives JsonCodec:
  case Write, Check

enum FormatTargets derives JsonCodec:
  case Build
  case Projects(names: NonEmptyChunk[ProjectName])
  case Files(paths: NonEmptyChunk[Path])

// histos.server.Call
case Format(targets: FormatTargets, mode: FormatMode)

// wire: the fault a call carries, and the configuration fault resolve carries
enum ScalafmtConfError:
  case Missing(config: Path)
  case Unreadable(failure: IoFailure)
  case NoVersion(config: Path)
  case BadVersion(config: Path, raw: String)
  case Unsupported(config: Path, version: String)

final case class ScalafmtPackage(group: String, artifact: String, version: String)

enum FormatError:
  case NotDeclared
  case Conf(error: ScalafmtConfError)
  case Resolved(error: ResolveError)
  case CoreDrift(asked: List[ScalafmtPackage], locked: List[ScalafmtPackage])
  case Load(reason: String)
  case Config(config: Path, reason: String)
  case Tracked(reason: String)
  case Selected(error: SelectError)
  case Io(failure: IoFailure)

// engine: histos.format. Every path is relative to the repository.
opaque type ScalafmtVersion = String // from(config, raw) is typed; the floor is 3.0.0
final case class FormatterJars(dynamic: ToolInput, core: ToolInput, roots: List[ScalafmtPackage])

enum FormatFault:
  case Parse(file: Path, line: Int, column: Int, reason: String)
  case Read(failure: IoFailure)
  case Write(failure: IoFailure)
  case Failed(file: Path, reason: String)

final case class FormatReport(
    mode: FormatMode,
    changed: List[Path], // written, or unformatted under Check
    unchanged: Int,
    skipped: Int,        // ledger hits
    excluded: Int,       // project filters, or untracked under project.git
    faults: List[FormatFault],
)
```

`ResolveError.Scalafmt(ScalafmtConfError)` is how `resolve` reports a declared configuration it cannot lock. `BuildFault.Format(FormatError)` is how a call reports a run that never formatted.

A check with a changed file, or any run with a fault, is a failed call. The message lists each file and ends with `run histos fmt`.

## Lock

- `ToolRole` gains `ScalafmtDynamic` and `ScalafmtCore`.
- `ToolCatalog` gains `FormatTools(dynamic: ToolRevision, scala: ScalaVersion)`, default `3.11.5` and `2.13.18`. That Scala version is the `BuildInfo.scala` of dynamic-core 3.11.5, which asks for `scalafmt-core_2.13` plus `scala-reflect`.
- The dynamic cell is the closure of `org.scalameta:scalafmt-dynamic-core_2.13:<dynamic>`.
- The core cell is the closure of `org.scalameta:scalafmt-core_2.13:<conf version>` and `org.scala-lang:scala-reflect:<scala>`.
- `LockedTools.plan` reads the conf only when the build declares `Scalafmt(config)`. `resolve --update` writes both cells. `resolve` and `resolve --check` fail with `ToolDrift` when the conf version moved and the lock did not, and the message names `histos resolve --update`.
- The downloader compares the packages dynamic asks for with the core cell's roots. A match returns the fetched files. A mismatch returns no files and records `CoreDrift`. Nothing throws.

## Service

```scala
trait Formatters:
  def format(request: FormatRequest): IO[FormatError, FormatReport]

object Formatters:
  val live: ZLayer[Scope, Nothing, Formatters]

final case class FormatRequest(repo: Path, graph: ProjectGraph, targets: FormatTargets, mode: FormatMode, jars: FormatterJars)
```

- `Formatters` sits beside `Compilers` in the daemon environment and in the local CLI path. It holds one warm `Scalafmt` keyed by the two `ToolInput` fingerprints in a `Ref.Synchronized`. A new key releases the old one (`clear()`, then the loader's `close`). The layer's scope releases the last one.
- `FormatRun` gets `FormatterJars` from `ResolutionService.tool`, so a locked build reads the lock and a solving build solves, the same as compile. The service never resolves.
- The warm engine's key is both fingerprints and the locked roots, because the downloader inside it holds the roots.
- `clear()` releases an engine. The dynamic loader is not closed: `clear` closes the core loaders on scalafmt's own pool, and closing the dynamic loader as well races that close.
- Each call creates a session for the conf, collects the targets, applies `matchesProjectFilters` (and git-tracked files when `isGitOnly`), drops ledger hits, and formats with `ZIO.foreachPar` at `Parallelism.default.cpu`. `Write` writes a file only when its text changed.

## Ledger

`.histos/format/ledger` holds one key, the sha256 of the two tool fingerprints and the conf bytes, and the `(RepoPath, sha256)` pairs known formatted under it. A file is skipped only when its current bytes hash to a recorded value. A new key starts empty. A fault or an unformatted file is never recorded. The ledger is read once and written once per run. It keys on content and never on mtime.

## Surfaces

- CLI: `Wire.Fmt`, `Command.Fmt`, `Operation` to `Call.Format`, and `Help`.
- Native client: `client/Batch.scala` forwards the call.
- Daemon: a `Perform` arm with `needsGraph` true, and `Calls.format`.
- Watch: `~ fmt --check` and `~ fmt` are allowed. A write retriggers once, and the second run is all ledger hits.
- MCP: `histos__fmt` with `FmtArgs(projects: List[String] = Nil, files: List[String] = Nil, mode: Option[String])`. `mode` is `write` or `check`, and absent means `write`. Projects and files together are rejected.
- Native client: `fmt` and `fmt --check` with or without project names batch. `--file` goes to the JVM client.
- Doctor: `format.config` (a declared formatter with a missing conf or `version`) and `format.lock` (the conf version is not the locked core), as pure outcomes in `FormatChecks`.
- Workflow: a `format` job beside `scaladoc` and `coverage` that runs `./histos fmt --check`. No Node, LLVM, Playwright, or store restore. It renders only when the build declares `.scalafmt`.

## Left out

- Format on compile. Compile reads sources and never writes them. Metals formats on save, and CI checks.
- sbt-scalafmt's `diff-dirty` and `diff-ref` filters. The ledger makes the full run cheap, and `affected` owns what changed.
- A unified diff on `--check`. It needs a diff implementation or a dependency.
- Scalafmt older than 3.0.0, which is `Unsupported`.
- Bumping the conf version from the supply-chain updater. The catalog row is ready for it.

## Checklist

- [x] **Miniature.** A `scala-cli` program in `/tmp` implements `RepositoryPackageDownloaderFactory` in Scala 3, loads the real 3.11.5 dynamic-core through `ScalafmtClassLoader`, returns pre-fetched core jars, and formats one file. A mismatched package list yields a typed failure. Nothing in the repo changes until this is green.
- [x] **Declaration and lock.** `Formatting` and `.scalafmt` in the api, `FormatTools` in the catalog, the two tool roles, `ScalafmtVersion`, and `LockedTools.plan` reading the conf. This repository declares `.scalafmt` and the lock gains both cells. Proof: `ScalafmtVersionLawSpec`, `FormatPlanLawSpec` (both requests when declared, none otherwise, a conf bump against an old lock is `ToolDrift`), `FormattingBuildLawSpec`, and `histos__resolve` `check`.
- [x] **Engine.** The `scalafmt-interfaces` dependency, the adapter, the downloader, targets, ledger, and `Formatters`. Proof: `FormatDownloaderLawSpec`, `FormatLedgerLawSpec`, `FormatTargetsLawSpec`, and `FormatterLawSpec` against the real 3.11.5 jars this checkout locks. A missing jar is fetched. `Write` rewrites a misformatted file and leaves a formatted one byte-identical. A written file is proved only by the next run. `Check` names the file and writes nothing. A syntax error is `FormatFault.Parse` with its line. An excluded file counts as excluded. A broken conf is `FormatError.Config`. A moved core is `CoreDrift` with both sides. A second call does not load scalafmt-dynamic again.
- [x] **Surface.** The wire call, grammar, client, daemon arm, and `histos__fmt`. `preview fmt` is cut, see the decisions. Proof: `FmtGrammarSpec` (`fmt`, `fmt --check` in any position, `fmt <project>`, `fmt --file`, `fmt --all` and an absolute file are usage, the native batch) and `McpLawSpec` (the tool is listed, the arguments map to the same `Call.Format` the command line sends, bad arguments are rejected, the call round-trips). Live through MCP: `histos__fmt` `check` named 177 drifted files on this tree, `write` formatted them, a fresh `check` with no ledger was green, and the scalafmt 3.11.5 CLI agreed. One deliberately misformatted file failed `check` on exactly that file, and `write` scoped to `engine` restored it.
- [x] **CI and doctor.** The `format` job, `FormatChecks`, this repository's regenerated workflow, and the docs sweep: README, `docs/next.md`, `AGENTS.md`, and `Help`. Proof: `FormatWorkflowLawSpec`, `FormatChecksLawSpec`, `histos__workflow` `check`, `histos__doctor` `build` (`format.config` and `format.lock` ok), and `histos__test` on every module: api 184 and wire 241 on JVM and Native, engine 649, central 79, app 186.
- [ ] **CI pin.** CI compiles `histos.scala` with the pinned launcher, and the `0.2.3` pin has no `.scalafmt`. The pin has to move to a snapshot that has it before this repository's CI can load the build.
