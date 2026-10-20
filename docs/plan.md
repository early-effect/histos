# Histos plan

The design is [design.md](design.md). This file is the stack, not a second design.

## Landed

Stacks A through D, and the platform follow-ups, are on main through pull request #41 (`c31b8c4`).

That includes self-build (`./histos` execs `dist/histos.jar`; `histos.scala` wires `api`, `central`, `engine`, and `app`), the task API, zio-cli, MCP (`graph`, `run`, `cancel`, `toolchain`), ACP `initialize`, the scheduler, the content-addressed cache, the JDK and Coursier locks, Zinc for Scala 3 and Scala 2.13, the test subprocess (`TestProvider`, `testOnly`, `testQuick`, watch), Scala.js and Scala Native, local Maven publish, the version catalog, CI render, BSP 2.2 as the as-built section describes it, the opt-in server, and the opt-in remote cache. There is no GitHub Actions file in this repo. There is no sbt and no scala-cli.

`histos/plugins` is seven local commits ahead of that main. It is not a pull request. The loader is not on main. The spec is [plugins.md](plugins.md).

## Types later work must not repaint

These signatures are what the code is. The next pulls extend them. They do not replace them with a stringly form, and they do not narrow a typed error to `String`.

```scala
inline def module(name: String, platform: Platform, platforms: Platform*)(
    version: ScalaVersion,
    versions: ScalaVersion*,
): ModuleDef

inline def aggregate(name: String, project: ProjectDef, projects: ProjectDef*): AggregateDef
```

One platform and one Scala version are required. `aggregate` takes `ProjectDef`, not `Project`. `scala"..."` and `dep"..."` stay inline literals. `Projects` returns `Either[BuildError, ProjectGraph]`. `HistosBuild.graph` is that either.

```scala
trait Tasks:
  def run(id: TaskId): ZIO[Scope, TaskError, Run]
  def cancel(id: RunId): IO[TaskError, Unit]
```

`TaskId.Inspect` is `"inspect"`. `TaskId.Versions` is `"versions"`. On main, `TaskKind` is `Inspect`, `Compile`, and `Versions`. The plugin branch adds `Plugin`. A plugin task id contains a slash and must not occupy the two built-in strings.

On main, load is:

```scala
trait Loader:
  def load(repo: Path, apiJar: Path): ZIO[Scope, LoadError, LoadedBuild]
```

The `Scope` closes the build classloader. On main the Scala `3.10.0` compiler loads `histos.scala` against the api jar, the code source of `scala.collection.immutable.Nil` (`scala-library`), and the code source of `scala.quoted.Quotes` (`scala3-library`). It is not the compiler that compiles a module. BSP pulls that land before the plugin branch compile against this signature, and they add `buildClasspath` rather than painting that list into `Bsp.open` by hand. The plugin branch widens the environment to `Scope & Resolve & ContentStore`, adds `scan`, and implements `buildClasspath` as `Boundary.jars` (the same libraries plus ZIO, `zio-stacktracer`, and `izumi-reflect`). Once that PR merges, do not narrow it back, and do not discover plugins by scanning the module classpath. Enablement stays the header. The classloader on that branch still parents `ApiBoundary` on the engine loader. `plugins.md` requires an api `URLClassLoader` parented on the platform loader. The plugin PR closes that gap. It does not accept the engine parent.

`ScalacFlag` produces compiler text (`JvmCompile`). The build file does not pass a raw `List[String]` into Zinc. Production compiles pass `ScalacFlags.empty`. The `EncodingUtf8` case exists and is not on that path. The ascent flag work adds enum cases. It does not open the argv. `-Xsemanticdb` and `-sourceroot <repo>` are appended beside `flags.argv`. They are not enum cases. `-Xfatal-warnings`, if a later case adds it, stays on the CLI argv and is dropped from the BSP copy of that list.

Artifact suffixes stay one function: `_3` or `_2.13`, `_sjs1`, `_native0.5`. `_native0.5` is the ABI. The toolchain revision is `nativeVersion`, default `0.5.12`.

`TestProvider` is not a `HistosPlugin`. `central`'s `Publisher.publish` returns `IO[PublishError, Published]`. This stack does not wrap `central` as a plugin.

The BSP diagnostic notification is `build/publishDiagnostics`. Do not "correct" it to the stale Metals page name `buildTarget/publishDiagnostics`.

ZIO is `2.1.27`, the revision `histos.scala` pins. `heddle-mcp` is `0.12.0`. `zio-cli` `0.8.2` is the CLI parser. Do not pin a second ZIO. On main, `api` has no ZIO library dependency. The plugin branch adds `dev.zio::zio:2.1.27` because `HistosPlugin.run` is a ZIO. Do not shade ZIO into the api jar, and do not put the engine on the plugin compile classpath.

## Next stack

The detail is the PR plan in [design.md](design.md). Open in this order. Do not merge. You merge.

1. **BSP navigation for this repo.** One shared scalac list (`flags.argv` plus `-Xsemanticdb` and `-sourceroot <repo>`), used by `cellFeed`, test compile, and `scalacOptions`. Zinc diagnostics publish `startLine` through `endColumn`, minus one, at least one character. `histos.scala` keeps the 0-based dotty range and maps `ERROR`/`WARNING`/`INFO` from 2/1/0 to BSP 1/2/3. A sink writes the frame before the result. `buildTarget/dependencySources` may download from the lock's repository and omits a miss. Every module id carries the platform: `?scala=<version>&platform=jvm&scope=main` and `&scope=test`. A test id depends on that main id. A main id depends on upstream main ids. Sources are file URIs. Widen `testSources`, `runtimeClasspath`, `testClasses`, and `compiledTests` to `private[histos]`. `compiledTests` stays the method that takes a `CompiledCell` and the test sources. The test compile calls it with a `CompiledCell` from `classesDir` (`classFiles = 0`). It does not call `cellFeed`, `GraphCompile.run`, or `ModuleTest.console`. `suiteEnv` and `testWork` stay private until item 2. A JVM main compile calls `GraphCompile.cellFeed` with `Platform.Jvm`, the versions on the requested ids, and `picked` equal to those modules plus the upstream closure. The check is that the module builds that platform, not `chooseCells`'s hardcoded `Platform.Jvm`. `layersOf` compiles the closure first inside that call. A class directory from an earlier request does not satisfy `GraphCompile.upstreamClasses`. It does not call `console` (`only = None`) or `runCells` (`platform = None`). Test `scalacOptions` reports the `runtimeClasspath` order and names a missing upstream directory. It does not call `upstreamClasses`. `MissingUpstream` stays on `compiledTests` and the test run. A synthetic target for `histos.scala` (`?build=histos.scala`) whose user classpath is `Loader.buildClasspath`: on main, the api jar, `scala-library`, and `scala3-library`. `ScalaBuildTarget.jars` stays `JvmCompile.scalaJars`. Prove in VS Code and in Cursor. Completion in the build file is that user classpath. A module squiggle is the Zinc range. A build-file squiggle is the unshifted dotty range. Keep the bsp trace. The trace does not choose the test protocol.
2. **One JVM test from the editor.** `buildTarget/jvmTestEnvironment` now, after compile. Classpath is test classes, then `runtimeClasspath`, including its directory check. Widen `suiteEnv` and `testWork` only. `runtimeClasspath` and `testClasses` are already visible. No debug provider, no DAP URI. Breakpoints stay out. Depends on item 1, not on a trace.
3. **The same BSP for ascent's JVM modules.** Js and Native targets only when `.histos/bsp-platforms` names them. The file is absent by default. Not a field on `Module`. `histos compile` still builds every platform. Ids already say `&platform=jvm`. A `js` or `native` line publishes a different id. It does not reuse the JVM URI. `buildTarget/compile` passes that token into `cellFeed`, with the upstream closure in `picked`, and does not link. The platform check accepts Js and Native. It does not call `chooseCells`.
4. **The plugin series.** The `histos/plugins` commits, rebased across `JvmCompile` and `Loader.buildClasspath`. Close the classloader gap to `plugins.md` (api `URLClassLoader` on the platform loader). Do not redesign the plugin model. The wait is stack order, not a compile break. `central` stays in-tree.
5. **A Specular plugin, in the Specular repo.** A docs task and a CI job. This repo's fixture stays a fixture. Pin a snapshot of `histos.api`. No release.
6. **Ascent proof.** Tests, named mains, scalac flags. Depends on item 1, because both change compiler argv. `./histos compile` and `./histos test` on `~/projects/early-effect/ascent-histos`. Central publish stays out. `-Xfatal-warnings`, if added, stays on the CLI and is dropped from the BSP list.
7. **Generated CI for this repo.** The renderer already exists. Commit the aggregate `ci.yml`. Drift check on. Do not enable split axes: the matrix argv uses `--platform` and `--scala`, and the CLI rejects those flags.
8. **Scala.js host and VSIX.** After 1 and 2 are in daily use. groks-beard host shape. Not a `HistosPlugin`. It does not own completion.

## Not in this stack

- Test jail, and remote execution. Tests stay local subprocesses.
- An sbt importer.
- MiMa on `histos.api`. That is the 1.0 cut.
- Publishing Histos. No snapshot and no Central release until you say the feature is complete. A downstream pin uses the snapshot channel.
- Loading `central` through the plugin classloader.
- A presentation compiler, a Bloop export, and a VS Code extension that is the language client.
- Editing `~/projects/early-effect/ascent-histos` or groks-beard or Specular from a docs change. The pulls above are what edit them, when those pulls are open.
