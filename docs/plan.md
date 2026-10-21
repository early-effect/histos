# Histos plan

The design is [design.md](design.md). This file points at that design. It is not a second one.

## Landed

Stacks A through D, and the platform follow-ups, are on main through pull request #41 (`c31b8c4`).

That includes self-build (`./histos` execs `dist/histos.jar`; `histos.scala` wires `api`, `central`, `engine`, and `app`), the task API, zio-cli, the scheduler, the content-addressed cache, the JDK and Coursier locks, Zinc for Scala 3 and Scala 2.13, the test subprocess (`TestProvider`, `testOnly`, `testQuick`, watch), Scala.js and Scala Native, local Maven publish, the version catalog, CI render, BSP 2.2, the opt-in server, and the opt-in remote cache. There is no sbt and no scala-cli.

The plugin loader is in this tree (`engine/src/histos/load`). `ApiBoundary` is parented on the platform loader. Boundary names load from the engine host that defined `HistosBuild`. A second api `URLClassLoader` that defines `ProjectGraph` again is not the work. The spec is [plugins.md](plugins.md), and it follows that rule.

## Types later work must not repaint

These signatures are the ones the tool is building toward. Do not replace them with a stringly form, and do not narrow a typed error to `String`.

```scala
inline def module(name: String, platform: Platform, platforms: Platform*)(
    version: ScalaVersion,
    versions: ScalaVersion*,
): ModuleDef

inline def aggregate(name: String, project: ModuleDef, projects: ModuleDef*): AggregateDef
```

One platform and one Scala version are required. `aggregate` takes `ModuleDef`. A nested aggregate does not compile. `scala"..."` and `dep"..."` stay inline literals. `Projects` returns `Either[BuildError, ProjectGraph]`. `HistosBuild.graph` is that either. A child that is not a module is `BuildError.AggregateChild`. `Workflow.target` still answers `CiError.NotModule` for an aggregate name.

```scala
trait Tasks:
  def run(id: TaskId): ZIO[Scope, TaskError, Run]
  def cancel(id: RunId): IO[TaskError, Unit]
```

`TaskId.Inspect` is `"inspect"`. `TaskId.Versions` is `"versions"`. `TaskKind` is `Inspect`, `Compile`, `Versions`, and `Plugin`. A plugin task id contains a slash and must not occupy the two built-in strings.

Load is:

```scala
trait Loader:
  def load(repo: Path, apiJar: Path): ZIO[Scope & Resolve & ContentStore, LoadError, LoadedBuild]
```

The `Scope` closes the build classloader. The Scala `3.10.0` compiler loads `histos.scala` against the api jar, the code source of `scala.collection.immutable.Nil` (`scala-library`), and the code source of `scala.quoted.Quotes` (`scala3-library`). It is not the compiler that compiles a module. `buildClasspath` is `Boundary.jars` (those libraries plus ZIO, `zio-stacktracer`, and `izumi-reflect`). Do not narrow it back to the api jar, and do not discover plugins by scanning the module classpath. Enablement stays the header. `ApiBoundary` is parented on the platform loader. Boundary names load from the engine host that defined `HistosBuild`, so `ProjectGraph` stays one class. Do not add a second api `URLClassLoader` that defines those classes again.

`ScalacFlag` produces compiler text (`JvmCompile`). The build file does not pass a raw `List[String]` into Zinc. `Module.flags` defaults to `ScalacFlags.empty`, and `ModuleDef.scalac` replaces that list. Compile and BSP pass `module.flags.argv`, then the SemanticDB append. The `EncodingUtf8` case is `-encoding` and `UTF-8`. The ascent flag work adds enum cases. It does not open the argv. `-Xfatal-warnings`, if a later case adds it, stays on the CLI argv and is dropped from the BSP copy of that list.

Artifact suffixes stay one function: `_3` or `_2.13`, `_sjs1`, `_native0.5`. `_native0.5` is the ABI. The toolchain revision is `nativeVersion`, default `0.5.12`.

`TestProvider` is not a `HistosPlugin`. `central`'s `Publisher.publish` returns `IO[PublishError, Published]`. This work does not wrap `central` as a plugin.

The BSP diagnostic notification is `build/publishDiagnostics`. Do not "correct" it to the stale Metals page name `buildTarget/publishDiagnostics`.

ZIO is `2.1.27`, the revision `histos.scala` pins. `heddle-mcp` is `0.12.0`. `zio-cli` `0.8.2` is the CLI parser. Do not pin a second ZIO. The api module depends on `dev.zio::zio:2.1.27` because `HistosPlugin.run` is a ZIO. Do not shade ZIO into the api jar, and do not put the engine on the plugin compile classpath.

## Remaining work

Commits 1 through 9 of [design.md](design.md) are in this working tree. They are not a release. The JVM session and the plugin loader were already in the tree, so they were not on that list.

What remains waits on names that are not chosen yet:

10. The ascent copy's tests, mains, and flags. This waits until the `ScalacFlag` cases are chosen.
11. A Specular `HistosPlugin` in the Specular repo. This waits. The plugin id, task id, `JobWhen`, and snapshot coordinate stay unnamed.
12. One Scala.js host module. This waits. The module name, vscode coordinate, and command list stay unnamed. It does not own completion.

## Not in this work

- Test jail, and remote execution. Tests stay local subprocesses.
- An sbt importer.
- MiMa on `histos.api`. That is the 1.0 cut.
- A Central release. The snapshot `0.0.1-SNAPSHOT` is uploaded by a local `histos publish snapshots`. CI downloads that launcher and does not publish.
- Loading `central` through the plugin classloader.
- A presentation compiler, a Bloop export, and a VS Code extension that is the language client.
- Editing `~/projects/early-effect/ascent-histos` or Specular from a docs change. Commit 10 edits the ascent copy. Commit 11 edits the Specular repo. groks-beard stays unedited. Commit 12 adds the host module in this repo when its names exist.
