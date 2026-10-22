# Histos Plugin Architecture

**Status:** Implemented
**Org:** early-effect
**Repo:** `early-effect/histos`

This document is the plugin contract: what a plugin author writes, what a build author writes, and what the engine guarantees. The api types live in `api/src/histos/plugin`. The loader and the runtime live in `engine/src/histos/load`. Command dispatch lives in `app/src/histos/cli`. [architecture.md](architecture.md) describes the rest of the tree as built, and [roadmap.md](roadmap.md) holds planned work, including the Specular plugin in the Specular repository.

Code blocks marked as excerpts copy declarations verbatim from the named file and leave out doc comments, `CanEqual` givens, and unrelated members. Every other Scala block is user or plugin code that compiles against today's api.

## Overview

A Histos plugin is a Scala 3 library compiled against `histos.api`, plus one entry object that the engine loads from the jar the build names. The entry returns pure data (task specs, command specs, CI jobs) and a runner the engine calls later. The project graph stores the data. The effect is the value `run` returns, a `ZIO[Scope, PluginRunError, PluginOutput]`.

The build opts in inside `histos.scala`: one file, one list. The loader never scans the dependency classpath for an entry, and a jar applies nothing just because it is present. Module settings a plugin offers are ordinary functions that return a new `ModuleDef`, the same way `.main`, `.jsEnv`, and `.in` do.

`central` is an in-tree module. `Publisher.publish` returns `IO[PublishError, Published]`. It is not a `HistosPlugin`, and the plugin loader does not load it.

## Background

Histos replaces sbt and zipx for a repo that has moved. Plugins are ordinary libraries on `histos.api`, and docs, docker, deploy, coverage, Central releases, and the dependency-update bot are meant to be plugins ([roadmap.md](roadmap.md) orders them). What hurts in sbt, by name, and what Histos does instead:

| sbt | Why it is hostile | Histos |
| --- | --- | --- |
| AutoPlugin `trigger` and `requires` | A jar on the classpath mutates a build the author did not name. | `use` names the plugin. There is no trigger, and no `requires` that pulls another plugin in. |
| `plugins.sbt` split from `build.sbt` | Two files and two classpaths, so the plugin graph is a second build. | One file, `histos.scala`. A header of literals lets that one file compile against the jars it names. |
| `SettingKey` / `TaskKey` and the project, configuration, and task axes, plus `:=` and `++=` | Scope is a stringly product. The plugin surface is a macro language. | A setting is a function `ModuleDef => ModuleDef`. A task is a `DeclaredTask` the engine runs. There is no scope axis. |
| A plugin that cannot be called as Scala | `site / makeSite` is a key, so the build file cannot treat it as a value. | `Specular.site` is a function. `.pipe` applies it, and the author can also write `Specular.site(module(...))`. |
| Binary compatibility of sbt as the plugin surface | Plugins break when sbt breaks, and the compatibility story is the whole builder. | The surface is `histos.api`. Before 1.0 it may break. `LinkStrategy.Api` is a typed refusal, and there is no shim. |

Publishing is a task a plugin can implement. `central` compiles as an in-tree module that sees `api` plus the ZIO it declares in `histos.scala`.

## Guarantees and Scope

### Guarantees

- A plugin compiles against `histos.api` only. The launcher supplies Histos and ZIO at runtime. The engine, the scheduler, the CLI, and `central` are off the plugin compile classpath.
- Enablement is one explicit list in `histos.scala`. The loader reads the jars the build names and scans nothing else for an entry.
- The graph stores descriptions. `Seal` and `Need` are data on the task. The runner is an effect the engine calls inside a `Scope`.
- A plugin task id cannot replace `inspect` or `versions`.
- A plugin command reaches the CLI without a grammar change per plugin, and the engine never names a plugin.
- A plugin CI job is data that `Workflow` renders. Its step is `./histos run <plugin>/<task>`.
- Before 1.0, a binary plugin must match the api jar the launcher loads. The token is `ApiDigest`, the sha256 of that jar. A mismatch is a typed error that names the plugin and both digests. Source plugins and `plugin(path"...")` compile against the running api.
- Plugin classes live on the build classloader only. A project compile never sees them.
- A plugin author tests `run` with ZIO Test and `PluginFixture`, against `api` only.
- Only a `Seal.Hermetic` task with no `Need` is sendable. `Sendable.decide` makes that call for core and plugin actions alike.

### Out of scope

- sbt 1.x plugin API compatibility: no AutoPlugin, `SettingKey`, `TaskKey`, `plugins.sbt`, `sbt.AutoPlugin` entry, or `project/plugins.sbt`.
- A binary-compatibility promise for `histos.api` before 1.0: no MiMa and no shim. `LinkStrategy.Api` exists so a header can name it and get a typed refusal.
- Test frameworks. `TestProvider` (`api/src/histos/TestProvider.scala`) names an `sbt.testing.Framework` that the test child process loads, and `ProviderRunner` runs in that child. A test framework is a different seam.
- Scala Native compiler plugins. `NativeComponent.Plugin` in `engine/src/histos/native/NativeLibs.scala` is the `nscplugin` jar passed as `-Xplugin`, a compiler plugin.
- Loading `central` through the plugin classloader.
- A filesystem or network sandbox. Enforcement is the declaration plus `Sendable`. [roadmap.md](roadmap.md) tracks the sandbox.
- Remote execution. A remote cache stores results and runs nothing elsewhere.
- BSP build targets for plugin tasks. BSP serves JVM compile cells. Agents run a plugin task through the MCP `run` tool.
- Cross-plugin task dependencies. A task may depend on another task of the same plugin, or on compiling a named module cell.
- A second settings language, a context-function front end, or closures from `histos.scala` stored on the graph. `Runner.Closure` stays the non-sendable bucket.

## The plugin is data plus a runner

One object in the plugin jar extends `HistosPlugin`. `contribute` returns data and is not a `ZIO`. `run` is the effect. The engine calls both. The plugin never calls the engine and never sees `Scheduler` or `Workflow`.

Excerpt from `api/src/histos/plugin/HistosPlugin.scala`:

```scala
trait HistosPlugin:
  def id: PluginId
  def contribute: Contribution
  def run(request: RunRequest): ZIO[Scope, PluginRunError, PluginOutput]

object HistosPlugin:
  val entryResource: String = "META-INF/histos/plugin"
```

`contribute` runs while `Projects` evaluates `graph`, on the load fiber, because that is when the data is copied onto the graph. It must be total: no I/O and no `throw`. Its return type is not a `ZIO`, which is the enforcement. A throw anyway becomes `BuildError.PluginDefect(plugin, binary)`, where `binary` is the exception class name. The exception message never enters the typed error. Load never calls `run`.

`ProjectGraph.plugins` holds data only. The plugin instance and the classloader stay with the loader. Excerpt from `api/src/histos/plugin/Request.scala`:

```scala
final case class RegisteredPlugin(
    id: PluginId,
    entry: EntryName,
    contribution: Contribution,
)
```

`EntryName` is the JVM binary name of the module class, including the trailing `$`. `Projects` records `EntryName.fromClass(plugin.getClass)`. During load, the loader defines each header entry on `BuildLoader` and reads its public `MODULE$` field. `PluginRuntime` keeps those instances, and `run` calls them. A Scala object is a singleton on its loader, so this is the same instance `Projects` called `contribute` on.

`api` has a compile dependency on `dev.zio::zio:2.1.27`, the same revision this repo's `histos.scala` gives `central`, `engine`, and `app`. The api jar does not shade ZIO. Plugins depend on `histos.api` and inherit that ZIO. At runtime the launcher's ZIO wins, because `zio.*` is a boundary name.

## Types

### Ids and tokens

Every id and token is an opaque type in `histos.plugin`. Each has `inline def apply(inline raw: String)`, backed by a macro in `PluginMacros` (`api/src/histos/plugin/Macros.scala`) that rejects a bad literal when the plugin compiles, and `from(raw)`, the runtime parser, which returns an error enum. `ProjectPath`, `dep"..."`, and `path"..."` follow the same pattern. The examples below call `apply`.

Excerpt from `api/src/histos/plugin/PluginId.scala`:

```scala
opaque type PluginId = String

object PluginId:
  inline def apply(inline raw: String): PluginId = ${ PluginMacros.pluginId('raw) }

  def from(raw: String): Either[PluginIdError, PluginId] =
    if Tokens.pluginId(raw) then Right(trusted(raw))
    else if raw.isEmpty then Left(PluginIdError.Empty)
    else Left(PluginIdError.Bad(raw))

  extension (id: PluginId) def value: String = id

enum PluginIdError:
  case Empty
  case Bad(raw: String)
```

| Type | File | Grammar |
| --- | --- | --- |
| `PluginId` | `PluginId.scala` | Dot-separated segments. Each segment starts with a letter, and the rest are letters, digits, or hyphens. `dev.earlyeffect.specular` is valid. Empty, a slash, a colon, whitespace, an empty segment, or a leading or trailing dot fails. |
| `TaskName`, `CommandName`, `JobName`, `OutputName`, `ParamName`, `FlagName`, `NoteKey` | `Names.scala` | A letter, then letters, digits, or hyphens. No slash and no dot. |
| `EntryName` | `EntryName.scala` | A JVM binary class name: dot-separated identifiers, where a segment may contain single `$` separators and end with `$`. |
| `RepoPath` | `RepoPath.scala` | A repo-relative file or directory. Empty, a leading `/`, a backslash, or a `:` fails. Each `/` segment matches `[A-Za-z][A-Za-z0-9._-]*`, so `.`, `..`, and empty segments fail and `docs/index.html` passes. |
| `NoteText`, `PluginDetail` | `Texts.scala` | Non-empty, one line, no control characters. |
| `InputKey` | `Texts.scala` | Non-empty, no whitespace. |
| `EnvName` | `Texts.scala` | A letter or `_`, then letters, digits, or `_`. |
| `ApiDigest`, `ContentHash` | `ApiDigest.scala` | 64 hex digits, stored lowercase. |
| `Cron` | `Cron.scala` | Five whitespace-separated GitHub Actions cron fields, stored single-spaced. Month and weekday names are accepted. |

`TaskName`, `CommandName`, and `JobName` share one grammar and stay distinct types. `inspect` is a legal `TaskName`, and it still cannot shadow the built-in, because a plugin task id always has a slash.

`ProjectPath` keeps the module-directory grammar, which rejects a dot in a segment. `plugin(path"...")` and `.in` take `ProjectPath`. Declared inputs and outputs take `RepoPath`.

Excerpt from `api/src/histos/plugin/PluginTaskId.scala`:

```scala
final case class PluginTaskId(plugin: PluginId, name: TaskName):
  def render: String = s"${plugin.value}/${name.value}"

enum PluginTaskError:
  case Shape(raw: String)
  case Plugin(error: PluginIdError)
  case Task(error: TaskNameError)

object PluginTaskId:
  def parse(raw: String): Either[PluginTaskError, PluginTaskId] =
    raw.split("/", -1).toList match
      case plugin :: name :: Nil =>
        for
          id   <- PluginId.from(plugin).left.map(PluginTaskError.Plugin(_))
          task <- TaskName.from(name).left.map(PluginTaskError.Task(_))
        yield PluginTaskId(id, task)
      case _ => Left(PluginTaskError.Shape(raw))
```

`parse` requires exactly one slash. `inspect` and `versions` fail with `Shape`. `dev.earlyeffect.specular/versions` parses, and it does not occupy `TaskId.Versions`, because it is a different string.

### Seal and Need

Excerpt from `api/src/histos/plugin/Seal.scala`:

```scala
enum Seal:
  case Hermetic
  case Open

enum Need:
  case Network
  case Docker
```

`histos.schedule.Sendable` imports both. `Sendable.decide` is the one policy for core and plugin actions.

### Contribution

Excerpt from `api/src/histos/plugin/Contribution.scala`:

```scala
final case class Contribution(
    tasks: List[DeclaredTask],
    commands: List[CommandSpec],
    jobs: List[CiJob],
)

final case class DeclaredTask(
    name: TaskName,
    seal: Seal,
    needs: Set[Need],
    inputs: List[DeclaredInput],
    outputs: List[DeclaredOutput],
    dependsOn: List[TaskDep],
)

enum DeclaredInput:
  case File(path: RepoPath)
  case Directory(path: RepoPath)
  case Module(name: ProjectName, version: Option[ScalaVersion], platform: Option[Platform])
  case Note(module: ProjectName, key: NoteKey)
  case Text(name: InputKey, value: String)
  case Env(name: EnvName)

final case class DeclaredOutput(name: OutputName, path: RepoPath)

enum TaskDep:
  case Compile(module: ProjectName, version: Option[ScalaVersion], platform: Option[Platform])
  case Same(name: TaskName)

final case class CommandSpec(
    name: CommandName,
    params: List[CliParam],
    task: TaskName,
    summary: PluginDetail,
)

enum CliParam:
  case Text(name: ParamName)
  case OptionalText(name: ParamName)
  case Flag(name: FlagName)

final case class CiJob(
    name: JobName,
    task: TaskName,
    when: JobWhen,
    permissions: JobPermissions = JobPermissions.ContentsRead,
)

enum JobWhen:
  case Always
  case Schedule(cron: Cron)

enum JobPermissions:
  case ContentsRead
  case ContentsWrite
```

Inputs and outputs:

- `Directory` hashes every regular file under that directory, keyed by relative path and sorted. There is no glob dialect.
- A declared output is a regular file. The engine hashes it after `run`. A directory cannot be an output.
- A declared `File` or `Directory` that is missing, or an `Env` that is unset, fails the run with `PluginRunError.Failed` before `run` is called.
- A task with `outputs = Nil` stores `PluginOutput.text` and nothing else, and that text is a legal cache hit. A task that declares files is a hit only when every declared file exists with the sha256 the manifest recorded.

Cells:

- `TaskDep.Compile` and `DeclaredInput.Module` name cells. A module builds one cell per Scala version and platform. `None` means every cell that module builds, Js and Native included. `Some` must name a cell that module builds, or `Projects` returns `BuildError.PluginMissingCell`. An unknown name, or an aggregate, is `BuildError.PluginMissingModule`.
- `TaskDep.Compile` compiles the selected cells before the task is hashed, and `RunRequest.compiled` carries their classpaths.
- `DeclaredInput.Module` hashes the selected cells' class directories as they are on disk, sorted by Scala version, then platform token. Pair it with `TaskDep.Compile` on the same cells when the task needs fresh classes.

Notes:

- `DeclaredInput.Note` is one module plus one key. `Projects` rejects two notes with the same plugin and key on one module (`BuildError.DuplicateNote`). A note input with no matching note is `BuildError.MissingNote`.
- The runner never sees another plugin's notes: `ModuleView.notes` holds only the running plugin's notes.

Text, commands, and cron:

- `DeclaredInput.Text` carries a plugin-chosen `InputKey` and a literal. The engine derives the cache input name itself, so a plugin never touches `histos.schedule.InputName`.
- `CommandSpec.summary` is a `PluginDetail`, the help line printed after the command name in usage.
- `Cron.from` rejects an empty string, the wrong number of fields, and a field outside GitHub's cron grammar. The renderer prints `cron.value`. A plugin has no way to hand raw text to the YAML layer.

### Link strategy and the api digest

Excerpt from `api/src/histos/plugin/Contribution.scala`:

```scala
enum LinkStrategy:
  case ExactBinary
  case Source
  case BinaryOrSource
  case Api

enum PluginLinked:
  case FromBinary(api: ApiDigest)
  case FromSource(published: ApiDigest, running: ApiDigest)
```

`LinkStrategy.Api` is a real case with no success path. A header `.link(Api)` fails the scan with `PluginLoadError.ApiLink`, even when the digests would match. Nothing rewrites bytecode and nothing runs MiMa.

Excerpt from `api/src/histos/plugin/ApiDigest.scala`:

```scala
opaque type ApiDigest = String

object ApiDigest:
  val resource: String = "META-INF/histos/version"

  inline def apply(inline raw: String): ApiDigest = ${ PluginMacros.apiDigest('raw) }

  def from(raw: String): Either[ApiDigestError, ApiDigest] =
    Tokens.sha256(raw) match
      case Some(hex) => Right(trusted(hex))
      case None      => Left(ApiDigestError.Bad(raw))

  extension (digest: ApiDigest) def value: String = digest
```

`ApiDigest` is the sha256 of the `histos-api` jar bytes. When the launcher runs from an assembly, `BuildClasspath` extracts that jar to `api/<sha>/histos-api.jar` under the cache. A binary plugin's `META-INF/histos/version` resource is one line holding that hex, and the loader compares it to the sha256 of the api jar it loads. The product label `0.0.1-SNAPSHOT` (`Cli.VersionText`, `Bsp.ServerVersion`, `.histos-version`) stays put when `histos.api` breaks, so every 0.x binary would match it. The label and the digest are independent, and neither derives from the other.

### Errors

Load and run fail with separate enums. Excerpt from `api/src/histos/plugin/Errors.scala`:

```scala
enum PluginLoadError:
  case BadCoordinate(raw: String)
  case Misplaced(line: Int)
  case Unlocked(coordinate: String)
  case NoEntry(coordinate: String)
  case AmbiguousEntry(coordinate: String)
  case Unversioned(coordinate: String)
  case VersionMismatch(dependency: Dep, pluginApi: ApiDigest, launcher: ApiDigest)
  case ApiLink(dependency: Dep)
  case NoSource(coordinate: PluginCoordinate)
  case PathIsSource(path: ProjectPath)
  case ReservedPackage(binary: String)
  case EngineLeak(coordinate: PluginCoordinate, binary: String)
  case ForeignEntry(plugin: PluginId, entry: EntryName)
  case NotEntry(plugin: PluginId, entry: EntryName, expected: EntryName)
  case IdMismatch(declared: PluginId, found: PluginId)
  case Undeclared(plugin: PluginId)
  case Defect(coordinate: PluginCoordinate, binary: String)
  case Io(op: PluginIo)

enum PluginIo:
  case Read, Resolve, Compile, Open

enum PluginRunError:
  case Failed(detail: PluginDetail)
  case MissingOutput(name: OutputName)
  case ExtraOutput(path: String)
  case MissingArg(name: ParamName)
  case MissingNote(module: ProjectName, key: NoteKey)
  case Unknown(id: PluginTaskId)
  case Defect(binary: String)
  case EngineLeak(binary: String)
  case Linkage(binary: String)
```

Excerpt from `api/src/histos/plugin/Coordinate.scala`:

```scala
enum PluginCoordinate:
  case Published(dependency: Dep)
  case Path(path: ProjectPath)
```

Every case has `message`, the same way `BuildError` and `LoadError` do. Neither enum carries a `Throwable`. `Defect` carries the exception class name. `Linkage` carries the missing class name the JDK reports. `PluginDetail` is the plugin's own message, used when the plugin fails a run on purpose.

`ApiLink`, `VersionMismatch`, `NoSource`, `EngineLeak`, and `Defect` fire before the plugin object exists, so they name the header coordinate and never a `PluginId`. `VersionMismatch` also names both digests. `ForeignEntry`, `NotEntry`, `IdMismatch`, and `Undeclared` fire after `graph` returns and name the `PluginId`. `Loader.load` wraps every `PluginLoadError` as `LoadError.Plugin(error)`, and the CLI exits 3 with `error.message`.

A slash id that is not in the graph is `PluginRunError.Unknown`, a task failure with exit 1.

## Registration

Excerpt from `api/src/histos/Builder.scala`:

```scala
final case class Uses(plugins: List[HistosPlugin])

def use(plugin: HistosPlugin, rest: HistosPlugin*): Uses =
  Uses(plugin :: rest.toList)
```

`use` only builds `Uses`. It does not call `contribute`. `Projects` and `Organized` have overloads whose first parameter is `Uses`, beside the `Splits` and `CatalogSpec` overloads in `api/src/histos/Graph.scala`. `Uses` is not a `ProjectDef`, so `Projects(docs, use(...))` does not compile. The list goes first: `Projects(use(Specular.plugin), docs)`. A build that never calls `use` has `plugins = Nil`.

`Projects` registers each plugin inside the `Either[BuildError, ProjectGraph]` it already returns. It calls `contribute` under a `NonFatal` catch and records `EntryName.fromClass(plugin.getClass)`. A throw from `contribute` becomes `BuildError.PluginDefect` with the exception class name. A class name that is not a valid `EntryName` becomes `PluginDefect` with that class name. The throw never escapes the `Either`, and it never becomes a `PluginLoadError`.

`Projects` then checks the registered plugins against each other and against the modules:

- `DuplicatePlugin`: two `use`d plugins share an id.
- `DuplicatePluginTask`: one plugin declares a task name twice. Two plugins may share a local name.
- `UnknownPluginTask`: a `TaskDep.Same`, a `CommandSpec.task`, or a `CiJob.task` names no task of that plugin.
- `CommandShadow`: a command name is in `CommandName.reserved`.
- `DuplicateCommand`: two commands share a name. Command names are one namespace across all plugins.
- `ReservedJob`: a job is named `test`, `versions`, or `workflow` (`JobName.reserved`).
- `DuplicateJob`: two jobs share a name across plugins.
- `DuplicateNote`, `MissingNote`, `PluginMissingModule`, and `PluginMissingCell`, as described under Contribution.

A defect in `contribute` reaches the CLI as `LoadError.BrokenGraph`, exit 3.

## Enablement in `histos.scala`

Two words, two jobs. `plugin(...)` puts a jar on the build classpath. `use(...)` registers its tasks, commands, and jobs. A header plugin that is never `use`d is still resolved, version-checked, and loaded, so the build file can import from it, and it contributes nothing.

The compiler cannot see a jar that the file it is compiling declares. sbt split `plugins.sbt` off for that reason. Histos keeps one file and reads a constrained prefix as text before compiling:

```scala
plugin(dep"dev.earlyeffect::specular:0.2.0")
plugin(path"tools/docs-preview")
plugin(dep"dev.earlyeffect::coverage:0.1.0").link(ExactBinary)

import histos.*
import dev.earlyeffect.specular.Specular

object build extends HistosBuild:
  def graph =
    val docs =
      module("docs", Jvm)(scala"3.9.0")
        .pipe(Specular.site)
    Projects(use(Specular.plugin), docs)
```

There is no `plugin` method in `histos`. `dep` and `path` are `StringContext` extensions in `histos.Literals`, which are out of scope above `import histos.*`. The header lines never reach the compiler.

### Header grammar

`HeaderScan` (`engine/src/histos/load/HeaderScan.scala`) is a text scanner, and it runs before compile. From the start of the file it skips blank lines and whole-line `//` comments. Each remaining header line, trimmed, is one of:

```text
plugin(dep"<coordinate>")
plugin(dep"<coordinate>").link(ExactBinary|Source|BinaryOrSource|Api)
plugin(path"<project path>")
plugin(path"<project path>").link(Source)
```

- The coordinate goes through `Dep.from`, then `Resolve.ranged`: a revision that starts with `latest`, or contains `[`, `]`, `(`, `)`, `+`, or `,`, is a range. `Dep.from` accepts ranges, and the header refuses them. A parse failure or a range is `PluginLoadError.BadCoordinate`, before any resolve. A hyphen, as in `1.0.0-M1`, is an exact revision.
- The path goes through `ProjectPath.from`. A failure, including a path that escapes the repo, is `BadCoordinate`.
- After the closing `")`, the line may hold one `.link(...)` with a known strategy and nothing else. A trailing comment, a semicolon, or an unknown strategy is `BadCoordinate`.
- `plugin(dep)` with no `.link` records `BinaryOrSource`.
- `plugin(path)` records a path request. `.link(Source)` is accepted, and any other strategy is `PathIsSource`.
- `.link(Api)` on a dep is `ApiLink` at scan time, before any lock write or compile.
- A header-position line that starts with `plugin(` and is neither form is `Misplaced`.
- Blank lines, comments, and header forms continue the header. Any other line, `import` included, ends it.
- After the header, a line that starts with `plugin(dep"` or `plugin(path"` is `PluginLoadError.Misplaced(line)`, and compile does not start.

A coordinate built by concatenation or bound to a `val` is invisible to the scanner, because header lines are never typechecked. The build file then fails to compile its import, or `use` fails as `ForeignEntry`. That limit is the cost of having no `plugins.sbt`. `Misplaced` and the concatenation rule belong to the text scan.

### The compilation unit

Excerpt from `engine/src/histos/load/HeaderScan.scala`:

```scala
enum PluginRequest:
  case Dep(dependency: histos.Dep, link: LinkStrategy)
  case Path(path: ProjectPath)

final case class ScannedBuild(requests: List[PluginRequest], source: String)
```

Excerpt from `engine/src/histos/load/Loader.scala`:

```scala
trait Loader:
  def scan(repo: Path): IO[LoadError, ScannedBuild]
  def load(repo: Path, apiJar: Path): ZIO[Scope & Resolve & ContentStore, LoadError, LoadedBuild]
  def buildClasspath(apiJar: Path): IO[LoadError, List[Path]]
  def reportBuild(repo: Path, apiJar: Path): IO[LoadError, BuildNotes]
```

`requests` is the header. `source` is the text of the compilation unit: each header `plugin(...)` line becomes an empty line, and blank and comment lines stay as they are. The line count matches the file on disk, so a diagnostic line is the line the author sees. `Loader.load` writes `source` to a temp file, compiles that temp file with `dotty.tools.dotc.Driver`, and deletes it when the load `Scope` closes. Diagnostics that mention the temp path are rewritten to `histos.scala` before `LoadError.CompileFailed`. `Fingerprint.read` hashes the on-disk `histos.scala`, header included.

For the example above, the compiler sees:

```scala




import histos.*
import dev.earlyeffect.specular.Specular

object build extends HistosBuild:
  def graph =
    val docs =
      module("docs", Jvm)(scala"3.9.0")
        .pipe(Specular.site)
    Projects(use(Specular.plugin), docs)
```

### Module settings and notes

Excerpts from `api/src/histos/ProjectDef.scala` (methods on `ModuleDef`) and `api/src/histos/plugin/Request.scala`:

```scala
  def pipe(f: ModuleDef => ModuleDef): ModuleDef =
    f(this)

  def note(plugin: PluginId, key: NoteKey, value: NoteText): ModuleDef =
    copy(notes = notes :+ ModuleNote(plugin, key, value))
```

```scala
final case class ModuleNote(plugin: PluginId, key: NoteKey, value: NoteText)
```

`Specular.site` is a `ModuleDef => ModuleDef`. It may do only what any build author can do: call `libraryDependencies`, `in`, `note`, and the other methods that return a new `ModuleDef`. It receives no settings DSL.

Notes are how a function leaves plugin-owned data on a module after `Projects` erases `ModuleDef` into `Module` (`Module.notes`). The engine does not interpret them. They become cache inputs when a task declares `DeclaredInput.Note`. Storage is one line of text on purpose: `Module` is a closed case class and cannot carry a type member per plugin. The typed api the author calls is the plugin's function. A plugin that needs richer settings encodes them in `NoteText` and decodes them inside `run`.

## Classloader

```mermaid
flowchart TD
  build[BuildLoader: compiled histos.scala plus plugin classpaths]
  boundary[ApiBoundary]
  host[Engine host: the loader that defined HistosBuild]
  platform[Platform class loader]
  build --> boundary
  boundary -->|boundary names| host
  boundary -->|other names| platform
```

`ApiBoundary` (`engine/src/histos/load/BuildLoader.scala`) has no URLs, and its parent is the platform loader. A boundary name loads from the host, the loader that defined `HistosBuild`, so `ProjectGraph` and `HistosPlugin` are the engine's own classes. A refused name throws `ClassNotFoundException`. Every other name goes to the platform loader, which supplies the JDK.

Boundary names (`ClassHome.boundary` in `engine/src/histos/load/Boundary.scala`):

- classes in package `histos` with no subpackage (the api surface: `HistosBuild`, `ModuleDef`, `Dep`, and the rest)
- `histos.version.*`
- `histos.plugin.*`
- `zio.*`
- `scala.*`

Every other name under `histos.` is refused before any plugin URL is searched. That covers `histos.schedule`, `histos.zinc`, `histos.load`, `histos.task`, `histos.workflow`, `histos.cli`, `histos.central`, and any engine package added later, including a copy vendored in a plugin jar.

`BuildLoader` is a `URLClassLoader` over the build file's class directory plus the plugin classpaths. It applies the same split before its own search: boundary names go to the parent, refused names throw and are recorded on the loader, and other names use normal parent-first lookup. A stock parent-first `URLClassLoader` would fall back to its own URLs after the parent misses, so a plugin could vendor `histos/schedule/Sendable.class`. The recorded refusal becomes `PluginLoadError.EngineLeak` during load and `PluginRunError.EngineLeak` during a run.

The loader rejects plugin classes in reserved packages:

- An entry name under `histos`, `zio`, or `scala` is `PluginLoadError.ReservedPackage`.
- Any other `.class` under `histos/`, `zio/`, or `scala/`, in the direct jar or in a class directory compiled from sources, is `EngineLeak`.

`central` keeps package `histos.central` because it is never loaded as a plugin. A future Central plugin needs a package outside `histos`, or the rule needs an explicit exception.

Plugins on one `BuildLoader` see each other. That is ordinary library visibility, which a build file needs in order to import two plugins. The boundaries that matter are the engine and project compiles.

When plugin code reaches for an engine class:

- At compile time the engine is off the classpath, so scalac fails before a jar exists.
- At load or `run`, `ApiBoundary` does not define it, and `BuildLoader` does not search plugin URLs for it. Linkage failures are caught around entry load and around `run`. A refused `histos.*` name is `EngineLeak`, and any other missing class during a run is `PluginRunError.Linkage`. The task fails and nothing is cached.

That guarantee covers parent delegation and classes defined from plugin URLs. Engine types remain reachable by other means. `./histos` runs with `java -jar`, so the system class loader is the assembly, which contains `histos.schedule` and the rest. `ClassLoader.getSystemClassLoader.loadClass("histos.schedule.Sendable")` still links the engine. The engine sets the thread context class loader to `BuildLoader` while it evaluates `graph` (which calls `contribute`) and on the thread that starts `run`, and restores the previous loader afterwards, including on failure and interruption. The `LoadSpec` test that vendors `histos/schedule/Sendable.class` covers the plugin-URL path, and nothing covers `getSystemClassLoader`.

### Compile classpath

The build file, and every plugin compiled from sources, compile against `Loader.buildClasspath(apiJar)`, which is `Boundary.jars`:

- the `histos-api` jar the launcher loads
- `scala-library`, the code source of `scala.collection.immutable.Nil`
- `scala3-library`, the code source of `scala.quoted.Quotes`
- `zio`, the code source of `zio.ZIO`
- `zio-stacktracer`, the code source of `zio.internal.stacktracer.Tracer`, because `ZIO.succeed` summons a `Trace` from it
- `izumi-reflect`, the code source of `izumi.reflect.Tag`, because `zio.Tag` extends it and a compile that sees `Tag` without it aborts

Scala 3.10 class files link `scala.runtime` types from `scala3-library`, so a classpath of `scala-library` alone fails to define `build$` for any Scala 3.10 build. `LoadSpec` loads an enum build with no plugin to keep that list honest.

With plugins, `Driver.process` compiles the temp file against those jars plus each plugin's classpath. `Driver.process` takes one flat `-classpath`, where parent-first delegation does not apply, so every plugin classpath leaves out the cell jars the boundary already supplies. `Boundary.supplied` names them: `scala-library`, `scala3-library_3`, `zio_3`, `zio-stacktracer_3`, `izumi-reflect_3`, and any name starting with `histos-api`. A published copy would otherwise be the one the compiler links.

### Project compiles

Project compiles never receive plugin URLs. `GraphCompile` keeps the module classpath (upstream class directories, then locked jars). `JvmCompile.jarBan` and `directoryBan` reject any classpath entry that contains `META-INF/histos/plugin`, the marker every direct plugin jar has and the api jar lacks. The ban is `ClasspathBan.Histos`, the same ban as `histos/HistosBuild.class`. `directoryBan` skips the `HistosBuild` marker on purpose, because a classes directory may contain the build trait. The plugin marker gets no skip. A module that lists a plugin coordinate in `libraryDependencies`, or depends on a directory that holds the marker, fails to compile.

## How a jar is chosen and loaded

### Lock cells

A header coordinate resolves like a module cell: the direct jar plus every transitive jar, each with its own sha256, resolved for Scala 3.10.0 on the JVM. One checksum cannot carry a plugin that calls a markdown parser or any other library. `DependencyLock.plugins: List[LockedPlugin]` holds these cells, rendered after the module cells:

```text
plugin dev.earlyeffect specular 0.2.0 binary-or-source
direct dev.earlyeffect specular 0.2.0 binary
artifact dev.earlyeffect specular_3 0.2.0 <sha256> central
artifact dev.earlyeffect specular-text_3 0.2.0 <sha256> central
source dev.earlyeffect specular_3 0.2.0 <sha256> central
```

- The link tokens are `exact-binary`, `source`, and `binary-or-source`. `Api` has no token, because the scan refuses it before any write.
- `direct` and `artifact` reuse the module-cell grammar. The repository token is the existing `central`, `ivy-local`, or `maven:<url>`.
- `source` is the sources jar of the direct artifact: organization, published name, revision, sha256, repository. It is present for `Source` and `BinaryOrSource` and absent for `ExactBinary`.
- A lock of only plugin cells is valid. An empty file is `LockError.Empty`. A lock with no plugin section still parses.
- The lock wins over the header for the link strategy. Editing `.link(...)` has no effect until `histos resolve --update` rewrites the cell.
- Path plugins have no lock cell, and they compile against the boundary jars only. A path plugin that needs a third-party library belongs on a published coordinate.

### Resolve phases

`Loader.scan` reads the header. It does not check the lock, touch the network, or compile. Coordinates live on `ScannedBuild` and stay off the graph.

- `histos resolve --update` scans, then `Resolve.updatePlugins` resolves each header dep against Maven Central and writes the plugin cells into `.histos/dependencies.lock`, keeping the module cells. The write needs no `use`, no successful compile, and no `PluginRuntime`, and it happens only after every coordinate resolves. A failed resolve leaves the previous file. Then the build loads and `Resolve.update` rewrites the module cells, keeping the plugin cells. A later compile failure does not roll back the plugin write. `.link(Api)` fails at scan and writes nothing.
- `histos resolve --check` scans, refuses a missing plugin cell as `Unlocked`, re-resolves each plugin cell and fails on drift, then loads and checks the module cells. It never appends a line.
- `histos resolve` with no flag, `./histos test`, `./histos compile`, and every other command that loads the build refuse a missing cell as `Unlocked` and do not solve. They fetch each locked artifact by coordinate and fail on a checksum mismatch (`ResolveError.Checksum`). They never download a classifier the lock does not checksum: if the link needs sources and the cell has no `source` line, the error is `NoSource`.

### The entry resource

A binary plugin's direct jar carries two one-line resources, which the plugin author's build writes. Histos does not generate them.

- `META-INF/histos/plugin` (`HistosPlugin.entryResource`): the `EntryName` of the entry object.
- `META-INF/histos/version` (`ApiDigest.resource`): the hex `ApiDigest` of the api jar the plugin compiled against.

The direct jar is the cell artifact whose organization, `_3` name, and revision match the header. The loader opens it as a zip and reads both lines. It never walks transitive jars and never scans the build classpath, so a marker in a transitive jar is just a library file.

- No direct jar, a missing or empty entry resource, or a line that is not an `EntryName` is `NoEntry`.
- Two lines in the entry resource is `AmbiguousEntry`.
- A missing, empty, or multi-line version resource, or a line that is not a sha256, is `Unversioned`.

A path plugin keeps `META-INF/histos/plugin` at the root of its directory. The loader compiles every `.scala` file under that directory, copies every other regular file into the class directory at the same relative path, and reads the entry line from there. A path plugin has no version resource, and a missing directory is `NoEntry`.

### Link strategies

The loader decides each cell from the lock strategy and the two digests:

| Lock strategy | Digests match | Digests differ |
| --- | --- | --- |
| `ExactBinary` | `FromBinary` | `VersionMismatch` |
| `BinaryOrSource` | `FromBinary` | `FromSource`, or `NoSource` when the cell has no `source` line |
| `Source` | `FromSource`, or `NoSource` when the cell has no `source` line | the same |

`FromBinary` puts the direct jar and the cell's other jars, minus the boundary-supplied ones, on `BuildLoader` and on the build file's classpath. The entry class is defined from the direct jar. An exact match on `BinaryOrSource` takes this path quietly.

`FromSource` fetches the locked sources jar, checks its sha256, and compiles it before the build file, with the same in-process Scala 3 compiler that compiles `histos.scala`. Its scalac classpath is the boundary jars plus the cell's third-party jars. The direct jar is left off, and so is every cell jar the boundary supplies, including a published `histos-api` whose bytes differ from the running one. The resulting class directory replaces the direct jar on `BuildLoader` and on the build file's classpath, and the third-party jars stay on both. The classes `run` uses are the ones just compiled, including when `.link(Source)` compiled against a matching digest.

A path plugin always links as `FromSource`, with the running digest as both values.

Compiled plugin classes live under `<cache>/plugins/<digest>`. The digest covers the running `ApiDigest`, the compiler version, the source bytes (the sources jar, or the path-plugin tree), and the cell's artifact checksums. A directory is reused only after its `.complete` marker exists. Builds never delete these directories. `histos cache list` lists them. `histos cache clean` removes those unused for `ContentStore.Retention` (30 days) whose digest is not on the store's keep list, and `--dry-run` previews. Every load that compiles or reuses a plugin directory adds its digest to that keep list, so `clean` keeps every plugin directory a load has used.

### Entry checks

After the build file compiles and before `graph` runs, the loader defines each header entry class on `BuildLoader` and reads its public `MODULE$` field, so the entry must be a public object. A `MODULE$` the loader cannot read, or one that is not a `HistosPlugin`, is `NoEntry`. A failure while defining the entry class is `PluginLoadError.Defect`.

After `graph` returns, the loader loads each registered entry class from `BuildLoader` and checks it:

- Its code source must be the header's direct jar (`FromBinary`) or the compiled class directory (`FromSource` and path plugins). Anything else is `ForeignEntry`, for example a plugin object defined in `histos.scala` itself.
- Its `EntryName` must equal the header's entry line, or the error is `NotEntry`.
- The object's `id` must equal the registered id, or the error is `IdMismatch`.
- A class the loader cannot find is `Undeclared`.

## Running a task

### The runtime

Excerpts from `engine/src/histos/load/LoadedBuild.scala` and `engine/src/histos/load/PluginRuntime.scala`:

```scala
final case class LoadedBuild(graph: ProjectGraph, plugins: PluginRuntime)
```

```scala
final case class PluginLink(id: PluginId, linked: PluginLinked, artifacts: List[String])

trait PluginRuntime:
  def run(id: PluginTaskId, args: PluginArgs): ZIO[ActionCache & RemoteCache & Resolve, PluginRunError, PluginOutput]
  def bind: ZIO[ActionCache & RemoteCache & Resolve, Nothing, Map[TaskId, IO[TaskError, TaskResult]]]
  def links: List[PluginLink]
```

`PluginRuntime` is an engine type, off `histos.api`. `Loader.load` builds it while it still holds `BuildLoader`, and the load `Scope` closes that loader through an `acquireRelease` finalizer. Nothing reads the classloader back out of the `Scope`. `links` lists each loaded plugin with its link and its runtime classpath. `bind` maps every plugin task id to a run with `PluginArgs.empty`. Today only `LoadSpec` calls `bind`.

### Task ids and dispatch

`TaskId.Inspect` and `TaskId.Versions` are the bare strings `inspect` and `versions`, and `TaskId.from` stays a loose non-empty parser. Plugin keys are `PluginTaskId.render`, which always contains a slash.

- CLI: `histos run <plugin>/<task>` parses a single-slash id as `Command.Plugin(id, PluginArgs.empty)`. A platform token is `CliParseError.TaskPlatform`, and an id that does not parse is `BadPluginTask`. Bare `histos run <module>` still runs a main. A contributed command also becomes `Command.Plugin`.
- Server: the CLI sends `Call.Task(id.render)` to a running `./histos server`, which runs the task on its held graph. With no matching server, the CLI loads the build and runs the task in process.
- MCP: the `run` tool sends `Call.Task` for a slash id, to the server or to the MCP process's own held graph. `cancel` interrupts the fiber that `Calls.tracked` forked for that id. There is no plugin-specific MCP tool and no BSP target.
- `Call.Task` carries only the id, so a run through the server or MCP receives `PluginArgs.empty`. Command arguments reach the runner only in process.

`histos graph`, the server's graph call, and the MCP `graph` tool print the same text (`GraphText.render` in `app/src/histos/cli/GraphText.scala`). After the project lines, each plugin prints:

- `plugin <id> binary`, or `plugin <id> source <published> <running>` for a `FromSource` link
- `task <plugin>/<task> <seal> <needs> <paths>`, where `<paths>` lists declared `File` and `Directory` paths and never env values
- `command <name>` and `job <name>`

`./histos server` holds the loaded graph. Before each call it compares `Fingerprint.read` with the held fingerprint, and a change calls `Loader.load`. Success swaps the generation and closes the previous build classloader after the last call holding it finishes. Failure keeps the previous graph and is that call's error. `Fingerprint.read` hashes `histos.scala`, `.histos/dependencies.lock`, and every regular file under each header `plugin(path)` directory, in sorted relative order. The lock hash covers binary plugins.

### The action

Excerpt from `engine/src/histos/schedule/Action.scala`:

```scala
enum Policy:
  case Core
  case Plugin(seal: Seal, needs: Set[Need], root: Option[Path] = None, outputs: List[RepoPath] = Nil)

final case class Action(
    id: ActionId,
    kind: TaskKind,
    inputs: List[Input],
    dependencies: List[ActionId],
    policy: Policy = Policy.Core,
)
```

`TaskKind` (`engine/src/histos/task/Task.scala`) is `Inspect`, `Compile`, `Versions`, or `Plugin`. Each plugin task is an `Action` with `TaskKind.Plugin` and `Policy.Plugin(seal, needs, Some(repo), outputs)`. `root` and `outputs` drive the file check on a cache hit and stay out of the digest. `Digest.of` appends a `plugin <seal> <needs>` line only for `Policy.Plugin`, so a seal change invalidates the key and `Policy.Core` keys keep their text.

Excerpt from `engine/src/histos/schedule/Sendable.scala`:

```scala
  def ofAction(action: Action): Verdict =
    action.policy match
      case Policy.Core =>
        val runner = action.kind match
          case TaskKind.Compile  => Runner.Compile
          case TaskKind.Inspect  => Runner.Inspect
          case TaskKind.Versions => Runner.Versions
          case TaskKind.Plugin   => Runner.Plugin(Seal.Open)
        decide(runner, Set.empty)
      case Policy.Plugin(seal, needs, _, _) =>
        decide(Runner.Plugin(seal), needs)
```

A `TaskKind.Plugin` without a plugin policy counts as `Seal.Open` and stays local. `decide` returns `Send` for `Hermetic` with no needs. Any `Need`, or `Open`, is `Stay`.

### The digest

The key covers the declared inputs, hashed by the engine, plus inputs the plugin cannot omit:

- each entry on the plugin's runtime classpath (`PluginLink.artifacts`): a jar's sha256, or a class directory's tree hash
- the running `ApiDigest`
- `PluginTaskId.render`
- for each `TaskDep.Compile`, the class directories of the selected cells
- the digests of the actions it depends on, and the seal and needs line

Declared inputs contribute:

- `File`: the file's sha256
- `Directory`: a hash over each regular file's relative path and sha256, sorted by relative path
- `Module`: the class-directory hashes of the selected cells
- `Note`: the text of that module's note
- `Text`: the literal
- `Env`: the sha256 of the value

`Digest.of` sorts inputs by name. A plugin that reads an undeclared file does not change the digest. The key is a declaration, and nothing sandboxes it: `Sendable` still says `Send` for a hermetic task that lied and opened a socket, and the residual risk is a poisoned cache. `histos graph` prints each task's seal, needs, and declared paths so a reviewer can see the claim.

### The request

Excerpt from `api/src/histos/plugin/Request.scala`:

```scala
final case class ModuleView(
    name: ProjectName,
    baseDirectory: String,
    notes: List[ModuleNote],
)

final case class NamedText(name: ParamName, value: String)

final case class PluginArgs(texts: List[NamedText], flags: Set[FlagName]):
  def get(name: ParamName): Option[String] =
    texts.collectFirst:
      case NamedText(key, value) if key == name => value

  def flag(name: FlagName): Boolean =
    flags.contains(name)

  def require(name: ParamName): Either[PluginRunError, String] =
    get(name) match
      case Some(value) => Right(value)
      case None        => Left(PluginRunError.MissingArg(name))

object PluginArgs:
  val empty: PluginArgs = PluginArgs(Nil, Set.empty)

enum MaterialInput:
  case File(path: RepoPath, sha256: ContentHash)
  case Directory(path: RepoPath, sha256: ContentHash)
  case Module(name: ProjectName, sha256: ContentHash)
  case Note(module: ProjectName, key: NoteKey, value: NoteText)
  case Text(name: InputKey, value: String)
  case Env(name: EnvName, value: String)

final case class WrittenFile(name: OutputName, path: RepoPath)

final case class PluginOutput(text: String, files: List[WrittenFile])

final case class CellClasspath(
    module: ProjectName,
    version: ScalaVersion,
    platform: Platform,
    compile: List[Path],
    test: List[Path],
)

final case class RunRequest(
    id: PluginTaskId,
    repo: Path,
    modules: List[ModuleView],
    args: PluginArgs,
    inputs: List[MaterialInput],
    compiled: List[CellClasspath] = Nil,
)
```

- `modules` has one `ModuleView` per module, and `ModuleView.notes` holds only the running plugin's notes.
- `inputs` is each declared input after hashing, in declaration order. `Env` carries the raw value, because the runner has to read what it declared. The raw value never reaches the manifest, the workflow, or `histos graph`.
- `compiled` has one `CellClasspath` per cell the task's `TaskDep.Compile` entries select. `compile` is the main class directory, upstream class directories, the JVM standard library, and compile-scope jars. `test` starts with the test class directory and ends with test-scope jars when the module has test sources, and equals `compile` otherwise.

### Execution

`PluginRuntime.run` plans the plugin actions, then runs the requested task through `Scheduler.execute` (`engine/src/histos/schedule/Scheduler.scala`):

1. Every cell that a reachable task names in `TaskDep.Compile` compiles first, test sources included when the module has them. A compile failure fails the run with `PluginRunError.Failed`.
2. Dependencies run before dependents. A `TaskDep.Same` sibling runs with `PluginArgs.empty`, and only the requested task receives the caller's arguments. A dependency cycle fails the run with `PluginRunError.Failed` carrying the scheduler's cycle message.
3. For each plugin action the scheduler looks in the remote cache when one is on, then in the local action cache. A hit for a task with declared outputs must find every declared file under the repo with the sha256 the manifest recorded. A missing or changed file is a miss. A text-only hit (`outputs = Nil`) is always accepted.
4. On a miss, the engine hashes the inputs, builds the `RunRequest`, sets the context class loader, and calls `plugin.run(request)` inside `ZIO.scoped`, on the task fiber.
5. After success, every `WrittenFile` must name a declared output (otherwise `ExtraOutput`), and every declared output must exist as a regular file (otherwise `MissingOutput`). A failed check stores nothing.
6. The stored value is the manifest (`PluginManifest`): `PluginOutput.text` when `outputs = Nil`, or a `histos-manifest v1` header, the file count, one `<sha256> <path>` line per declared output, then the text. Both caches store only that UTF-8 text, and output bytes stay in the work tree.
7. `Policy.Plugin` stores a result only when the verdict is `Send`. A `Stay` result is returned and never written. `Policy.Core` stores every local success and uploads only a `Send`.

On a hit, `run` returns the stored text, plus a `WrittenFile` for each declared output.

The CLI and the server run plugin tasks against the local action cache in the content store, with the remote cache off. Output file bytes never travel through `RemoteCache`. Another machine that receives a manifest without the files misses and runs locally.

### Failure and cancellation

Interruption closes the inner `Scope`, so `acquireRelease` in the plugin runs its release. A plugin that starts a process ties it to that `Scope`, and the engine installs no shutdown hook for it. Failure, a defect, interruption, a missing output, and linkage store nothing. Interruption stays interruption: CLI exit `CliExit.Interrupted` (130), with no `PluginRunError`. Partial files may remain in the work tree. They never count as a hit, and the next run overwrites them. There is no transactional output directory.

- A defect in `contribute` is `BuildError.PluginDefect`, then `LoadError.BrokenGraph`, exit 3.
- A failure while defining the entry class is `PluginLoadError.Defect`, then `LoadError.Plugin`, exit 3.
- A `Die` inside `run` is `PluginRunError.Defect(binary)`, exit 1. A `ClassNotFoundException` or `NoClassDefFoundError` is `Linkage` instead.

Each carries a class name in `message`. The CLI prints `message` and never branches on exception text.

## Commands

`Grammar.root` (`app/src/histos/cli/Grammar.scala`) is a closed zio-cli tree built in source. It cannot grow a subcommand per plugin, and `Cli.execute` parses before `Loader.load`, so the grammar never sees plugin specs. A dynamic zio-cli tree has the same ordering problem. Plugin commands use one fallback, on an unknown root word.

`Grammar.reject` maps a zio-cli `CommandMismatch` to `CliParseError.UnknownRoot` when the first token is outside `CommandName.reserved`, and to `UnknownCommand` when it is inside (`histos cache nope`). Flag and argument errors keep their own cases (`histos test --nope`). Only `UnknownRoot` loads the build.

1. The built-in grammar runs unchanged. A success never consults plugins.
2. `CommandName.reserved` is the root vocabulary: `version`, `graph`, `run`, `test`, `testOnly`, `testQuick`, `compile`, `assembly`, `publish`, `mcp`, `acp`, `bsp`, `server`, `toolchain`, `cache`, `resolve`, `workflow`, `reload`, `reboot`. `acp` has no built-in command today and stays reserved so a plugin cannot claim the word. `Projects` rejects a plugin command with a reserved name (`CommandShadow`). `path`, `list`, and `clean` are cache subcommands and stay free at the root, so `histos list` can be a plugin command while `histos cache list` stays built-in.
3. On `UnknownRoot`, `Cli.execute` loads the build and calls `PluginCommands.parse` with every `CommandSpec` in `graph.plugins`, each bound to its plugin.
4. A match parses the remaining tokens with the spec's `CliParam`s and becomes `Command.Plugin(id, args)`. No match, or a parameter error, prints usage with each plugin command and its summary appended, exit 2.
5. `~` applies the same rule to the inner tokens. `~ test` never loads in order to decide the command. `~ site` loads once, the same as `site`, and wraps the result in `Command.Watch`.

Excerpt from `app/src/histos/cli/PluginCommands.scala`:

```scala
final case class BoundCommand(plugin: PluginId, spec: CommandSpec)
```

`PluginCommands.parse(commands: List[BoundCommand], args: List[String])` returns `Either[CliParseError, (PluginTaskId, PluginArgs)]`. The argv schema is the spec's data, and the plugin never parses argv:

- `CliParam.Text` parameters are required positionals, in declaration order.
- At most one `CliParam.OptionalText` follows them. A spec with two is rejected every time it is invoked.
- `CliParam.Flag(name)` is the boolean `--name`, anywhere in the tokens. An unknown flag is rejected.
- Too few positionals is `MissingArgument`, and too many is `UnexpectedArguments`.

`histos run <plugin>/<task>` passes `PluginArgs.empty`. A runner that needs an argument and starts through `run` returns `PluginRunError.MissingArg` from `PluginArgs.require`.

A typo of a root word (`histos tst`) loads the build before printing usage, so a broken `histos.scala` on that path exits 3 (`LoadFailed`). That is the cost of plugin commands being data loaded from the build. `histos version` and `histos cache nope` do not load.

## CI jobs

`Workflow.document` (`engine/src/histos/workflow/Workflow.scala`) renders `ci.yml` as the `test` job, a `versions` job when the catalog is non-empty, one job per `JobWhen.Always` plugin job, then the `workflow` drift job. The plugin never writes YAML:

```yaml
  docs:
    runs-on: ubuntu-latest
    permissions:
      contents: read
    steps:
      - uses: actions/checkout@v4
      - run: "./histos run dev.earlyeffect.specular/site"
```

The step is `./histos run` plus the rendered task id, quoted the same way as test cells, so CI does not depend on `CommandSpec`. `JobPermissions.ContentsRead` renders `contents: read`, the default and the workflow header's value. `ContentsWrite` renders `contents: write` on that job only. No other permission token exists, so a plugin cannot emit `id-token: write`.

`JobWhen.Schedule(cron)` renders a separate `.github/workflows/histos-<job>.yml`, because `cron` is a workflow-level key and would schedule `test` too. That file has `on.schedule` with the cron, a top-level `permissions: contents: read`, and the same job.

Excerpt from `engine/src/histos/workflow/Workflow.scala`:

```scala
final case class RenderedWorkflow(name: String, body: String)
```

- `Workflow.render(graph, setup)` returns the `ci.yml` text. `Workflow.renderAll(graph, setup)` returns `ci.yml` plus every scheduled file.
- `histos workflow --check` fails with `CiError.Drift` when a rendered body differs, when a scheduled file is missing, or when a `histos-*.yml` exists that `renderAll` did not emit.
- `histos workflow --update` writes every rendered file and deletes those orphans. A workflow with any other name stays. Removing a `CiJob` stops its schedule on the next update, and `--check` fails until then.

A dependency-update bot fits this shape: `Seal.Open` or `Need.Network`, `JobWhen.Schedule`, never sent and never cached.

## What the author tests

Excerpt from `api/src/histos/plugin/PluginFixture.scala`:

```scala
object PluginFixture:
  def request(
      plugin: PluginId,
      task: TaskName,
      repo: Path,
      modules: List[ModuleView] = Nil,
      args: PluginArgs = PluginArgs.empty,
      inputs: List[MaterialInput] = Nil,
  ): RunRequest =
    RunRequest(PluginTaskId(plugin, task), repo, modules, args, inputs)
```

`PluginFixture` builds a `RunRequest` and does nothing else: it loads no jar, opens no scheduler, and hashes nothing. The author passes the `MaterialInput` values the test cares about. `compiled` is always empty.

A plugin suite is ZIO Test (`ZIOSpecDefault`) on `histos.api` and `zio-test`. ZIO Test provides `Scope`. The suite calls `plugin.run(PluginFixture.request(...))` and asserts on the `PluginOutput` or the `PluginRunError`.

The in-tree fake is `Echo` in `api/test/PluginSpec.scala`, so testing the api module compiles it without the engine. Written outside `histos.plugin`, it needs two imports:

```scala
import histos.plugin.*
import zio.{Scope, ZIO}

object Echo extends HistosPlugin:
  val id: PluginId = PluginId("test.echo")

  def contribute: Contribution =
    Contribution(
      tasks = List(DeclaredTask(
        name = TaskName("echo"),
        seal = Seal.Open,
        needs = Set.empty,
        inputs = Nil,
        outputs = Nil,
        dependsOn = Nil,
      )),
      commands = Nil,
      jobs = Nil,
    )

  def run(request: RunRequest): ZIO[Scope, PluginRunError, PluginOutput] =
    ZIO.succeed(PluginOutput("echo", Nil))
```

`PluginSpec` registers it with `Projects(use(Echo), module("core", Jvm)(scala"3.9.0"))`-shaped builds and asserts the graph holds the task. A second fake, `Quiet`, throws from `run`, which proves `Projects` never calls `run`. `Echo` stays a single class with `ZIO.succeed` and is not a loader test.

## Worked example: a Specular-shaped fixture

`engine/test/SpecularFixtureSpec.scala` is a test fixture shaped like the Specular plugin, so the build-file shape is real. It is not the Specular repository, and it is not part of the engine. The id is `dev.earlyeffect.specular`. The `site` task is `Seal.Open`, because a preview reads the work tree and writes a site.

A second jar, `dev.earlyeffect::specular-text`, holds:

```scala
package dev.earlyeffect.specular.text

object SiteText:
  def render(root: String): String = s"site:$root"
```

The plugin jar:

```scala
package dev.earlyeffect.specular

import dev.earlyeffect.specular.text.SiteText
import histos.{ModuleDef, ProjectName}
import histos.plugin.*
import zio.{Scope, ZIO}

object Specular:
  val plugin: HistosPlugin = SpecularPlugin

  def site(module: ModuleDef): ModuleDef =
    module.note(plugin.id, NoteKey("role"), NoteText("site"))

object SpecularPlugin extends HistosPlugin:
  val id: PluginId = PluginId("dev.earlyeffect.specular")

  def contribute: Contribution =
    ProjectName.from("docs") match
      case Right(docs) =>
        Contribution(
          tasks = List(DeclaredTask(
            name = TaskName("site"),
            seal = Seal.Open,
            needs = Set.empty,
            inputs = List(
              DeclaredInput.Directory(RepoPath("docs")),
              DeclaredInput.Note(docs, NoteKey("role")),
            ),
            outputs = List(DeclaredOutput(OutputName("site"), RepoPath("docs/index.html"))),
            dependsOn = Nil,
          )),
          commands = List(CommandSpec(CommandName("site"), Nil, TaskName("site"), PluginDetail("write the site"))),
          jobs = List(CiJob(JobName("docs"), TaskName("site"), JobWhen.Always)),
        )
      case Left(_) => Contribution(Nil, Nil, Nil)

  def run(request: RunRequest): ZIO[Scope, PluginRunError, PluginOutput] =
    val text = SiteText.render("docs")
    ZIO.succeed(PluginOutput(text, List(WrittenFile(OutputName("site"), RepoPath("docs/index.html")))))
```

- `SpecularPlugin` is a public object, because the loader reads its public `MODULE$` field.
- The package `dev.earlyeffect.specular` is the build file's import.
- `ProjectName` has no literal, so `contribute` builds the name with `ProjectName.from`. The literal `"docs"` always parses, and the `Left` branch keeps `contribute` total.
- `run` calls `SiteText.render` from the second jar, so a loader that put only the direct jar on `BuildLoader` would fail that call with `PluginRunError.Linkage`.
- The declared output is the file `docs/index.html`. `ProjectPath` rejects the dot in `index.html`, and `RepoPath` accepts it.

The fixture's build file:

```scala
plugin(dep"dev.earlyeffect::specular:0.2.0")

import histos.*
import dev.earlyeffect.specular.Specular

object build extends HistosBuild:
  def graph =
    val docs =
      module("docs", Jvm)(scala"3.9.0")
        .pipe(Specular.site)
    Projects(use(Specular.plugin), docs)
```

The test publishes both jars into a temp Maven directory, runs `Resolve.updatePlugins` against it, loads the build, and asserts:

- the link is `FromBinary` with `specular_3` and `specular-text_3` on the plugin classpath, and the lock cell lists both checksums plus a `source` line
- the registered entry is `dev.earlyeffect.specular.SpecularPlugin$`, with the `site` command, the `site` task and its output, and the `docs` job
- the `docs` module carries the `role` note
- `run` returns `PluginOutput("site:docs", ...)`, so `SiteText.render` ran
- the rendered workflow contains `./histos run dev.earlyeffect.specular/site` and none of the plugin's source text
- `Sendable.decide` on the task's `Seal.Open` is `Stay(Hold.Plugin)`, so a later edit cannot start uploading docs

## Security and Privacy

A plugin is code the build author enabled by coordinate or by repo path. It runs in the launcher process, as the user who ran `./histos`, on the build classloader. Explicit `use` is the trust boundary, and nothing sandboxes it. A hostile plugin that the author named can read the repo, the environment, and the network through ordinary JDK APIs, whether or not it declared `Need.Network`. `ApiBoundary` stops parent delegation and classes defined from plugin URLs from linking engine packages. `getSystemClassLoader` under `java -jar` still sees the assembly, and the context loader does not stop `java.lang.ProcessBuilder` or `java.nio.file`. Severity of a hostile enabled plugin is high. The mitigation is the locked cell (every jar is pinned) and review of the coordinate. The classloader is not a security sandbox.

`Need` and `Seal` are the cache boundary. `Network` and `Docker` keep a task off the remote cache, so a task that admits it talks to the network never uploads its output. A task that lies is the medium-severity cache-poison case under Known limits. Credentials (Central, a deploy token) are never manifest inputs and never appear in a workflow file, whose step is only `./histos run <id>`. A runner reads credentials from the environment when it declares `DeclaredInput.Env`, or from a path it declares. Undeclared secrets do not enter the key, and nothing protects them from the plugin either. The graph printer never prints env values.

`plugin(path)` takes a `ProjectPath`, so the sources are under the repo. A path that escapes is `BadCoordinate` at scan time, and the header accepts no absolute path.

`ContentsWrite` on a job is a capability the author opted into by `use`ing a plugin that declared it. The default job permission is `contents: read`, the same as `ci.yml`. A plugin cannot raise permissions with a free string.

Plugin classes on a project classpath would let project code call the plugin, and would let a published jar depend on Histos. `META-INF/histos/plugin` is banned in jars and in class directories, and the api jar is banned through `HistosBuild`.

The digest check refuses a binary built against a different api jar. It is an integrity check for the api surface and carries no signature. Jars come from the repository named in the lock and are checksummed the same way locked compile jars are (`ResolveError.Checksum`).

## Observability

Histos has no metrics stack, and plugins add none. The signals are CLI output, `histos graph`, MCP's `graph` and `run` tools, and the workflow drift check.

- An exact digest match is quiet.
- A `FromSource` link prints nothing during load. `histos graph` shows `source <published> <running>` on the plugin line.
- `VersionMismatch`, `ApiLink`, `NoSource`, `EngineLeak`, and `Unlocked` print the error `message`, which names the header coordinate. `VersionMismatch` also names both digests. The exit is `CliExit.LoadFailed` (3), through `LoadError.Plugin` or `LoadError.BrokenGraph`.
- A plugin task failure, including `PluginRunError.Unknown` and `PluginRunError.Defect`, prints `message` and exits `CliExit.TaskFailed` (1). Interruption exits 130.
- A cache hit is quiet, the same as a compile cache hit. A `Stay` task logs no cache decision.
- Workflow drift is `CiError.Drift` when a rendered plugin job changes a committed file, or when an orphan `histos-<job>.yml` remains.

`PluginLoadError.Defect`, `BuildError.PluginDefect`, and `PluginRunError.Defect` print the exception class name and never the exception message. The class name and the plugin's coordinate or id are enough to open the plugin's own test.

## Known limits

| Limit | Severity | Mitigation |
| --- | --- | --- |
| `java -jar` leaves the assembly as the system loader, so `getSystemClassLoader` still links `histos.schedule`. | Medium | Outside the `ApiBoundary` guarantee. The context loader is `BuildLoader` while `graph` runs and when `run` starts. |
| A hermetic plugin reads an undeclared file or opens a socket, and the digest stays the same. | Medium | The declaration is the key. `Stay` results are never stored. `histos graph` shows the claim. There is no sandbox. |
| `UnknownRoot` loads the build, so a root typo pays for a load and a broken build reports exit 3. | Low | Only `UnknownRoot` loads. `histos cache nope` and `histos test --nope` do not. |
| ZIO on `api` widens the jar that projects might touch. | Medium | Projects are banned from the api jar through `HistosBuild`, and from plugins through `META-INF/histos/plugin` in both `jarBan` and `directoryBan`. The launcher's ZIO wins at runtime. |
| The header scan misses a `plugin(dep)` written as ordinary Scala. | Medium | The compiler sees blank lines where the header was. A later header form is `Misplaced`. Otherwise the build fails closed: the import does not compile, or `use` is `ForeignEntry`. |
| `central` in `histos.central` cannot be loaded as a plugin under the package rule. | Low | Central is not a plugin. A later wrapper picks a package outside `histos`, or changes the rule on purpose. |
| Adding policy to `Digest.of` could change compile cache keys. | Medium | Only `Policy.Plugin` adds a line. `Policy.Core` keeps its text. |

[roadmap.md](roadmap.md) lists the open plugin-isolation and plugin-step work.

## Alternatives Considered

### sbt AutoPlugin

The familiar model: `plugins.sbt`, an AutoPlugin object, `trigger` (`allRequirements`, `noTrigger`), `requires`, and `SettingKey` / `TaskKey` updated with `:=` and `++=`. A plugin is on when its jar is on the meta-build classpath and its trigger says so. Settings appear in projects the build file never mentioned.

Rejected. The trigger is the bug. In Histos the graph is the list of projects the file passed to `Projects`, fixed when load finishes, and a plugin that adds settings the author did not call breaks that. `requires` is the same bug one level up: enabling A silently enables B. Scope axes exist so one key can mean different things in `Compile`, `Test`, and `ThisBuild`. Histos settings are functions on `ModuleDef`, and test dependencies are their own method (`.testDependencies`). Calling the plugin as Scala is impossible while the surface is a key macro, and the compatibility surface would be sbt itself.

The idea Histos keeps is a named object in a jar, loaded because the header named the coordinate.

### Just call a main class

`java -cp specular.jar dev.earlyeffect.specular.Main`, or a `histos` subcommand that starts that main with `ProcessBuilder`. No trait, no contribution, no graph.

Rejected. A main class has no `ModuleDef => ModuleDef`, so `.pipe(Specular.site)` cannot exist. It declares no `Seal` or `Need`, so `Sendable.decide` cannot see it, and the safe default would be to never cache it (deleting the hermetic case) or to always cache it (uploading a network task). It produces no CI job the renderer can emit, so either the plugin writes YAML or the engine special-cases Specular. Its arguments are an `Array[String]` inside the plugin, so the CLI cannot reject a bad flag before the process starts. Failure collapses to a process exit code with no `PluginRunError`. Cancellation becomes a process kill the runner has to invent, where a `Scope` already handles it. The main's classpath is whatever the author put on the command, which is how engine classes leak.

A main class is the right shape for a tool a task runs as a subprocess (a node binary, a linker). The Native linker works that way: a child process calls Scala Native, and the engine never loads `scala.scalanative`. That boundary is a different one.

### Mill-style magic imports

`import $ivy.`...`` inside the build file, so one compile both resolves and imports. Rejected. It is a second language hidden in an import-shaped comment, the same split as `plugins.sbt` with worse tooling. Histos already has `dep"..."` literals and a lock file, and the header reuses both. A magic import would grow a resolver inside the compiler wrapper and would still need an explicit `use` to avoid AutoPlugin triggers. The header is one mechanism for one decision.

### Dynamic zio-cli grammar

Build `CliCommand` values from `CommandSpec` and pass them to `root.subcommands`. Rejected as the parsing strategy. The built-in tree is compile-time and parsing runs before load, so the dynamic tree is empty until a second parse after load. That second parse is the fallback Histos already has, without teaching zio-cli a data schema and without the risk of a plugin command beating `test` because it registered first. `PluginCommands` is smaller than a second zio-cli construction.

### One checksum line per plugin

A single `plugin org name revision sha repo` line, with that one URL on `BuildLoader`. Rejected. Specular, and any plugin that is an ordinary library, links types from other jars. A markdown parser is not a boundary name. `ZIO.succeed` in a fixture hides the miss until `run` fails with `Linkage`. The module lock already records directs plus transitive artifacts, each checksummed, and a plugin coordinate uses that cell. The entry resource stays on the direct jar, so a transitive library is never a second plugin.

### A sealed assembly as the only plugin jar

Shade every dependency into the direct jar and keep one checksum. Rejected. Shading fights parent-first ZIO and the reserved-package rule: a shaded `zio.*` or `scala.*` is either a duplicate the boundary already supplies, or a reserved-package refusal. It also needs a second lock format for bytes the module cell already pins. The cell wins because the resolver, the checksum check, and `test` / `compile` materialization already exist.

## Key Decisions

1. **`HistosPlugin` is one trait on `histos.api`.** `contribute` is data. `run` is `ZIO[Scope, PluginRunError, PluginOutput]`. One object is the fixed entry. The graph stores `RegisteredPlugin`, copied from `contribute`, and never the trait.
2. **`Seal` and `Need` live in `histos.plugin`.** Plugins cannot import `histos.schedule`, and a second pair of enums would drift from `Sendable.decide`. The policy function stays in the engine.
3. **Enablement is `plugin(...)` in a header plus `use(...)` on `Projects`.** The header is text. The loader blanks those lines into a temp file, and `Driver.process` compiles the temp file. `use` names the instances. `Projects` calls `contribute`, so a jar on the classpath cannot mutate the build.
4. **The classpath is a lock cell, and the entry resource is read from the direct jar only.** `META-INF/histos/plugin` is one line in the direct jar, opened as a zip. Transitive jars are libraries, each checksummed, and never scanned for an entry. `FromBinary` puts the cell jars on `BuildLoader`. `FromSource` replaces the direct jar with the class directory compiled from the locked sources and keeps the third-party jars. A classpath scan is the AutoPlugin bug.
5. **Task ids are `pluginId/localName`.** `TaskId.from` stays loose for engine use. The rendered plugin id always has a slash, so `inspect` and `versions` cannot be occupied, and `specular/versions` does not shadow `versions`. A slash id never reaches `program`.
6. **`ApiBoundary` is parented on the platform loader, and boundary names load from the engine host.** The compile classpath is the api jar, `scala-library`, `scala3-library`, ZIO, `zio-stacktracer`, and `izumi-reflect`. Allowed packages are the api root, `histos.version`, `histos.plugin`, `zio`, and `scala`. A second api `URLClassLoader` would make `ProjectGraph` a different class. Other `histos.*` names are refused before plugin URLs are searched. The system loader under `java -jar` is outside this boundary.
7. **Commands dispatch after the built-in grammar, on `UnknownRoot` only.** A mismatch under a reserved root word stays `UnknownCommand`. `~` applies the same rule to its inner tokens. `path`, `list`, and `clean` stay free at the root. Built-ins win twice: the grammar matches first, and `CommandShadow` rejects the shadow in `Projects`. The plugin never parses argv.
8. **CI jobs are data on the contribution, and `Workflow` renders them.** Scheduled jobs are separate `histos-<job>.yml` files, so `cron` never wraps `test`. An orphan of that shape is drift, and `--update` deletes it. The step is `./histos run <plugin>/<task>`.
9. **Cache eligibility is `Sendable.decide`.** The plugin-specific parts are `Policy.Plugin` on `Action`, the declared-file check on both hit paths, and the refusal to store a `Stay` result. Declared inputs plus the plugin classpath, the api digest, and the task id are the key. `outputs = Nil` stores `PluginOutput.text`. Undeclared reads go undetected.
10. **Until 1.0, binary plugins match the `ApiDigest` of the api jar being loaded.** That digest changes whenever the jar bytes change, and `0.0.1-SNAPSHOT` does not. `BinaryOrSource` compiles the locked source jar on mismatch, before `Driver.process`, and loads that class directory in place of the direct jar. `.link(Source)` compiles even when the digests match. `ExactBinary` mismatch is `VersionMismatch`. `Api` is `ApiLink` with no success path. `plugin(path)` compiles against the boundary jars, so it matches the running api by construction.
11. **`central` is not loaded by this loader.** It stays the in-tree module that compiles against `api` and not the engine. The fixture in this repo stays a fixture.
12. **Test providers and `nscplugin` stay outside.** They are different seams. Folding them in would put `sbt.testing` or a compiler flag api on `HistosPlugin`.
13. **Module settings are functions.** `pipe` applies `ModuleDef => ModuleDef`. Notes are the residual stored on `Module`, because the case class cannot carry a type member per plugin. A note input is `(ProjectName, NoteKey)`, and the runner sees only its own plugin's notes.
14. **ZIO 2.1.27 is a compile dependency of `api`.** The runner signature needs it. The launcher supplies the runtime copy, and plugins do not pin their own ZIO.
15. **`resolve --update` writes plugin cells before compile.** `scan` needs no successful load. `test`, `compile`, and `--check` refuse a missing cell and never download an unlocked jar.

## History

The plugin series landed as eight commits:

- `c6bafde` added the plugin types to `histos.api`: ids, `Contribution`, the link and error enums, `ApiDigest`, `RepoPath`, and `PluginFixture`, and moved `Seal` and `Need` into `histos.plugin`.
- `4a6bfd7` added `use`, `Uses`, `ModuleDef.pipe`, `ModuleDef.note`, `Module.notes`, `ProjectGraph.plugins`, the plugin `BuildError` cases, and the `Echo` fake.
- `d440ce0` added the header scan, the temp compilation unit, `ApiBoundary`, `BuildLoader`, plugin lock cells, `Resolve.updatePlugins`, `PluginRuntime`, `LoadError.Plugin`, and the `META-INF/histos/plugin` project-classpath ban, loading exact-binary plugins only.
- `449ef75` added `Command.Plugin`, `UnknownRoot`, `PluginCommands`, `Policy.Plugin`, `TaskKind.Plugin`, the declared-file check on cache hits, and scoped plugin runs.
- `8809552` rendered plugin jobs into `ci.yml` and `histos-<job>.yml`, with orphan drift and deletion.
- `844dcc4` added source and path plugins, the `plugins/<digest>` cache directory and its clean, path-plugin fingerprinting, and the `BinaryOrSource` default.
- `f50fa66` added the Specular-shaped fixture with two jars served from a temp resolver.
- `7eeacb3` compiled `TaskDep.Compile` cells before a plugin task and added `RunRequest.compiled`.

Later, `6dd46d3` made MCP a client of the server, so MCP runs plugin tasks through `Call.Task` and no longer installs `PluginRuntime.bind`. This repo's own `histos.scala` enables no plugin.

## References

- [architecture.md](architecture.md): the modules, the process model, and the error enums around this contract.
- [roadmap.md](roadmap.md): plugin isolation, plugin tasks as typed steps, the sandbox, and the real plugins (Specular, Central, docs, docker, deploy, coverage, the dependency-update bot).
- `api/src/histos/plugin/`: `HistosPlugin`, the ids, `Contribution`, `Request`, `Errors`, `PluginFixture`, and `PluginMacros`.
- `api/src/histos/Builder.scala`, `api/src/histos/ProjectDef.scala`, `api/src/histos/Graph.scala`: `use`, `pipe`, `note`, and plugin registration in `Projects`.
- `engine/src/histos/load/`: `HeaderScan`, `Loader`, `Boundary`, `BuildLoader`, `PluginJar`, `PluginCompile`, `PluginInputs`, `PluginCells`, and `PluginRuntime`.
- `engine/src/histos/resolve/DependencyLock.scala` and `engine/src/histos/resolve/Resolve.scala`: plugin cells, `updatePlugins`, `checkPlugins`, and `ranged`.
- `engine/src/histos/schedule/`: `Action`, `Policy`, `Digest`, `Sendable`, `Scheduler`, and `PluginManifest`.
- `engine/src/histos/workflow/Workflow.scala`: `renderAll`, plugin jobs, and orphan drift.
- `engine/src/histos/zinc/JvmCompile.scala` and `engine/src/histos/zinc/ClassMarker.scala`: the project-classpath bans.
- `engine/src/histos/cache/ContentStore.scala`: `Retention`, the keep list, and `clean` over `cas` and `plugins`.
- `app/src/histos/cli/Grammar.scala`, `app/src/histos/cli/Cli.scala`, `app/src/histos/cli/PluginCommands.scala`, `app/src/histos/cli/GraphText.scala`: dispatch, `UnknownRoot`, argv, and the graph text.
- `app/src/histos/server/Daemon.scala` and `app/src/histos/server/Calls.scala`: `Fingerprint.read`, reload, and `Call.Task`.
- `app/src/histos/mcp/HistosMcp.scala` and `app/src/histos/client/Session.scala`: the MCP `run` and `cancel` tools.
- `engine/test/LoadSpec.scala`, `engine/test/PluginRunSpec.scala`, `engine/test/SpecularFixtureSpec.scala`, `api/test/PluginSpec.scala`, `app/test/CliSpec.scala`: the tests that hold this contract.
