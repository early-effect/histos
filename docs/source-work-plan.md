# Source modules

Two products. A checkout is a module you are editing in another directory. A git library is a classpath jar built from a commit, so a Histos consumer can take an early-effect library without waiting for a Central publish.

A module edge stays `dependsOn`. A Maven edge stays a `Dep` in `.histos/dependencies.lock`. A git library is a third edge: the coordinate of a library, the bytes of a commit. The resolved graph and the lock hold a commit. A branch is an update input.

This file is the decision. The checklist is the work. Top to bottom. Finish one item and prove it before the next. The proof is the suite that item names, run with `./histos test` for the modules it touches. A fixture is two directories or a local git repository. Tests do not call GitHub.

## What stays closed

Matching sbt feature for feature stays closed in [next.md](next.md). These stay closed with it:

- A staging directory that is both the checkout and the cache. sbt's `RootProject(uri)` is that directory. [sbt#8880](https://github.com/sbt/sbt/pull/8880) put `Manual`, `Always`, and `Every(interval)` on top of it.
- `Always` and `Every`. Fetch runs when asked.
- Reading `build.sbt` or a Mill build. The other file may sit in the tree. Histos does not parse it.
- Splicing a foreign commit into this graph as a module. Their plugins, their Scala version, and their internal modules stay in the build that produces the jar.
- A setting that rejects source changes. A commit names one tree. A branch moves on update. The consumer rebuilds when the jar changes.
- A git coordinate that silently binds a local clone. The dirty tree is the checkout edge, written in the build file.
- An edge that omits the module name.
- A branch, a tag, or a dirty tree as a cache key.
- Installing git. `git` is on `PATH`, the way `java` and `node` are. Auth is that git's.

## Checkout

`dependsOn` takes an upstream in a directory you are editing. The files are the identity. The lock has no line for it.

[ProjectPath](../api/src/histos/ProjectPath.scala) is a directory under this repository. A segment that climbs fails. A sibling clone is a `CheckoutPath`: a relative path that may climb. An absolute path or a URI fails.

```scala
engine.dependsOn(upstream("core").at(checkout"../heddle"))
```

`upstream` is a `ProjectDef`. It is not a `ModuleDef`. This file does not set its Scala version, its platforms, or its plugins.

When that directory contains `histos.scala`, that file is the build. Load it for the named module and the modules it `dependsOn`. That load uses that build's plugins and that build's lock. This build's plugin classpath stays this build's. The named module joins the graph. Its compile key is the path relative to its tree, then the bytes, once [scenario-work-plan.md](scenario-work-plan.md) lands "Repo-relative sources".

When the directory has no `histos.scala`, the upstream carries an overlay: the source directory, the Scala version, and the library dependencies. The overlay is the build. `build.sbt` beside those sources is unread. An overlay on a tree that already has `histos.scala` fails at load. An overlay missing the source directory or the Scala version fails at load.

```scala
engine.dependsOn(
  upstream("core")
    .at(checkout"../zinc")
    .sources(path"internal/zinc-core/src/main/scala")
    .scala(scala"3.10.0")
    .libraryDependencies(dep"org.scala-lang::scala3-compiler:3.10.0")
)
```

These fail at load, as a typed error: no module of that name, a Scala binary mismatch, a platform the upstream does not build, a cycle, a library or application publication that takes the edge. The upstream's tests stay in its build. This edge compiles it.

`upstream` and `at` are the names. `module(...).from` collides with a module this file is defining. `VcsProject` is the staging directory. `Source` is already the Scala sources.

## Git library

The daily early-effect loop is a library on the classpath, not a second graph inside this one. Publishing that library to Central is the slow step. The git library skips that step for a consumer that builds with Histos.

The build file names a repository and either a branch or a commit. Both are legal. The lock stores the commit either way.

```scala
enum GitRequest:
  case Branch(name: Branch)
  case Commit(id: GitCommit)

engine.libraryDependencies(
  git"rocks.earlyeffect::heddle#core".at(repo"https://github.com/early-effect/heddle.git", branch"main")
)

engine.libraryDependencies(
  git"rocks.earlyeffect::heddle#core".at(
    repo"https://github.com/early-effect/heddle.git",
    commit"0123456789abcdef0123456789abcdef01234567",
  )
)
```

`git` is a coordinate: organization, artifact, module. The `#module` selects the module in that repository. Omitting it means the module name is the artifact name. `Dep` stays a Maven revision. A git library is not a `Dep`.

`resolve --update` is the fetch. It runs `git` on `PATH`.

- A `Commit` request fetches that object. The lock records that commit. Update refuses a different commit.
- A `Branch` request resolves the token with `git rev-parse`, stores the commit it got, and may replace the previous commit. The token is stored as written. The lock does not invent `refs/heads/`.

A later run reads the lock and does not talk to the remote. A missing `git` fails the update command. Compile of a stored jar does not look for `git`.

The lock line holds the organization, the artifact, the module, the repository, the request token, the commit, the tree hash, and the jar sha256. It lives in `.histos/dependencies.lock` beside the cells. One update writes Maven cells and git lines together.

The tree is checked against the recorded tree hash. The jar is checked against the recorded sha256. Either mismatch is a typed error. That is the whole integrity rule. A commit cannot change its tree, so there is no second policy for "source changes". A branch keeps the locked tree until update. Remote movement is invisible until then, and invisible movement is not a build error. Pin a commit when the tree must stay. Pin a branch when an update may move it.

The jar is the consumer's compile input, the same way a Maven artifact is. A new commit whose API digest is unchanged does not recompile the consumer. A commit that changes a signature does. A commit whose own compile fails, fails the consumer with that compile error. The library boundary does the work a "reject source changes" flag would do badly: a flag would fail a branch update that still compiles.

The jar is produced by compiling the named module from that commit's `histos.scala`. The consumer's resolve is the classpath. The upstream lock is what that repository uses when it is the root. It is not applied inside someone else's resolve. The module's dependency requests join the consumer's resolve. Internal modules it `dependsOn` are jars on that classpath, not modules of the consumer graph.

A commit with no `histos.scala` uses the same overlay as a checkout: source directory, Scala version, library dependencies, declared on the `git` value. An overlay beside a `histos.scala` fails at load. A missing source directory or Scala version fails at load.

The commit's files live in the content store, keyed by the tree. The jar lives there, keyed by its sha256 and by the commits it was compiled against. `./histos clean` deletes the consumer's work tree and leaves the store. A second consumer that resolved the same commits restores the jar and does not compile the upstream again.

## One module, one commit

A git commit has no version order. Two commits of one module do not sort, and the classpath carries one of them.

`a`, `b`, and `c` never publish. `d` depends on all three and publishes one jar. `b` depends on `a` at another commit. `d` names the commit that wins:

```scala
libraryDependencies(
  git"earlyeffect::a#a".at(repo"https://github.com/early-effect/a.git", commit"<h2>").unify,
  git"earlyeffect::b#b".at(repo"https://github.com/early-effect/b.git", commit"<hb>"),
  git"earlyeffect::c#c".at(repo"https://github.com/early-effect/c.git", commit"<hc>"),
)
```

`b`'s build file still asks for `a` at `h1`. That request joins `d`'s resolve. `b`'s lock stays in `b`'s repository for when `b` is the root.

With no `.unify`, resolve fails and names every requester and commit: `d` asked for `h2`, `b` asked for `h1`. `.unify` keeps that commit. The lock records the eviction in the shape [Eviction](../engine/src/histos/resolve/ResolveModel.scala) already has: the requested commit, then the selected commit. `b` is compiled against `a` at `h2`. If those sources do not compile, `d` fails with `b`'s compile error. The jar recorded for `b` is that compilation, so a later resolve that unifies `a` to `h1` does not restore it.

One requester needs no `.unify`. Two `.unify` marks for one module at two commits fail at load. `d`'s own sources see the same `a` as `b`. A second copy of `a` on one classpath stays closed.

The rule is one classpath, not one build file. A header plugin resolves to a [LockedPlugin](../engine/src/histos/resolve/DependencyLock.scala). Its jars load on [BuildLoader](../engine/src/histos/load/BuildLoader.scala). A module resolves to its own cell, and the assembly packs that cell. Those are different classpaths, so they may carry different commits of one library. `.unify` does not cross them.

Specular is the case. The plugin depends on heddle at `h1`. The core module depends on heddle at `h2`. The site task writes files. `h1` stays on the plugin loader. The assembly contains `h2`.

```scala
plugin(
  git"rocks.earlyeffect::specular".at(repo"https://github.com/early-effect/specular.git", commit"<hs>")
)

core.libraryDependencies(
  git"rocks.earlyeffect::heddle#core".at(repo"https://github.com/early-effect/heddle.git", commit"<h2>")
)
```

Specular's own request for heddle at `h1` joins the plugin cell. It does not join the core cell.

`dependsOn` merges classpaths. A docs module that depends on core and also on specular-as-a-library puts both commits on one classpath, and `.unify` applies. The win is the plugin loader, not a second module on the assembly.

A task runs on one of those classpaths. Stacking them, plugin loader as the parent of the module classpath, would bind `h1` for classes compiled against `h2`. That stack stays closed. The plugin sees the project through the api types and through files.

## Publish

A library or an application pom cannot list a git library. Central cannot fetch the commit. `publish release` and `publish snapshots` refuse the module at load when a compile dependency is a git library. That is the ship rule in [release-work-plan.md](release-work-plan.md), applied to this origin: a published library depends on coordinates a Maven consumer can resolve.

An unpublished module may take the edge. An assembly may pack the jar. `publishLocal`, compile, test, and run accept it.

`d` in the example above publishes an assembly. The jar contains `a`, `b`, and `c` as resolved, including `a` at the unified commit. The pom lists none of them. Those repositories never publish, and Central cannot fetch the commits. A library pom stays refused. A downstream Maven build that needs to name `a` itself depends on `a` the same way, or `a` grows its own publish.

The Sonatype wait that goes away is the one between two Histos builds. The producer does not publish. The consumer's lock names the commit. The cache holds the jar. The Sonatype wait that stays is a release of a library whose callers resolve it with Coursier: publish the upstream, change the dependency to that Maven revision, then publish the consumer.

## Settled

- Checkout and a git library. Checkout is first. A foreign commit joined as a module stays closed.
- The other tree is a Histos build, or an overlay that names its sources. `build.sbt` and Mill stay unread.
- The build file may name a branch or a commit. The lock records the commit. Update moves a branch and refuses to move a commit.
- The pin is a git line in `.histos/dependencies.lock`. `resolve --update` writes it with the cells.
- A published library or application cannot take the edge. An assembly may pack it. An unpublished module may depend on it. `a`, `b`, and `c` unpublished, with `d` publishing one jar, is that assembly.
- One classpath has one commit. The root names it with `.unify`. Anything else on that classpath fails. A plugin cell and a module cell are two classpaths and may differ. The upstream lock does not apply inside that resolve. The jar key includes the commits it was built against.

## Checklist

- [x] **Choices.** Closed above.

- [ ] **Repo-relative sources.** Owned by [scenario-work-plan.md](scenario-work-plan.md). Checked there. The checkout compile key waits while that box is open.

- [ ] **Checkout.** `upstream(name).at(checkout)` loads the named module from that directory when `histos.scala` is present. A missing module, a Scala mismatch, a platform mismatch, a cycle, and a library or application publication that takes the edge fail at load as typed errors. An unpublished module compiles against it. Proof: the api spec for the edge, and an engine spec that compiles the pair. No git.

- [ ] **Overlay.** A tree with no `histos.scala` compiles from the declared source directory, Scala version, and library dependencies. A tree with both the file and an overlay fails at load. A `build.sbt` in the tree is not read. Proof: the engine spec, still no git.

- [ ] **Git request.** A `git` coordinate with a repository and either a branch or a commit parses. The lock stores the commit, the tree hash, and the jar sha256. Load fails when the line is absent, the token differs, or a commit request disagrees with the stored commit. Proof: the lock spec. No network.

- [ ] **Fetch.** `resolve --update` runs `git` against a local repository whose tree has `histos.scala`, writes the line, and a later compile uses the stored jar. Update of a commit request refuses a different commit. Update of a branch request may replace the commit. A missing `git` on `PATH` fails that command. Proof: an engine spec with a local repository.

- [ ] **Integrity.** A stored tree whose hash differs from the line fails. A jar whose sha256 differs from the line fails. Proof: the same engine spec, with the bytes altered.

- [ ] **Library boundary.** A second commit whose API digest matches does not recompile the consumer. A commit that changes a public signature does. Proof: an engine spec on one local repository and two commits.

- [ ] **Transitive.** A git library whose module depends on another git library contributes that request to the consumer's resolve. A cycle fails at load. Its Maven dependencies are ordinary locked artifacts. The upstream lock is not read. Proof: two local repositories.

- [ ] **One commit.** Two commits of one module on one classpath fail and the error names each requester and commit. `.unify` on the root's request keeps that commit, records an eviction, and compiles the other requester against it. The stored jar misses when that commit changes. Proof: local `a` and `b`, consumer `d`, no network beyond the local repositories.

- [ ] **Plugin classpath.** A plugin cell and a module cell may lock two commits of one library. The assembly contains the module's commit. The plugin's commit stays on the plugin loader. A `dependsOn` that puts both on one module classpath still requires `.unify`. Proof: an engine spec with both cells, and an assembly listing that excludes the plugin commit.

- [ ] **Shared hit.** Two consumers that resolved the same commits restore one jar from one store. The second does not compile the upstream. Proof: `histos.ScenarioSpec`.

## Out of scope

Submodules. A sparse checkout. A tag as a request. A credential type. A test-only git edge. Replacing a `Dep` with a git edge by rewriting the coordinate in place. A parser for `build.sbt` or Mill.
