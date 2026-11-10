# Plugins are layers. Coordinates are modules or ships.

Pre-release. Replace the entry-file loader. Do not keep `plugin(dep"...")`, `plugin(path"...")`, `META-INF/histos/plugin`, or a classloader that hunts for one object. The in-progress entry-file work is the wrong design. Do not finish it.

Histos stays extensible by ordinary Scala. An in-repo plugin is a module. A published plugin is a Ship pin, resolved through Coursier like any library. Several plugins may live in one module. The build names a layer. The compiler checks it.

## What a plugin is

A plugin value is a service. The layer provides that service. Configuration is the environment, not a function argument the loader invents.

```scala
object Specular extends HistosPlugin:
  def layer: ZLayer[Site, Nothing, Specular] =
    pluginWith[Specular]((site: Site) => configured(site))
```

Construction helpers pin the plugin type first, then take the function, the same way `ZLayer.fromFunction` does. The lambda's result is already `Specular`. The parameters are the environment.

- `plugin[Specular](Specular)` is `ZLayer.succeed`.
- `pluginWith[Specular]((site: Site) => configured(site))` is pure construction from services.
- `pluginWithZIO[Specular]((site: Site) => ZIO.succeed(configured(site)))` is effectful construction. `Scope` is in the environment only when the effect acquires something.

`run` stays a method on the value. Its environment is still `Scope & PluginScopes`. A scoped layer, when the build opens one, lives for the generation. The invocation scope still dies when `run` returns.

The error channel of a layer stays that layer's `E`. Composition unions the errors and the requirements.

## A bundle, and a subset

Scala 3 can derive a layer from vals or types the author wrote. It cannot see every object in a jar. Discovery does not walk a classpath.

The preferred registration is a bundle in the plugin module. Adding a plugin is adding a val.

```scala
object OurPlugins derives Bundle:
  val specular = Specular
  val house    = HouseStyle
```

`derives Bundle` builds `OurPlugins.layer`. Each val that has a `layer` member uses it. A val that is only the object becomes `ZLayer.succeed` of that object. The layer provides `Specular & HouseStyle` and a `Plugins` value, a `Chunk[HistosPlugin]`, so the engine can run every plugin without naming the services. The build may still `ZIO.service[Specular]`.

A call site that wants a subset writes `wire[Specular]`. Same derivation, from the types instead of the vals. Both stay. The bundle is the default. `wire` is the selection.

## Coordinates

Two coordinates, both already typed. No stringly `plugin(dep"...")`.

- In-repo: a module, the same value as `dependsOn`. `module("plugins")` is a normal module in a Ship. Its tests are normal tests. Other modules may `dependsOn` it. In-repo plugins are the path we encourage.
- Published: a Ship pin. `Ship("specular").released.dep(organization"dev.earlyeffect", artifact"specular")`, or the pointer or commit pin. Coursier resolves it. The pin is a `Dep`, not a plugin-only language.

The author is responsible for putting the classes on the plugin classpath by naming one of those coordinates. A path of loose sources is not a third coordinate. Those sources are a module.

## Dogfood

A build that produces a plugin uses that plugin by local construction. Other builds consume the published Ship through Coursier. The producing build does not resolve its own plugin from Coursier.

Histos is the dogfood. A `plugins` module is a member of `Ship("histos")`. The wiring compile sees that module's classes from this build, and writes `OurPlugins.layer`. Publish puts the same module on the Ship. A downstream build names `Ship("histos").released.dep(...)` and writes `OurPlugins.layer` against the resolved jar.

The proof is two graphs in one test. The producer wires the module coordinate and publishes to a local repository. The consumer's wiring classpath is that jar, selected by the Ship pin. The consumer never sees the producer's class directory. The producer never sees the jar. No test calls Coursier. The local repository stands in for it.

## Stringly pins

Builds and ships are values. A coordinate the build must accept is a `Dep`, a module, or a Ship pin. A literal `dep"org::name:1.2.3"` stays: `LiteralMacro.dep` checks it at compile time and produces a `Dep`. That is not a parse step.

What goes:

- `HeaderScan`. It slices `histos.scala` as text, extracts `plugin(dep"...")` and `plugin(path"...")`, and blanks those lines before compile. That is the brittle parse. Delete it, and delete `PluginLoadError.BadCoordinate` and `Misplaced` with it.
- Any loader path that re-parses a coordinate string the build already held as a `Dep`.

A runtime parser may exist at an edge: `Dep.from` already returns a typed error. A `parseShip` effect of that shape is allowed for a CLI or a lock line. The build DSL never calls it, and no API insists on a `String`.

While implementing, grep the loader and the build DSL for coordinate strings. A hit that is a validated literal stays. A hit that is `split` / `indexOf` / `startsWith("plugin(")` goes. Do not widen this into a rewrite of every `dep"..."` in `histos.scala`.

## Compile order

`histos.scala` can name a module and a Ship pin with types that already live in `histos-api`. It cannot name `OurPlugins` until that module's sources, or the Ship's jar, are on the compile.

1. Read the graph. Collect plugin coordinates: module names and Ship deps. This pass does not need plugin classes.
2. Compile in-repo plugin modules. Resolve Ship pins.
3. Compile the wiring against those class directories and jars. The wiring is a `ZLayer` the build writes: `OurPlugins.layer ++ SpecularPlugins.layer`, or `wire[Specular, HouseStyle]`.
4. The engine provides the requirements and builds `Plugins`.

The graph and the wiring cannot be one compilation. The graph is what decides the classpath of the wiring. `HistosBuild.graph` stays the graph. The wiring is a second definition, compiled second. Publishing and local stay layers on `HistosBuild`.

## What dies

- The header scan of `plugin(dep` and `plugin(path`.
- `META-INF/histos/plugin`, in source, in the class directory, and in the jar.
- The path-plugin compiler and its content-addressed `plugins/<digest>` output.
- Loading an entry class by a binary name read from a file.
- The ban that treats a directory or a jar containing that file as Histos. A plugin module is a legal dependency.

The api digest (`META-INF/histos/version`, the sha256 of the host api jar) stays on published jars. `BinaryOrSource` and `ExactBinary` stay. This design does not reopen #148.

`ClasspathBan` for `HistosBuild` stays. That class is the build, not a plugin.

## Specular

Do not edit `~/projects/early-effect/specular` in the histos change. Specular's plugin becomes a module in its own Ship. The histos build that uses it names that Ship pin and writes `SpecularPlugins.layer` (or `wire[Specular]`) in the wiring compile. The committed `META-INF/histos/plugin` under `specular-histos/src/main/resources` is deleted in a later change that names that repo. The classloader assertion in `SpecularPluginSpec` is deleted with it.

## Documentation

The design lives in the repo, not only in the session plan.

- Add `docs/plugins.md` as the contract. It states that `engine/src/histos/load` (`plugin(dep)`, `plugin(path)`, `META-INF/histos/plugin`) is the code this replaces.
- Replace the README Plugins section with that contract. Keep `run`, `PluginScopes`, and resource copying, which do not change.
- Replace the entry-line paragraph in `docs/publishing.md`. The api digest stays. The entry line does not.
- Correct the "marker is inside the jar" line in `docs/release-work-plan.md`.
- Point `Agents.md` at `docs/plugins.md`. Forbid the entry file, the header scan, class-file discovery, and a `String` coordinate on the build DSL.
- Rewrite the workspace memory `histos-plugin-api` to this contract. Drop the bytecode, TASTy, and "modules must not dependsOn a plugin" notes. Note the digest stay in `histos-resources` without treating the entry file as the design.

## Proof

The dogfood test above is the coordinate proof. Beside it: an in-repo module with two plugins and `derives Bundle`, wiring via `OurPlugins.layer`, and a second wiring via `wire` on one of them. A plugin that needs a `Site` service fails the wiring compile when `Site` is absent, and succeeds when the engine provides it. The module's own tests compile and `dependsOn` from another module compiles. No test calls Coursier.

`./histos test` on the modules this touches. Docs and the README lose the entry file and the header. They do not gain a ship number.

