# Plugins are layers. Coordinates are modules or ships.

Pre-release. The contract is [plugins.md](plugins.md). A plugin is a layer. A coordinate is an in-repo module or a Ship pin.

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

Two coordinates, both already typed.

- In-repo: a module, the same value as `dependsOn`. `module("plugins")` is a normal module in a Ship. Its tests are normal tests. Other modules may `dependsOn` it. In-repo plugins are the path we encourage.
- Published: a Ship pin. `Ship("specular").released.dep(organization"dev.earlyeffect", artifact"specular")`, or the pointer or commit pin. Coursier resolves it. The pin is a `Dep`, not a plugin-only language.

The author puts the classes on the plugin classpath by naming one of those coordinates. Loose sources belong in a module.

## Dogfood

A build that produces a plugin uses that plugin by local construction. Other builds consume the published Ship through Coursier. The producing build does not resolve its own plugin from Coursier.

Histos is the dogfood. A `plugins` module is a member of `Ship("histos")`. The wiring compile sees that module's classes from this build, and writes `OurPlugins.layer`. Publish puts the same module on the Ship. A downstream build names `Ship("histos").released.dep(...)` and writes `OurPlugins.layer` against the resolved jar.

The proof is two graphs in one test. The producer wires the module coordinate and publishes to a local repository. The consumer's wiring classpath is that jar, selected by the Ship pin. The consumer never sees the producer's class directory. The producer never sees the jar. No test calls Coursier. The local repository stands in for it.

## Stringly pins

Builds and ships are values. A coordinate the build must accept is a `Dep`, a module, or a Ship pin. A literal `dep"org::name:1.2.3"` stays: `LiteralMacro.dep` checks it at compile time and produces a `Dep`. A family member is `.module` on that value. That is not a parse step.

A coordinate on the build is a `Dep`, a module, or a Ship pin. `Dep.from` returns a typed error for a CLI or a lock line. A validated `dep"..."` literal stays.

## Compile order

`histos.scala` can name a module and a Ship pin with types that already live in `histos-api`. It cannot name `OurPlugins` until that module's sources, or the Ship's jar, are on the compile.

1. Read the graph. Collect plugin coordinates: module names and Ship deps. This pass does not need plugin classes.
2. Compile in-repo plugin modules. Resolve Ship pins.
3. Compile the wiring against those class directories and jars. The wiring is a `ZLayer` the build writes: `OurPlugins.layer ++ SpecularPlugins.layer`, or `wire[Specular, HouseStyle]`.
4. The engine provides the requirements and builds `Plugins`.

The graph and the wiring cannot be one compilation. The graph is what decides the classpath of the wiring. `HistosBuild.graph` stays the graph. The wiring is a second definition, compiled second. Publishing and local stay layers on `HistosBuild`.

## Classpath and stamp

A directory or a jar of plugin classes is a legal dependency. `ClasspathBan` for `HistosBuild` stays. That class is the build, not a plugin.

The api digest (`META-INF/histos/version`, the sha256 of the host api jar) is written into the binary jar of a module the graph names as a plugin. The sources jar is not stamped. `BinaryOrSource` and `ExactBinary` stay.

## Specular

Do not edit `~/projects/early-effect/specular` in the histos change. Specular's plugin becomes a module in its own Ship. The histos build that uses it names that Ship pin and writes `SpecularPlugins.layer` (or `wire[Specular]`) in the wiring compile.

## Documentation

`docs/plugins.md` is the contract. The README, `docs/publishing.md`, and `Agents.md` follow it. They do not gain a ship number.

## Proof

The dogfood test above is the coordinate proof. Beside it: an in-repo module with two plugins and `derives Bundle`, wiring via `OurPlugins.layer`, and a second wiring via `wire` on one of them. A plugin that needs a `Site` service fails the wiring compile when `Site` is absent, and succeeds when the engine provides it. The module's own tests compile and `dependsOn` from another module compiles. No test calls Coursier.

`./histos test` on the modules this touches. Docs and the README follow `docs/plugins.md`. They do not gain a ship number.

