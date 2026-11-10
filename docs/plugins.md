# Plugins are layers

A plugin value is a service. The layer provides that service. Configuration is the environment. The build names the layer. The compiler checks it.

Helpers pin the plugin type first, then take the function. The lambda parameters are already the environment.

```scala
trait Specular extends HistosPlugin

object Specular:
  def layer: ZLayer[Site, Nothing, Specular] =
    pluginWith[Specular, Site](site => configured(site))
```

- `plugin[Specular](value)` is `ZLayer.succeed`.
- `pluginWith[Specular, Site](site => configured(site))` is pure construction.
- `pluginWithZIO[Specular, Site](site => ZIO.succeed(configured(site)))` is effectful construction. `Scope` is in the environment only when the effect asks for it.

`run` stays a method. Its environment is `Scope & PluginScopes`. A scoped layer the build opens lives for the generation. The invocation scope dies when `run` returns.

## A bundle, and a subset

```scala
object OurPlugins derives Bundle:
  val specular = Specular
  val house    = HouseStyle
```

`derives Bundle` checks that the owner is an object. `OurPlugins.layer` is the precise layer. A val with a `layer` member uses it. A val that is only the plugin becomes `ZLayer.succeed`. The layer provides each service and `Plugins`, a `Chunk[HistosPlugin]`, so the engine can run them without naming each service. The build may still `ZIO.service[Specular]`.

`wire[Specular]` and `wire[Specular, HouseStyle]` (up to four types) build the same kind of layer from the types. Derivation does not walk a jar. A plugin that the wiring does not name does nothing.

## Coordinates

Two coordinates, both typed.

- In-repo: `PluginSources.module(moduleDef)`, the same module value as `dependsOn`. Its tests are normal tests. Other modules may `dependsOn` it.
- Published: `PluginSources.ship(dep)`, a Ship pin. `.link(LinkStrategy)` selects `BinaryOrSource` or `ExactBinary`. The default is `BinaryOrSource`.

A path of loose sources is not a coordinate. Those sources are a module. `dep"..."` literals stay. The build DSL does not take a plugin coordinate as a `String`.

`histos.scala` names the coordinates. The load then compiles each in-repo plugin module, and the modules it depends on, and resolves each ship pin. `histos.wiring.scala` defines `object HistosWiring` with `def layer`, compiled against those classes. The load builds that layer and reads `Plugins`. A requirement that is still open fails that compile. The wiring closes a requirement by providing the service. `LinkStrategy.Api` is refused. The load fails when a plugin module does not compile, a pin does not resolve, the wiring file does not compile, or building the layer fails. No wiring file means no plugins.

## What is gone

- `plugin(dep"...")` and `plugin(path"...")`.
- `META-INF/histos/plugin`, in source, in the class directory, and in the jar.
- Loading an entry class by a binary name read from a file.
- Treating a directory or a jar that contains that file as Histos. A plugin module is a legal dependency.

The api digest name `META-INF/histos/version` stays the sha256 of the host api jar. Authors do not commit that file, and they do not commit the entry file.
