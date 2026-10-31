# Publishing

`publish` is an effect. It requires a `Publishing`. A command closes that requirement with `provideLayer`. Anything larger is `flatMap` of those effects. The engine does not look up a publisher by name.

This file is the decision. [release-work-plan.md](release-work-plan.md) is the history of how the commands got here. The catalog is `manual("histos", manual"0.0.1")`. Editing this file does not upload it.

## The effect

```scala
val publish: ZIO[Publishing, PublishFault, PublishReceipt] =
  ZIO.serviceWithZIO[Publishing](_.publish(request))

val publishLocal =
  publish.provideLayer(build.local)

val publishSona =
  publish.provideLayer(build.sona)

val publishAll =
  publishLocal.flatMap(_ => publishSona)
```

`publishAll` is composition, not the only legal program. A build that does not want local, or does not want Sonatype, does not write that `flatMap`. The same effect can go back into a layer:

```scala
val sona: ULayer[Publishing] =
  ZLayer.succeed(Publishing.fromEffect(publishSona))
```

That is `ZLayer.succeed`. It is not a scoped layer that opens two stocks and chooses between them.

```scala
trait Publishing:
  def publish(request: PublishRequest): IO[PublishFault, PublishReceipt]
  def secretNames: List[String]
  def followUp: FollowUp
```

`Publishing` lives in `api`. `api` depends only on ZIO. `fromEffect` builds one from a function. `PublishRequest` carries the staged modules and the ship version. `secretNames` and `followUp` are pure. The release workflow builds the Sonatype layer and reads them. It does not upload. A `flatMap` does not merge secrets.

`PublishFault` is the typed failure. A narrower publisher may use `PublishFault.Version` when it is handed a ship shape it refuses. The stock Sonatype publisher does not, because that stock handles both shapes.

## Sonatype looks at the ship

The stock Sonatype publisher is one effect. It reads the ship version on the request and runs the protocol that version applies to.

- A snapshot (`Revision.Pointer`, the `<ship>-SNAPSHOT` form, or `Revision.Snapshot`, the unique coordinate) is the snapshot PUT.
- A release (`Revision.Release`) is sign, then one portal bundle. The release branch signs. The snapshot branch does not.
- A `-ci` coordinate (`Revision.Ci`) is not a Sonatype publish.

sbt and zipx already publish this way: one action, the version decides. The trait does not force two layers.

```scala
Sona.layer(ReleaseMode.Automatic)
Sona.layer(ReleaseMode.Hold)
```

The snapshot branch ignores the mode. The release branch uses it. `--automatic` is on the request, so a wrapper and the stock both see it, and it forces Automatic for that run. There is no `--hold` flag. `followUp` is `Deployment` only for Hold, and that is the promote workflow. Automatic is `FollowUp.Finished`. Secrets are the five names, because the release branch signs: `SONATYPE_USERNAME`, `SONATYPE_PASSWORD`, `PGP_SECRET`, `PGP_PASSPHRASE`, `PGP_KEY_HEX`.

`publish snapshots` and `publish release` both provide `build.sona`. The ship version is what differs. This repository uses the default, so `0.0.1` portals and a snapshot coordinate PUTs, through the same layer.

A narrower publisher is still a legal `Publishing`. A build that will never snapshot writes an effect that only portals, and that effect may fail `Version` if handed a snapshot. That fault belongs to that effect.

Signing stays inside the Sonatype effect. The stage that both branches share packs unsigned files. `histos.central.Publisher` remains the Maven-file writer for the snapshot PUT. It is not this trait.

## Local is Coursier

The local publisher's interface is Coursier, the library resolve already calls. It is not a directory Histos chooses.

[Coursier's repository docs](https://get-coursier.io/docs/other-repositories) set the defaults for the CLI and the high-level API: ivy2 local, then Maven Central. The same page says not to use `~/.m2/repository`. That directory is Maven's own cache, and Coursier does not treat it as a repository other tools should read. The short name `ivy2Local` is the local repository Mill, Scala CLI, and sbt publish into. The short name `m2Local` exists and is marked not recommended.

The high-level API is `coursier.Resolve` and `coursier.Fetch` ([API](https://get-coursier.io/docs/api)). Those types resolve and fetch. They do not publish. A local install is done when a later `Resolve`, with Coursier's default repositories, finds the coordinate. The test asks Coursier. It does not open `~/.m2` or `~/.ivy2`.

`Publishing` does not take a `Path`. `publishLocal` does not take a directory. A test checks the install by resolving the coordinate through Coursier. The local effect does not sign. It accepts a snapshot, a release, and a `-ci` coordinate.

## What a build overrides

The build file is compiled against `histos-api` only. The Coursier call and the portal call live in the engine. The default layer is that stock service, so the build can wrap it without naming an engine class.

```scala
trait HistosBuild:
  def graph: Either[BuildError, ProjectGraph]
  def local: ZLayer[LocalPublish, Nothing, Publishing] = Local.layer
  def sona: ZLayer[SonaPublish, Nothing, Publishing] = Sona.layer(ReleaseMode.Automatic)
```

`Local.layer` is `ZLayer.service[LocalPublish]`. `Sona.layer(mode)` is the stock `SonaPublish`, with the release branch closed over `mode`. The engine builds the layer with those stocks in the environment. An override that needs nothing ignores the stock. An override that requires `SonaPublish` wraps it. A wrapper is that function, or `publishSona.flatMap(announce)`. A third destination is another `ULayer[Publishing]`, written in the build file or coming from a plugin. There is no registry and no publisher id.

The modules a command names are the modules that effect publishes. There is no per-module publisher list. `engine` and `central` are not in the ship, so they are not coordinates.

## What this replaces

The data trait `histos.Publisher`, `PublisherId`, `.publish`, and `.publishTo` are the old model. A destination was a value the engine matched by id. That match is gone. `publishLocal`, `publish snapshots`, and `publish release` remain the commands. Each one is `publish.provideLayer(...)`.

Promote, drop, and the asset upload stay commands. They are not methods on `Publishing`. GitHub Packages and CodeArtifact are not implementations in this repository. A later one is another layer.
