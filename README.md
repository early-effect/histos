# Histos

Histos is a Scala build tool. The build is a Scala program that returns a graph. The work is a ZIO effect. A person, an editor, and an agent ask for the same operations and read the same results.

The manual, the command reference, and the design are at [early-effect.github.io/histos](https://early-effect.github.io/histos/).

## Install

Histos runs on macOS and Linux. On Windows, use WSL. It needs JDK 25 or newer.

```sh
curl -fsSL https://github.com/early-effect/histos/releases/download/dist/shim | sh
export PATH="$HOME/.histos/bin:$PATH"
```

The script installs itself as `~/.histos/bin/histos` and does not edit a shell rc.

## A project

`histos new <name>` writes a project. The build file is one JVM module with a ZIO dependency and a main class. The Scala version and the ZIO version are the ones the scaffold writes.

```sh
cd <name>
./histos test
```

## Builds that use new APIs

The project's `histos.version` pin selects the toolchain that compiles `histos.scala`, not just the launcher. A build using scoped roots, scoped plugin emissions or `MetadataUri` needs a snapshot containing those APIs. Obtain the matching snapshot coordinate from the integrating build, then:

1. Run `histos toolchain install <snapshot-version> --no-client` with the installed launcher.
2. Set `histos.version=<snapshot-version>` in `build.properties`.
3. Run `./histos reboot`, then `./histos version` and `./histos graph`. Confirm the command and daemon use the matching toolchain and the final build loads.
4. Run the project's compile, full tests and docs/site gates with that pin before adopting it downstream.

If the snapshot has not been published yet, the integrating owner must supply a matching source-built toolchain first. An older installed API cannot compile new declarations, and an old daemon is not a valid substitute. Temporary source-launcher scripts used during development are evidence for that checkout, not a fresh-checkout installation recipe. Use the snapshot channel for the handoff, not a stable release to unblock compilation.

Publication URLs now use `MetadataUri("http://...")` or `MetadataUri("https://...")`. Runtime input uses `MetadataUri.from`, returning `Either[MetadataUriError, MetadataUri]`. This applies to license, SCM, developer/organization homepages and organization/ship URLs. `url"..."` still constructs the separate HTTPS `HttpUrl` network capability.

## License

Apache License 2.0. See [LICENSE](LICENSE).
