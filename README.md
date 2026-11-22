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

## License

Apache License 2.0. See [LICENSE](LICENSE).
