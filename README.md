# Histos

A Scala build tool whose graph is data and whose build steps are ZIO effects. The CLI, BSP, and MCP use the same graph; their request and result contracts are not yet one application service.

Pre-release. M0-M3 are delivered. M4 implementation and local verification are complete; stock Windows bootstrap verification runs in PR CI.

- [Roadmap](docs/roadmap.md): delivered foundations, current acceptance gates, and remaining milestones.
- [Architecture](docs/architecture.md): current types, resource ownership, command behavior, and trust boundaries.
- [Plugins](docs/plugins.md): enablement, linking, execution, and cache contracts.
- [Audit](docs/audit.md): historical findings and current remaining work, each with an owner.

## Locked builds

[`Projects.tools`](api/src/histos/Graph.scala) selects a typed [`ToolCatalog`](api/src/histos/ToolCatalog.scala). Modules may override their Scala.js and Scala Native revisions with `jsVersion` and `nativeVersion`.

`histos resolve --update` explicitly solves module, plugin, and tool dependencies, then commits them together in `.histos/dependencies.lock`. Ordinary execution fetches recorded artifacts and checks their SHA-256 values. Missing, changed, or corrupt tool cells fail with typed errors rather than an implicit solve. Classifier identity is part of the lock.

JDK and Node locks contain a nonempty table of OS/CPU archive pins. Selection requires an exact host match. `.histos/toolchain.local` and `.histos/node.local` explicitly opt into local runtimes; their content identities are build inputs. Without a runtime lock, bootstrap trusts the installed runtime. This repository currently commits neither a JDK nor Node lock.

The remote launcher has an immutable timestamped revision and SHA-256 in `.histos/launcher.lock`. Only `histos launcher --update` advances it. Workflow generation consumes the existing pin without refreshing metadata. The verified M4 launcher is pinned to `0.0.1-20261024.180900-15`.

These locks do not make arbitrary plugin code, clang, browsers, or system packages hermetic. See the [architecture and migration rules](docs/architecture.md) and the [M4 verification checkpoint](docs/audit.md).

## License

Apache License 2.0. See [LICENSE](LICENSE).
