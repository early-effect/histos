# Histos

A Scala build tool whose graph is data and whose build steps are ZIO effects. The CLI, BSP, and MCP use the same graph; their request and result contracts are not yet one application service.

Pre-release. Histos does not install a JDK or Node. If JDK 21 or Node 25 is not already there, it explains what it found and fails. The roadmap and audit describe delivered work and remaining capabilities.

- [Roadmap](docs/roadmap.md): delivered foundations, current acceptance gates, and remaining milestones.
- [Architecture](docs/architecture.md): current types, resource ownership, command behavior, and trust boundaries.
- [Plugins](docs/plugins.md): enablement, linking, execution, and cache contracts.
- [Audit](docs/audit.md): historical findings and current remaining work, each with an owner.

## Locked builds

[`Projects.tools`](api/src/histos/Graph.scala) selects a typed [`ToolCatalog`](api/src/histos/ToolCatalog.scala). Modules may override their Scala.js and Scala Native revisions with `jsVersion` and `nativeVersion`.

`histos resolve --update` explicitly solves module, plugin, and tool dependencies, then commits them together in `.histos/dependencies.lock`. Ordinary execution fetches recorded artifacts and checks their SHA-256 values. Missing, changed, or corrupt tool cells fail with typed errors rather than an implicit solve. Classifier identity is part of the lock.

The running JVM is the JDK. A Js module also needs `node` on `PATH`. Both are floors: JDK 21 or newer, and Node 25 or newer. A newer install satisfies the floor. `.histos/toolchain.local` and `.histos/node.local` name a home the user already chose. A present file that is missing, unreadable, or below the floor is rejected. Histos does not create that home and does not fall through to another install.

The remote launcher has an immutable timestamped revision and SHA-256 in `.histos/launcher.lock`. Only `histos launcher --update` advances it. Workflow generation consumes the existing pin without refreshing metadata. See [the committed launcher pin](.histos/launcher.lock).

These locks do not make arbitrary plugin code, clang, browsers, or system packages hermetic. See the [architecture and migration rules](docs/architecture.md) and the [audit ledger](docs/audit.md).

## License

Apache License 2.0. See [LICENSE](LICENSE).
