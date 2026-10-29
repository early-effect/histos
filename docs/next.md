# What we still want

A person and an agent drive one build tool. The work is a typed effect. The loop is fast enough to disappear.

The terminal, the server, and the agent share one request. Histos refuses to install a JDK or Node.

The build file rejects a setting for a platform the module does not build. Compile, test, run, package, and the editor call the same engine paths. A bad name, a bad pattern, and a bad flag are one typed error. Preview names the sources a compile would rebuild and the suites a quick run would skip, and it says when it does not know. A generated workflow names the tools the graph declared.

The release path is open. The decisions are in [release-work-plan.md](release-work-plan.md).

The cache is why this tool exists. The decisions and the checklist are in [cache-work-plan.md](cache-work-plan.md).

A module can compile against another checkout. A library can be built from a git commit instead of fetched from Central. The lock names the commit. A branch is an update input. The decisions are in [source-work-plan.md](source-work-plan.md).

A scenario is how a cache claim is checked, including Histos against itself: one store, several checkouts, and an assertion on the values. The same shape is the Histos side of a comparison with sbt and zipx. The decisions are in [scenario-work-plan.md](scenario-work-plan.md). The code is [Scenario](../engine/test/Scenario.scala). It is a test, not a command.

Closed until we reopen them: a Windows port, installing toolchains, matching sbt feature for feature, and a hermetic sandbox.
