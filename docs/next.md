# What we still want

A person and an agent drive one build tool. The work is a typed effect. The loop is fast enough to disappear.

The terminal, the server, and the agent already share one request. Histos already refuses to install a JDK or Node. The gaps below are what the goal still needs.

Closed until we reopen them: a non-snapshot release, a Windows port, installing toolchains, matching sbt feature for feature, and a hermetic sandbox.

## The build file cannot state a lie

A mistake that the builder can see belongs in the type, so the build file does not compile. A setting for a platform the module does not build is that kind of mistake. So is a builder that takes a pile of loose values where one value would do. The graph stays data. The type carries the constraint.

## One engine

Compile, test, run, package, and the editor call the same engine paths. BSP asks the engine for a classpath and a platform. A plugin's work is a Step the scheduler already runs: typed, scoped, and cancelled with the run.

## One refusal

A person reads a line. An agent reads the value that line was rendered from. A bad name, a bad pattern, and a bad flag are that typed error on every surface.

## A fast loop

The second compile of an unchanged module, and a repeated passing test, do less than the first. A change that makes either one slower needs a reason. Preview learns to say which sources would be recompiled, and which tests a quick run would skip, without writing class files. Until it can, it says it does not know.

## CI follows the graph

A generated workflow names the tools and platforms the graph needs. The runner provides them. The workflow file is not a second list kept by hand.
