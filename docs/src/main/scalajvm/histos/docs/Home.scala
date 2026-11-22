package histos.docs

import mermoid.ascent.MermoidAscent
import specular.*

import zio.test.*

/** The front door. The manual starts at Install. */
object Home extends DocSpec:

  def doc = page("Histos")(
    md"""
Histos is a build tool for Scala. You describe the project in a Scala program,
`histos.scala`. Histos compiles that program into a graph of modules, then does the work:
compile, test, run, publish. The work is a ZIO effect, so a command can be interrupted and
a resource it opened gets closed.

You can ask from a terminal, from an editor, or from an agent. They share one graph and
one result. A compile runs when the code it depends on changed. A skip that hides a real
change is a failure, even when the command came back quickly.
""",
    section("Three clients, one graph")(
      md"""
You can ask Histos from three places, and they share one answer.

From a terminal, you type `histos`. That command runs on the JVM. When you have linked
`histos-client`, the commands that do the heavy work (`compile`, `test`, `run`, `clean`,
`fmt`, and `complete`) go through that native binary instead, and it talks to the same JVM
server.

From an editor, Metals imports the build. Metals speaks BSP, the build server protocol.
Histos answers as `histos bsp`.

From an agent, the client speaks MCP, the Model Context Protocol, on standard input.
`histos mcp` is that server. The tools are the same operations you type.

All three reach one JVM server, and that server works on one project graph. There are not
three copies of the build.

If you already build with sbt, Mill, or Bazel, [Why Histos](why-histos.html) is the
difference in purpose: a program that returns one graph, a skip that is a claim about
the bytes, and a cache you place and pin yourself.
""",
      illustration {
        MermoidAscent.diagram(Figures.clients)
      }.assert { ui =>
        val text = ui.toString
        assertTrue(
          text.contains("terminal"),
          text.contains("histos-client"),
          text.contains("histos bsp"),
          text.contains("histos mcp"),
        )
      },
      cite[histos.bsp.Bsp.type](_.BspVersion),
      cite[histos.mcp.HistosMcp.type](_.toolNames),
    ),
    section("A typed build")(
      md"""
The build file is `histos.scala`. It is an ordinary Scala program: `object build extends HistosBuild`,
and `graph` returns the project. A module, a Scala version, a dependency, and a path are
typed values, not strings you hope are well formed. If you write a version or a dependency
the compiler does not accept, `histos.scala` fails to compile, and Histos does not start
the build.
""",
      cite[histos.HistosBuild](_.graph).elided(12),
      cite[histos.ScalaVersion.type](_.from),
    ),
    section("A skip you can trust")(
      md"""
Compile and package are expensive, so Histos stores their outputs by the bytes that went in.
A later compile is skipped only when those bytes say nothing relevant changed.

Two checks matter. The API digest is the public signature of a module: the names and types
other modules compile against. A comment or a change inside a method body does not move
that signature, so a downstream module is not rebuilt. The product digest is the class
files themselves, with line-number tables and the TASTY attribute removed, so a formatting
change does not look like a new binary. A skip that hides a real change is a failure, even
when the command returned quickly.
""",
      cite[histos.zinc.ApiDigest.type](_.of),
      cite[histos.zinc.ProductDigest.type].elided(8),
    ),
    section("Install")(
      md"""
You install Histos once, with the script published on the release. It becomes
`~/.histos/bin/histos` and does not edit your shell startup files. You add that directory
to `PATH` yourself.

```sh
curl -fsSL https://github.com/early-effect/histos/releases/download/dist/shim | sh
export PATH="$$HOME/.histos/bin:$$PATH"
```

The Install page walks through what that script does, how a project pins a version, and
when the optional native client is worth linking.
"""
    ),
  )
end Home
