package histos.docs

import specular.*

/** Hermetic lock versus a solved build. */
object ResolutionPage extends DocSpec:

  def doc = page("Resolution")(
    md"""
A build has to get its jars from somewhere. Histos has two ways, and they are a choice
about what is allowed to change. The default is solved. `.locked` is hermetic.

Compile, test, scaladoc, `histos deps`, and a plugin task all use the same choice. A
solved build does not consult `dependencies.lock`. A hermetic build does not solve.
""",
    cite[histos.Resolution].signature,
    section("Solved")(
      md"""
Solved is the omission. You write the dependencies in `histos.scala`. Histos resolves
them and remembers the result in `.histos/resolution` until the build file changes. A
jar that is not remembered yet is fetched. That includes a test classpath and a tool
the test runner needs. You do not write those into a lock first.

This is the mode to use while a build is moving. The program is the description. A
published plugin can still be an exact revision in `histos.plugins.scala`. That
revision is fetched. It does not freeze every other jar.

CI caches a solved build on `histos.scala`, because that file is what the solve
depends on. It does not run `histos resolve --check`. That command compares a
fresh solve with `dependencies.lock`, and a solved build has no lock.
"""
    ),
    section("Hermetic")(
      md"""
`.locked` makes the build hermetic. `dependencies.lock` is the complete set of jars
and tools. Compile, test, scaladoc, deps, and a plugin task read that file and do not
resolve. A cell or a tool that is not in the file is a failure, not a download.

`histos resolve --update` solves and writes the file. `histos resolve --check` fails
when a fresh solve would write a different one. You commit the file. CI caches a
hermetic build on it, because the jars cannot move unless the file moves.

Choose this when two machines, or a laptop and CI, must get the same bytes. A release
is the usual reason. A plugin jar you do not want re-resolved tomorrow is another.
The cost is that every jar the runner asks for has to be in the file, including a
test tool. If it is missing, the command stops.
""",
      cite[histos.Organized](_.locked),
    ),
    section("The plugin catalog agrees")(
      md"""
`histos.plugins.scala` has the same switch, and it defaults to solved. If the catalog
selects a lock and the build says nothing, the build inherits the lock. If the build
explicitly selects the other mode, loading fails. The two files have to describe one
choice.
""",
      cite[histos.plugin.HistosPlugins](_.resolution),
    ),
  )
end ResolutionPage
