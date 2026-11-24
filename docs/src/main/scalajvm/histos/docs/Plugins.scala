package histos.docs

import specular.*

import zio.test.*

/** A plugin is a service the build turns on. A coordinate does not install it. */
object Plugins extends DocSpec:

  def doc = page("Plugins")(
    md"""
A plugin adds a capability: a command, a task, or a CI job. Putting a jar on the
classpath does not turn that capability on. Two files share the work.

`histos.plugins.scala` only names coordinates. It is read before any plugin class exists,
so it cannot import one. It says where to resolve the plugin from, and whether the lock
must match.

`histos.scala` turns a capability on by its Scala type. You provide a `ZLayer` for that
service. If the build never registers the service, the plugin's commands are not there,
even if the jar was downloaded.
""",
    cite[histos.plugin.HistosPlugins](_.sources),
    cite[histos.plugin.BuildServices].signature,
    section("What you publish for the next machine")(
      md"""
A plugin library is published so a later Histos can load it. The default publication is
rebuildable: it includes the sources and the inputs a later compiler needs, so a host on
a newer Scala can recompile the plugin. `PluginPublication.BinaryOnly` keeps the binding
metadata and skips that recipe. Use binary-only when you are shipping a closed binary and
you do not want a later host to rebuild it.
""",
      cite[histos.PluginPublication].signature,
    ),
    section("A task can write sources")(
      md"""
`TaskDep.Emit` adds the task's declared directories as conventional Main source
roots for a module. Use `TaskDep.EmitScoped` when only one output belongs to a
particular Main or Test stage and platform selection. Compile and test order the
producer before the stage consuming it. A producer cannot wait for that same
stage to compile: that would be a cycle.

Every output declares ownership: `DeclaredOutput.File` owns one file, never its
parent; `DeclaredOutput.Directory` owns that tree. The task writes to the staged
paths supplied in its run request, not to the public repository aliases. Histos
validates and commits the complete output set, then exposes it to readers. A
failed task does not publish a partial replacement. Clean removes only the declared
extent and refuses to remove a generation while readers hold it.

`EmitScoped` pairs a File with `SourceFile(ArtifactPath(...))` or
`ResourceFile(ResourceName(...))`. The source path is the entry inside the source
JAR; the source text still declares its package. The resource name is its exact
classpath location, independent of the generated repository path. Neither scans
siblings. Directories pair with `SourceTree` or `ResourceTree`, with entries relative
to the declared root.

`TaskDep.CompileScoped` requests exactly Main or Test. Test necessarily includes
Main. Legacy `TaskDep.Compile` supplies both classpaths. `TaskDep.Generate` only
orders tasks; it does not add sources.
""",
      cite[histos.plugin.TaskDep].signature,
      exampleValue {
        import histos.*
        import histos.plugin.*

        val output   = OutputName("fixture")
        val declared = DeclaredOutput.File(output, RepoPath("generated/Fixture.scala"))
        val content  = EmittedContent.SourceFile(ArtifactPath("fixtures/Fixture.scala"))
        val emission = TaskDep.EmitScoped(
          ProjectName("core"),
          SourceSet.Test,
          PlatformSelection.only(Platform.Jvm),
          content,
          output,
        )
        (declared, emission)
      }.assert(result => assertTrue(result._1.kind == histos.plugin.OutputKind.File)),
    ),
    section("A task can wait for another task")(
      md"""
`TaskDep.Generate` names another task of the same plugin. Histos runs that task
first, then this one. The name has to be a task the plugin declared. A missing
name is refused when the build loads.

This is how a task that writes files runs before a task that reads them. It does
not, by itself, add those files to a module's compile. A module still compiles
the sources under its directory and any extra directory named with `sources`.
A file the build copies into the class directory after compile is a
`GeneratedAsset`, which is a resource, not a source.
""",
      cite[histos.GeneratedAsset].signature,
    ),
    section("Effects and restored products are different")(
      md"""
Use `Seal.Open` for tasks with effects that must happen on each invocation, including
building or serving a docs site. Their effects rerun; a retained generation is not
permission to skip the invocation. Hermetic tasks with no declared needs can store
reusable results. Declared needs such as network or Docker keep work local.

Explicit cache restoration is a separate operation: Histos restores stored bytes
through the same staged ownership transaction as fresh production. It does not run
the producer's effects. Read-only consumers and previews lease committed physical
products. They do not run generators or infer ownership from a repository alias.
After a cold load or clean, execute the producer (or explicitly restore its result)
before asking a preview to read generated inputs.
""",
      cite[histos.plugin.Seal].signature,
      cite[histos.plugin.DeclaredOutput].signature,
      cite[histos.plugin.EmittedContent].signature,
    ),
    section("A task runs a program")(
      md"""
A plugin that starts a program uses `Program`. Histos reads both output streams while
the child is still running, and prints each line before the task finishes. A CI log
shows those lines. The exit code is returned only after both streams have closed, so a
line written just before the process exits is still in the log.

The process dies when the scope that started it closes. A task uses the invocation
scope, so interrupt stops the program. A preview server uses the generation scope, so
it stays up until that generation is retired.

`TaskDep.Compile` prints the same way. The cell's compile lines are invocation events,
and they show up while that cell is compiling.
"""
    ),
    section("Specular, as a worked example")(
      md"""
This documentation site is built by Specular, which is a Histos plugin. The build names
the docs module and the `DocsSite` main, then registers the service twice: once so the
plugin can contribute commands, and once so it can contribute tasks. One layer is shared,
so both registrations see the same site.

`linkClient` copies the linked browser script to `assets/client.js`. That script is what
reloads the preview tab when the site is rebuilt.

You build the site with `./histos run dev.earlyeffect.specular/site`. You serve it with
`./histos run dev.earlyeffect.specular/preview`, which leaves the server up. `preview` is
already a Histos command, so the plugin's command is named `serve` and it runs that same
task. `./histos serve` is the short form. `./histos ~ run dev.earlyeffect.specular/site`
rebuilds when the docs sources change, and the preview reloads.
""",
      cite[histos.plugin.Site](_.publish),
    ),
  )
end Plugins
