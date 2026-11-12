package histos.plugin

import histos.{Dep, DepKind, ProjectName}

import java.nio.file.Path

import zio.test.*

object PluginMessageLawSpec extends ZIOSpecDefault:
  private val binary     = Dep.trusted("dev.earlyeffect", "specular", "0.2.0", DepKind.Binary)
  private val java       = Dep.trusted("dev.earlyeffect", "specular", "0.2.0", DepKind.Java)
  private val full       = Dep.trusted("dev.earlyeffect", "specular", "0.2.0", DepKind.Full)
  private val moduleName = ProjectName("tools")
  private val plugin     = PluginId("test.echo")
  private val other      = PluginId("test.other")
  private val pluginApi  = ApiDigest.trusted("a" * 64)
  private val launcher   = ApiDigest.trusted("b" * 64)
  private val echo       = PluginTaskId(plugin, TaskName("echo"))
  private val site       = PluginTaskId(plugin, TaskName("site"))

  def spec = suite("plugin message laws")(
    test("a load error is that sentence, and the coordinate keeps its separator"):
      val rows = List(
        PluginLoadError.Unlocked(binary.render)    -> s"plugin ${binary.render} is not in the lock",
        PluginLoadError.Unversioned(binary.render) ->
          s"plugin ${binary.render} does not contain ${ApiDigest.resource}",
        PluginLoadError.VersionMismatch(binary, pluginApi, launcher) ->
          s"plugin ${binary.render} was built against api ${pluginApi.value} and this process loads ${launcher.value}",
        PluginLoadError.ApiLink(binary) ->
          s"plugin ${binary.render} uses LinkStrategy.Api, which is refused",
        PluginLoadError.ApiLink(java) ->
          s"plugin ${java.render} uses LinkStrategy.Api, which is refused",
        PluginLoadError.ApiLink(full) ->
          s"plugin ${full.render} uses LinkStrategy.Api, which is refused",
        PluginLoadError.NoSource(PluginCoordinate.Published(binary)) ->
          s"plugin ${binary.render} needs sources, and this loader does not have them",
        PluginLoadError.NoSource(PluginCoordinate.Module(moduleName)) ->
          s"plugin ${moduleName.value} needs sources, and this loader does not have them",
        PluginLoadError.ReservedPackage("java.lang.String") -> "'java.lang.String' is in a reserved package",
        PluginLoadError.EngineLeak(PluginCoordinate.Module(moduleName), "histos.Graph") ->
          s"plugin ${moduleName.value} links 'histos.Graph'",
        PluginLoadError.EngineLeak(PluginCoordinate.Published(binary), "histos.schedule.Sendable") ->
          s"plugin ${binary.render} links 'histos.schedule.Sendable'",
        PluginLoadError.BuildLeak("histos.Graph") ->
          "the build links 'histos.Graph', and no plugin class asked for it",
        PluginLoadError.IdMismatch(plugin, other) ->
          s"plugin ${plugin.value} loaded as ${other.value}",
        PluginLoadError.Defect(PluginCoordinate.Published(java), "java.lang.ExceptionInInitializerError") ->
          s"plugin ${java.render} failed while loading (java.lang.ExceptionInInitializerError)",
        PluginLoadError.Defect(PluginCoordinate.Module(moduleName), "java.lang.ExceptionInInitializerError") ->
          s"plugin ${moduleName.value} failed while loading (java.lang.ExceptionInInitializerError)",
        PluginLoadError.Io(PluginIo.Read(Path.of("read.bin")))       -> "plugin read failed at read.bin",
        PluginLoadError.Io(PluginIo.Resolve(Path.of("resolve.bin"))) -> "plugin resolve failed at resolve.bin",
        PluginLoadError.Io(PluginIo.Compile(Path.of("compile.bin"))) -> "plugin compile failed at compile.bin",
        PluginLoadError.Io(PluginIo.Open(Path.of("open.bin")))       -> "plugin open failed at open.bin",
      )
      assertTrue(
        rows.forall((error, sentence) => error.message == sentence),
        java.render == "dev.earlyeffect:specular:0.2.0",
        binary.render == "dev.earlyeffect::specular:0.2.0",
        full.render == "dev.earlyeffect:::specular:0.2.0",
      )
    ,
    test("a run error is that sentence, and a cycle joins its path from the task it closes on"):
      val detail = PluginDetail("runs the task")
      val rows   = List(
        PluginRunError.Failed(detail)                    -> "runs the task",
        PluginRunError.MissingOutput(OutputName("site")) -> "plugin output site was not written",
        PluginRunError.ExtraOutput("dist/extra.txt")     -> "plugin wrote 'dist/extra.txt', which was not declared",
        PluginRunError.MissingArg(ParamName("out"))      -> "plugin argument out is missing",
        PluginRunError.MissingNote(ProjectName("core"), NoteKey("role")) -> "module core has no note role",
        PluginRunError.Unknown(echo)                     -> s"plugin task ${echo.render} is not in the graph",
        PluginRunError.Cycle(::(echo, List(echo)))       -> "plugin task cycle test.echo/echo -> test.echo/echo",
        PluginRunError.Cycle(::(echo, List(site, echo))) ->
          "plugin task cycle test.echo/echo -> test.echo/site -> test.echo/echo",
        PluginRunError.Defect("java.lang.RuntimeException") -> "plugin task failed (java.lang.RuntimeException)",
        PluginRunError.EngineLeak("histos.Graph")           -> "plugin task links 'histos.Graph'",
        PluginRunError.Linkage("histos.Missing")            -> "plugin task is missing class histos.Missing",
      )
      assertTrue(rows.forall((error, sentence) => error.message == sentence))
    ,
    test("note text and a plugin detail hide the raw line, and a key or env name shows it"):
      assertTrue(
        NoteText.from("   ").map(_.value) == Right("   "),
        NoteText.from("\t") == Left(NoteTextError.Bad("\t")),
        NoteTextError.Empty.message == "note text is not empty",
        NoteTextError.Bad("a\nb").message == "note text is one line",
        PluginDetail.from("   ").map(_.value) == Right("   "),
        PluginDetail.from("\t") == Left(PluginDetailError.Bad("\t")),
        PluginDetailError.Empty.message == "a plugin detail is not empty",
        PluginDetailError.Bad("a\nb").message == "a plugin detail is one line",
        InputKeyError.Empty.message == "an input key is not empty",
        InputKeyError.Bad("a b").message == "'a b' is not an input key",
        EnvName.from("_").map(_.value) == Right("_"),
        EnvNameError.Empty.message == "an environment name is not empty",
        EnvNameError.Bad("1A").message == "'1A' is not an environment name",
      ),
  )

  extension (dependency: Dep) private def render: String = PluginCoordinate.Published(dependency).render
end PluginMessageLawSpec
