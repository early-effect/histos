package histos.native

import java.nio.file.Path

/** The child that calls Scala Native. The engine does not load `scala.scalanative`. */
object NativeBridge:
  val MainName: String = "subprocess.NativeBridge"

  def source(
      module: String,
      main: String,
      clang: Path,
      clangpp: Path,
      classpath: List[Path],
      optimize: Boolean,
      libraries: Set[histos.NativeLibrary] = Set.empty,
      os: Option[String] = None,
      opensslHome: Option[Path] = None,
      resources: List[histos.ResourceName] = Nil,
  ): String =
    val (compile, link) = NativeFlags.of(libraries, os.getOrElse(""), opensslHome)
    val compileArgs     = (List("-Qunused-arguments") ++ compile).map(arg => s""""${escape(arg)}"""").mkString(", ")
    val linkArgs        = link.map(arg => s""""${escape(arg)}"""").mkString(", ")
    val entries  = classpath.map(path => s"""Path.of("${escape(path.toAbsolutePath.toString)}")""").mkString(", ")
    val included = resources
      .map(_.value)
      .distinct
      .sorted
      .map: name =>
        val literal = name.flatMap:
          case char @ ('*' | '?' | '[' | ']' | '{' | '}' | '\\') => s"\\$char"
          case char                                              => char.toString
        s""""${escape(literal)}""""
    val patterns = included.mkString(", ")
    s"""package subprocess
       |
       |import scala.scalanative.build.{Build, Config, Logger, NativeConfig}
       |import scala.scalanative.util.Scope
       |
       |import java.nio.file.{Files, Path}
       |
       |object NativeBridge {
       |  def main(args: Array[String]): Unit = {
       |    val base = Path.of("link")
       |    Files.createDirectories(base)
       |    val binary = Scope.apply { scope =>
       |      val config = Config.empty
       |        .withLogger(Logger(
       |          (_: Throwable) => (),
       |          (_: String) => (),
       |          (_: String) => (),
       |          (_: String) => (),
       |          (msg: String) => System.err.println(msg)
       |        ))
       |        .withBaseDir(base)
       |        .withModuleName("${escape(module)}")
       |        .withMainClass(Some("${escape(main)}"))
       |        .withClassPath(Seq($entries))
       |        .withCompilerConfig(
       |          NativeConfig.empty
       |            .withClang(Path.of("${escape(clang.toAbsolutePath.toString)}"))
       |            .withClangPP(Path.of("${escape(clangpp.toAbsolutePath.toString)}"))
       |            .withLinkStubs(true)
       |            .withOptimize($optimize)
       |            .withMultithreading(true)
       |            .withEmbedResources(true)
       |            .withResourceIncludePatterns(Seq($patterns))
       |            .withCompileOptions(Seq($compileArgs))
       |            .withLinkingOptions(Seq($linkArgs))
       |        )
       |      Build.buildCachedAwait(config)(using scope)
       |    }
       |    Files.writeString(Path.of("binary.path"), binary.toAbsolutePath.toString)
       |  }
       |}
       |""".stripMargin

  private def escape(raw: String): String =
    raw.flatMap {
      case '\\' => "\\\\"
      case '"'  => "\\\""
      case '\n' => "\\n"
      case char => char.toString
    }
end NativeBridge
