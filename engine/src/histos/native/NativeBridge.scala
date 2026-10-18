package histos.native

import java.nio.file.Path

/** The child that calls Scala Native. The engine does not load `scala.scalanative`. */
object NativeBridge:
  val MainName: String = "subprocess.NativeBridge"

  def source(module: String, main: String, clang: Path, clangpp: Path, classpath: List[Path]): String =
    val entries = classpath.map(path => s"""Path.of("${escape(path.toAbsolutePath.toString)}")""").mkString(", ")
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
       |            .withOptimize(false)
       |            .withMultithreading(false)
       |            .withCompileOptions(Seq("-Qunused-arguments"))
       |        )
       |      Build.buildCachedAwait(config)(using scope)
       |    }
       |    Files.writeString(Path.of("binary.path"), binary.toAbsolutePath.toString)
       |  }
       |}
       |""".stripMargin

  private def escape(raw: String): String =
    raw.flatMap {
      case '\\'  => "\\\\"
      case '"'   => "\\\""
      case '\n'  => "\\n"
      case char  => char.toString
    }
