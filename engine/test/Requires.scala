package histos

import histos.native.ClangProbe

import java.io.File
import java.nio.file.{Files, Path}

import _root_.scala.jdk.CollectionConverters.*

import zio.{System, Trace, UIO, ZIO}
import zio.test.{Live, Spec, TestAspectPoly}

enum Tool:
  case AnyClang
  case Clang(major: ClangMajor)
  case Node
  case Npm
  case Firefox
  case IvyLocalNative(revision: NativeVersion)

/** CI installs every tool, so a gated spec still runs there. */
object Requires:
  def apply(tool: Tool, more: Tool*): TestAspectPoly =
    new TestAspectPoly:
      def some[R, E](spec: Spec[R, E])(using trace: Trace): Spec[R, E] =
        spec.whenZIO(Live.live(ZIO.forall(tool :: more.toList)(present)))

  /** Must match the chekhov-jsenv the engine links Js tests with. */
  private val chekhovPlaywright = "1.62.1"

  private def present(tool: Tool): UIO[Boolean] =
    for
      env  <- System.envs.orElseSucceed(Map.empty[String, String])
      home <- System.property("user.home").orElseSucceed(None)
      os   <- System.property("os.name").orElseSucceed(None)
      found <- tool match
        case Tool.AnyClang       => ClangProbe.found(env).isSuccess
        case Tool.Clang(major)   => ClangProbe.resolve(major, env).isSuccess
        case Tool.Node           => onPath(env, "node")
        case Tool.Npm            => onPath(env, "npm")
        case Tool.Firefox        => firefox(env, home, os)
        case Tool.IvyLocalNative(revision) =>
          home match
            case None => ZIO.succeed(false)
            case Some(dir) =>
              exists(Path.of(dir, ".ivy2", "local", "org.scala-native", "nativelib_native0.5_3", revision.value))
    yield found

  private def onPath(env: Map[String, String], name: String): UIO[Boolean] =
    val dirs = env.get("PATH").toList.flatMap(_.split(File.pathSeparatorChar).toList).filter(_.nonEmpty)
    ZIO.attemptBlocking(dirs.exists(dir => Files.isExecutable(Path.of(dir, name)))).orElseSucceed(false)

  /** Chekhov's own lookup for the Playwright driver and its Firefox build. */
  private def firefox(env: Map[String, String], home: Option[String], os: Option[String]): UIO[Boolean] =
    home match
      case None => ZIO.succeed(false)
      case Some(dir) =>
        ZIO
          .attemptBlocking:
            val cache  = osCache(env, Path.of(dir), os)
            val driver = setting(env, "PLAYWRIGHT_DRIVER_CLI").map(Path.of(_)).getOrElse:
              setting(env, "CHEKHOV_CACHE").map(Path.of(_)).getOrElse(cache.resolve("chekhov"))
                .resolve("playwright").resolve(chekhovPlaywright)
                .resolve("node_modules").resolve("playwright").resolve("cli.js")
            val browsers = setting(env, "PLAYWRIGHT_BROWSERS_PATH").filter(_ != "0").map(Path.of(_))
              .getOrElse(cache.resolve("ms-playwright"))
            (driver, browsers)
          .flatMap: (driver, browsers) =>
            exists(driver).zipWith(hasFirefox(browsers))(_ && _)
          .orElseSucceed(false)

  private def osCache(env: Map[String, String], home: Path, os: Option[String]): Path =
    os.map(_.toLowerCase) match
      case Some(name) if name.contains("mac") => home.resolve("Library").resolve("Caches")
      case Some(name) if name.contains("win") =>
        setting(env, "LOCALAPPDATA").map(Path.of(_)).getOrElse(home.resolve("AppData").resolve("Local"))
      case _ => setting(env, "XDG_CACHE_HOME").map(Path.of(_)).getOrElse(home.resolve(".cache"))

  private def hasFirefox(browsers: Path): UIO[Boolean] =
    ZIO
      .scoped:
        ZIO
          .fromAutoCloseable(ZIO.attemptBlocking(Files.list(browsers)))
          .map(_.iterator.asScala.exists(path => Option(path.getFileName).exists(_.toString.startsWith("firefox-"))))
      .orElseSucceed(false)

  private def exists(path: => Path): UIO[Boolean] =
    ZIO.attemptBlocking(Files.exists(path)).orElseSucceed(false)

  private def setting(env: Map[String, String], name: String): Option[String] =
    env.get(name).filter(_.nonEmpty)
