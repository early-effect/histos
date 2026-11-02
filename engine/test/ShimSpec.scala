package histos.publish

import histos.jdk.Sha256
import histos.process.{ChildEvent, ChildProcess, ProcessExit}
import histos.workspace.Workspace

import java.io.IOException
import java.net.InetSocketAddress
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.util.concurrent.CopyOnWriteArrayList

import com.sun.net.httpserver.{HttpExchange, HttpServer}

import zio.{Chunk, Scope, System, ZIO}
import zio.test.{Spec, TestEnvironment, ZIOSpecDefault, assertTrue}

object ShimSpec extends ZIOSpecDefault:
  private val Version  = "0.0.2-20260101.120000-1"
  private val Pointer  = "0.0.2-SNAPSHOT"
  private val JarBytes = "histos-jar".getBytes(StandardCharsets.UTF_8)

  def spec: Spec[TestEnvironment & Scope, Any] = suite("the user shim")(
    test("the action ships the same script as the installer"):
      ZIO
        .attempt {
          val root      = shimSource.getParent.getParent
          val installed = Files.readString(shimSource)
          val action    = Files.readString(root.resolve(".github/actions/setup/histos"))
          (installed, action)
        }
        .map { (installed, action) =>
          assertTrue(
            installed == action,
            installed.contains("On Windows, use WSL"),
            installed.contains("--compile-client"),
            installed.contains("--make-default"),
          )
        }
    ,
    test("self update replaces the shim when the published hash differs"):
      scene(remoteSuffix = "\n# updated\n").flatMap { case (home, repo, _, _) =>
        runShim(home, repo, "self", "update").flatMap { exit =>
          ZIO.attempt(Files.readString(home.resolve("bin/histos"))).map { text =>
            assertTrue(exit.value == 0, text.contains("# updated"))
          }
        }
      }
    ,
    test("a toolchain download keeps the jar that matches the manifest"):
      scene().flatMap { case (home, repo, hits, log) =>
        runShim(home, repo, "toolchain", "install", Version).flatMap { exit =>
          val jar = home.resolve(s"toolchains/$Version/histos.jar")
          text(jar).map { bytes =>
            assertTrue(
              exit.value == 0,
              bytes == new String(JarBytes, StandardCharsets.UTF_8),
              hits.contains(DistLayout.manifest(Version)),
              hits.contains(DistLayout.jar(Version)),
              !logLines(log).exists(_.contains("-jar")),
            )
          }
        }
      }
    ,
    test("a jar checksum mismatch is a failure and does not exec"):
      scene(jarSha = Sha256.of(Chunk.fromArray("other".getBytes)).value).flatMap { case (home, repo, _, log) =>
        runShim(home, repo, "toolchain", "install", Version).flatMap { exit =>
          ZIO.succeed(
            assertTrue(exit.value != 0, logLines(log).isEmpty)
          )
        }
      }
    ,
    test("a local assembly is the jar and no toolchain is fetched"):
      scene().flatMap { case (home, repo, hits, log) =>
        val jar = repo.resolve("dist/histos.jar")
        write(repo.resolve("build.properties"), s"histos.version=$Version\n") *>
          write(jar, new String(JarBytes, StandardCharsets.UTF_8)) *>
          write(repo.resolve(".histos/assembly.sha256"), Sha256.of(Chunk.fromArray(JarBytes)).value + "\n") *>
          runShim(home, repo, "test").flatMap { exit =>
            ZIO.succeed(
              assertTrue(
                exit.value == 0,
                hits.isEmpty,
                logLines(log).exists(_.contains("-jar")),
                logLines(log).exists(_.endsWith("dist/histos.jar")),
              )
            )
          }
      }
    ,
    test("java older than 21 fails with the floor"):
      scene(javaVersion = "17.0.2").flatMap { case (home, repo, _, log) =>
        val jar = repo.resolve("dist/histos.jar")
        write(repo.resolve("build.properties"), s"histos.version=$Version\n") *>
          write(jar, new String(JarBytes, StandardCharsets.UTF_8)) *>
          write(repo.resolve(".histos/assembly.sha256"), Sha256.of(Chunk.fromArray(JarBytes)).value + "\n") *>
          runShimFull(home, repo, "test").flatMap { (exit, err) =>
            ZIO.succeed(assertTrue(exit.value != 0, err.contains("older than 21"), err.contains("does not install it")))
          }
      }
    ,
    test("update inside a project leaves the user default alone"):
      scene().flatMap { case (home, repo, _, _) =>
        write(home.resolve("default"), s"$Pointer\n") *>
          write(repo.resolve("build.properties"), s"histos.version=$Pointer\n") *>
          runShim(home, repo, "update").flatMap { exit =>
            text(repo.resolve("build.properties")).zip(text(home.resolve("default"))).map { (pin, user) =>
              assertTrue(exit.value == 0, pin.contains(Version), user.contains(Pointer), !user.contains(Version))
            }
          }
      }
    ,
    test("update --make-default writes the same resolved version to both files"):
      scene().flatMap { case (home, repo, _, _) =>
        write(home.resolve("default"), s"$Pointer\n") *>
          write(repo.resolve("build.properties"), s"histos.version=$Pointer\n") *>
          runShim(home, repo, "update", "--make-default").flatMap { exit =>
            text(repo.resolve("build.properties")).zip(text(home.resolve("default"))).map { (pin, user) =>
              assertTrue(
                exit.value == 0,
                pin.contains(s"histos.version=$Version"),
                user.trim == Version,
              )
            }
          }
      }
    ,
    test("a missing client runs java -jar and says compile-client can link one"):
      scene(clients = Nil).flatMap { case (home, repo, _, _) =>
        write(repo.resolve("build.properties"), s"histos.version=$Version\n") *>
          runShimFull(home, repo, "test").flatMap { (exit, err) =>
            ZIO.succeed(
              assertTrue(exit.value == 0, err.contains("no prebuilt client"), err.contains("--compile-client"))
            )
          }
      }
    ,
    test("compile-client links a binary and records that it was compiled here"):
      scene(clients = Nil, sources = true).flatMap { case (home, repo, _, log) =>
        runShim(home, repo, "toolchain", "install", Version, "--compile-client").flatMap { exit =>
          val dir = home.resolve(s"toolchains/$Version")
          text(dir.resolve("client.origin")).map { origin =>
            assertTrue(
              exit.value == 0,
              origin.trim == "local",
              Files.isExecutable(dir.resolve("histos-client")),
              logLines(log).exists(_.contains("client")),
            )
          }
        }
      },
  ) @@ zio.test.TestAspect.withLiveSystem

  private def scene(
      jarSha: String = Sha256.of(Chunk.fromArray(JarBytes)).value,
      clients: List[(String, String)] = List(
        "linux-x86_64"  -> "aa",
        "macos-aarch64" -> "bb",
      ),
      sources: Boolean = false,
      javaVersion: String = "21.0.2",
      remoteSuffix: String = "",
  ): ZIO[Scope, Any, (Path, Path, CopyOnWriteArrayList[String], Path)] =
    Workspace.work("histos-shim").mapError(error => new IOException(error.message)).flatMap { root =>
      val packed =
        if sources then tar(root.resolve("sources-in"), root.resolve("sources.tar")).as(())
        else ZIO.succeed(())
      packed.flatMap { _ =>
        ZIO
          .attempt {
            val home    = root.resolve("home")
            val repo    = root.resolve("repo")
            val bin     = root.resolve("bin")
            val log     = root.resolve("java.log")
            val hits    = new CopyOnWriteArrayList[String]()
            val javaBin = bin.resolve("java")
            Files.createDirectories(home.resolve("bin"))
            Files.createDirectories(repo)
            Files.createDirectories(bin)
            val script = shimSource
            Files.copy(script, home.resolve("bin/histos"))
            scriptMode(home.resolve("bin/histos"))
            Files.writeString(
              javaBin,
              s"""#!/bin/sh
             |printf '%s\\n' "$$@" >> "${log}"
             |if [ "$$1" = "-version" ]; then
             |  echo 'openjdk version "$javaVersion"' >&2
             |  exit 0
             |fi
             |for arg in "$$@"; do
             |  if [ "$$arg" = client ]; then
             |    mkdir -p dist
             |    printf 'linked\\n' > dist/histos-client
             |    chmod +x dist/histos-client
             |  fi
             |done
             |exit 0
             |""".stripMargin,
            )
            scriptMode(javaBin)
            val clang = bin.resolve("clang")
            Files.writeString(clang, "#!/bin/sh\nexit 0\n")
            scriptMode(clang)
            val files           = scala.collection.mutable.LinkedHashMap.empty[String, Array[Byte]]
            val manifestClients = clients.map { (platform, _) =>
              platform -> Sha256.of(Chunk.fromArray(s"client-$platform".getBytes)).value
            }
            if sources then
              val archive = root.resolve("sources.tar")
              files.put(DistLayout.sources(Version), Files.readAllBytes(archive))
            val manifest = DistLayout.manifestJson(
              Version,
              jarSha,
              manifestClients,
              if sources then Some(Sha256.of(Chunk.fromArray(Files.readAllBytes(root.resolve("sources.tar")))).value)
              else None,
            )
            files.put(DistLayout.manifest(Version), manifest.getBytes(StandardCharsets.UTF_8))
            files.put(DistLayout.jar(Version), JarBytes)
            manifestClients.foreach { (platform, _) =>
              files.put(DistLayout.client(Version, platform), s"client-$platform".getBytes(StandardCharsets.UTF_8))
            }
            files.put(DistLayout.channel(Pointer), (Version + "\n").getBytes(StandardCharsets.UTF_8))
            val shimBytes = Files.readAllBytes(script) ++ remoteSuffix.getBytes(StandardCharsets.UTF_8)
            files.put(DistLayout.Shim, shimBytes)
            files.put(
              DistLayout.ShimSha,
              (Sha256.of(Chunk.fromArray(shimBytes)).value + "\n").getBytes(StandardCharsets.UTF_8),
            )
            val server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0)
            server.createContext(
              "/",
              (exchange: HttpExchange) =>
                val path = exchange.getRequestURI.getPath.stripPrefix("/")
                hits.add(path)
                files.get(path) match
                  case Some(body) =>
                    exchange.sendResponseHeaders(200, body.length)
                    val out = exchange.getResponseBody
                    out.write(body)
                    out.close()
                  case None =>
                    exchange.sendResponseHeaders(404, -1)
                    exchange.close(),
            )
            server.start()
            val base = s"http://127.0.0.1:${server.getAddress.getPort}"
            Files.writeString(root.resolve("dist-base"), base)
            Files.writeString(root.resolve("path-bin"), bin.toString)
            sys.addShutdownHook(server.stop(0))
            (home, repo, hits, log)
          }
          .mapError(error => new IOException(error.getMessage))
      }
    }

  private def runShim(home: Path, repo: Path, args: String*): ZIO[Any, Throwable, ProcessExit] =
    runShimFull(home, repo, args*).map(_._1)

  private def runShimFull(home: Path, repo: Path, args: String*): ZIO[Any, Throwable, (ProcessExit, String)] =
    for
      base   <- ZIO.attempt(Files.readString(home.getParent.resolve("dist-base")).trim)
      bin    <- ZIO.attempt(Files.readString(home.getParent.resolve("path-bin")).trim)
      path   <- System.env("PATH").map(_.getOrElse(""))
      events <- ChildProcess
        .command(
          home.resolve("bin/histos").toString :: args.toList,
          repo,
          Map(
            "HISTOS_HOME" -> home.toString,
            "HISTOS_DIST" -> base,
            "PATH"        -> s"$bin:$path",
          ),
        )
        .runCollect
        .mapError(error => new IOException(error.toString))
      exit = events.collect { case ChildEvent.Exited(code) => code }.headOption
      err  = events.collect { case ChildEvent.Err(line) => line }.mkString("\n")
      code <- ZIO.fromOption(exit).orElseFail(new IOException("shim did not exit"))
    yield (code, err)

  private def shimSource: Path =
    sys.env.get("HISTOS_REPO") match
      case Some(root) => Path.of(root).resolve("install/histos")
      case None       => Path.of("install/histos")

  private def write(path: Path, text: String): ZIO[Any, Throwable, Unit] =
    ZIO.attempt {
      Option(path.getParent).foreach(parent => Files.createDirectories(parent))
      Files.writeString(path, text)
    }.unit

  private def text(path: Path): ZIO[Any, Throwable, String] =
    ZIO.attempt(Files.readString(path))

  private def scriptMode(path: Path): Unit =
    val file = path.toFile
    file.setExecutable(true)
    file.setReadable(true)

  private def logLines(log: Path): List[String] =
    if Files.exists(log) then Files.readString(log).split('\n').toList.filter(_.nonEmpty) else Nil

  private def tar(dir: Path, out: Path): ZIO[Any, IOException, Path] =
    ZIO
      .attempt {
        Files.createDirectories(dir)
        Files.writeString(dir.resolve("README"), "sources\n")
        val process = new ProcessBuilder("tar", "-cf", out.toString, "-C", dir.toString, ".").start()
        process.waitFor()
      }
      .mapError(error => new IOException(error.getMessage))
      .flatMap { code =>
        if code == 0 then ZIO.succeed(out) else ZIO.fail(new IOException(s"tar exited $code"))
      }
end ShimSpec
