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
    test("a piped install fetches the published shim"):
      scene(remoteSuffix = "\n# published\n").flatMap { case (home, _, hits, _) =>
        for
          script <- ZIO.attempt(Files.readString(shimSource))
          base   <- ZIO.attempt(Files.readString(home.getParent.resolve("dist-base")).trim)
          exit   <- piped(home, script, base)
          text   <- ZIO.attempt(Files.readString(home.resolve("bin/histos")))
        yield assertTrue(
          exit.value == 0,
          text.contains("# published"),
          hits.contains(DistLayout.Shim),
          hits.contains(DistLayout.ShimSha),
        )
      }
    ,
    test("a symlink to the installed shim dispatches instead of copying itself"):
      scene().flatMap { case (home, repo, hits, _) =>
        val linked = home.getParent.resolve("local/bin/histos")
        val name   = "0.0.2-installed"
        for
          _ <- ZIO.attempt {
            Files.createDirectories(linked.getParent)
            Files.createDirectories(home.resolve(s"toolchains/$name"))
            Files.createSymbolicLink(linked, home.resolve("bin/histos"))
          }
          (exit, out, err) <- runAt(linked, home, repo, "toolchain", "list")
          text             <- ZIO.attempt(Files.readString(home.resolve("bin/histos")))
        yield assertTrue(
          exit.value == 0,
          out.split('\n').contains(name),
          !err.contains("same file"),
          !hits.contains(DistLayout.Shim),
          text.contains("Histos user install"),
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
    test("a fresh install writes the published channel and new dispatches"):
      scene(clients = Nil).flatMap { case (home, _, _, log) =>
        val outside = home.getParent.resolve("outside")
        for
          script         <- ZIO.attempt(Files.readString(shimSource))
          base           <- ZIO.attempt(Files.readString(home.getParent.resolve("dist-base")).trim)
          (exit, _, err) <- pipedFull(home, script, base)
          written        <- text(home.resolve("default"))
          _              <- ZIO.attempt(Files.createDirectories(outside))
          (ran, runErr)  <- runShimFull(home, outside, "new", "foo")
          logged         <- ZIO.attempt(logLines(log))
        yield assertTrue(
          exit.value == 0,
          !err.contains("cannot open"),
          written.trim == Pointer,
          ran.value == 0,
          !runErr.contains("cannot open"),
          !runErr.contains("no default toolchain"),
          logged.contains("new"),
          logged.contains("foo"),
        )
      }
    ,
    test("update outside a project with no default resolves the published channel"):
      scene().flatMap { case (home, _, hits, _) =>
        val outside = home.getParent.resolve("outside")
        ZIO.attempt(Files.createDirectories(outside)) *>
          runShimFull(home, outside, "update").flatMap { (exit, err) =>
            text(home.resolve("default")).map { written =>
              assertTrue(
                exit.value == 0,
                written.trim == Version,
                !err.contains("cannot open"),
                !err.contains("no default toolchain"),
                Files.exists(home.resolve(s"toolchains/$Version/histos.jar")),
                hits.contains("channel"),
                hits.contains(DistLayout.channel(Pointer)),
              )
            }
          }
      }
    ,
    test("toolchain install sets the default when none exists"):
      scene().flatMap { case (home, repo, _, _) =>
        runShim(home, repo, "toolchain", "install", Version).flatMap { exit =>
          text(home.resolve("default")).map { written =>
            assertTrue(exit.value == 0, written.trim == Version)
          }
        }
      }
    ,
    test("toolchain install leaves an existing default in place"):
      scene().flatMap { case (home, repo, _, _) =>
        write(home.resolve("default"), "kept\n") *>
          runShim(home, repo, "toolchain", "install", Version).flatMap { exit =>
            text(home.resolve("default")).map { written =>
              assertTrue(exit.value == 0, written.trim == "kept")
            }
          }
      }
    ,
    test("a fresh install prefers the newest snapshot channel over a release"):
      scene(
        channel = None,
        assets = Some("channel-0.0.1-SNAPSHOT\nchannel-0.0.2-SNAPSHOT\n0.9.0-manifest.json\n"),
      ).flatMap { case (home, _, _, _) =>
        for
          script  <- ZIO.attempt(Files.readString(shimSource))
          base    <- ZIO.attempt(Files.readString(home.getParent.resolve("dist-base")).trim)
          exit    <- piped(home, script, base)
          written <- text(home.resolve("default"))
        yield assertTrue(exit.value == 0, written.trim == Pointer)
      }
    ,
    test("a fresh install uses the latest release when no snapshot channel is published"):
      scene(
        channel = None,
        assets = Some(
          "0.0.1-manifest.json\n0.0.2-manifest.json\n0.0.2-20260101.120000-1-manifest.json\n"
        ),
      ).flatMap { case (home, _, _, _) =>
        for
          script  <- ZIO.attempt(Files.readString(shimSource))
          base    <- ZIO.attempt(Files.readString(home.getParent.resolve("dist-base")).trim)
          exit    <- piped(home, script, base)
          written <- text(home.resolve("default"))
        yield assertTrue(exit.value == 0, written.trim == "0.0.2")
      }
    ,
    test("a fresh home with nothing published fails in one line"):
      scene(channel = None).flatMap { case (home, _, _, _) =>
        val outside = home.getParent.resolve("outside")
        ZIO.attempt(Files.createDirectories(outside)) *>
          runShimFull(home, outside, "new", "foo").map { (exit, err) =>
            val lines = err.split('\n').toList.filter(_.nonEmpty)
            assertTrue(
              exit.value != 0,
              lines == List("histos: no default toolchain. Run 'histos toolchain install <version>'"),
              !err.contains("cannot open"),
            )
          }
      }
    ,
    test("a snapshot pin reads the channel once inside the ttl"):
      scene(clients = Nil).flatMap { case (home, repo, hits, _) =>
        val channel = DistLayout.channel(Pointer)
        write(repo.resolve("build.properties"), s"histos.version=$Pointer\n") *>
          runShimFull(home, repo, "version").flatMap { (first, firstErr) =>
            val once = (0 until hits.size).count(index => hits.get(index) == channel)
            runShimFull(home, repo, "version").map { (exit, err) =>
              val twice = (0 until hits.size).count(index => hits.get(index) == channel)
              assertTrue(
                first.value == 0,
                exit.value == 0,
                !firstErr.contains("did not resolve"),
                !err.contains("did not resolve"),
                once == 1,
                twice == 1,
              )
            }
          }
      }
    ,
    test("a stale snapshot pin reads the channel again"):
      scene(clients = Nil).flatMap { case (home, repo, hits, _) =>
        val channel = DistLayout.channel(Pointer)
        val cache   = home.resolve("channels").resolve(Pointer)
        write(repo.resolve("build.properties"), s"histos.version=$Pointer\n") *>
          runShim(home, repo, "version") *>
          ZIO.attempt {
            java.nio.file.Files.setLastModifiedTime(
              cache,
              java.nio.file.attribute.FileTime.fromMillis(java.lang.System.currentTimeMillis - 7200000L),
            )
          } *>
          runShimFull(home, repo, "version").map { (exit, err) =>
            val seen = (0 until hits.size).count(index => hits.get(index) == channel)
            assertTrue(exit.value == 0, !err.contains("did not resolve"), seen == 2)
          }
      }
    ,
    test("a stale snapshot pin keeps the cached revision when the channel is down"):
      scene().flatMap { case (home, repo, hits, _) =>
        val cache = home.resolve("channels").resolve(Pointer)
        val jar   = home.resolve("toolchains").resolve(Version).resolve("histos.jar")
        write(repo.resolve("build.properties"), s"histos.version=$Pointer\n") *>
          write(cache, s"$Version\n") *>
          write(jar, new String(JarBytes, StandardCharsets.UTF_8)) *>
          ZIO.attempt {
            java.nio.file.Files.setLastModifiedTime(
              cache,
              java.nio.file.attribute.FileTime.fromMillis(java.lang.System.currentTimeMillis - 7200000L),
            )
          } *>
          runShimDist(home, repo, Some("http://127.0.0.1:1"), "version").map { (exit, err) =>
            val seen = (0 until hits.size).count(index => hits.get(index) == DistLayout.channel(Pointer))
            assertTrue(exit.value == 0, seen == 0, !err.contains("did not resolve"))
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
      channel: Option[String] = Some(Pointer),
      assets: Option[String] = None,
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
            channel.foreach(value => files.put("channel", (value + "\n").getBytes(StandardCharsets.UTF_8)))
            assets.foreach(value => files.put("assets", value.getBytes(StandardCharsets.UTF_8)))
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

  private def piped(home: Path, script: String, base: String): ZIO[Any, Throwable, ProcessExit] =
    pipedFull(home, script, base).map(_._1)

  private def pipedFull(home: Path, script: String, base: String): ZIO[Any, Throwable, (ProcessExit, String, String)] =
    for
      events <- ChildProcess
        .command(
          List("sh", "-c", script),
          home.getParent,
          Map(
            "HISTOS_HOME" -> home.toString,
            "HISTOS_DIST" -> base,
          ),
        )
        .runCollect
        .mapError(error => new IOException(error.toString))
      exit = events.collect { case ChildEvent.Exited(code) => code }.headOption
      out  = events.collect { case ChildEvent.Out(line) => line }.mkString("\n")
      err  = events.collect { case ChildEvent.Err(line) => line }.mkString("\n")
      code <- ZIO.fromOption(exit).orElseFail(new IOException(s"shim did not exit: $err"))
    yield (code, out, err)

  private def runShim(home: Path, repo: Path, args: String*): ZIO[Any, Throwable, ProcessExit] =
    runShimFull(home, repo, args*).map(_._1)

  /** Runs `command` with `HISTOS_HOME` set, so a symlink can be the argv0. */
  private def runAt(
      command: Path,
      home: Path,
      repo: Path,
      args: String*
  ): ZIO[Any, Throwable, (ProcessExit, String, String)] =
    for
      base   <- ZIO.attempt(Files.readString(home.getParent.resolve("dist-base")).trim)
      bin    <- ZIO.attempt(Files.readString(home.getParent.resolve("path-bin")).trim)
      path   <- System.env("PATH").map(_.getOrElse(""))
      events <- ChildProcess
        .command(
          command.toString :: args.toList,
          repo,
          Map(
            "HISTOS_HOME" -> home.toString,
            "HISTOS_DIST" -> base,
            "PATH"        -> s"$bin:$path",
          ),
        )
        .runCollect
        .mapError(error => new IOException(error.toString))
      exit = events.collect { case ChildEvent.Exited(code) => code }
      out  = events.collect { case ChildEvent.Out(line) => line }.mkString("\n")
      err  = events.collect { case ChildEvent.Err(line) => line }.mkString("\n")
      code <- ZIO.fromOption(exit.headOption).orElseFail(new IOException(s"shim did not exit: $err"))
    yield (code, out, err)

  private def runShimFull(home: Path, repo: Path, args: String*): ZIO[Any, Throwable, (ProcessExit, String)] =
    runShimDist(home, repo, None, args*)

  private def runShimDist(
      home: Path,
      repo: Path,
      dist: Option[String],
      args: String*
  ): ZIO[Any, Throwable, (ProcessExit, String)] =
    for
      base   <- ZIO.attempt(dist.getOrElse(Files.readString(home.getParent.resolve("dist-base")).trim))
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
