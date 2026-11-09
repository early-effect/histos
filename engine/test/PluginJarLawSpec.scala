package histos.load

import histos.plugin.{ApiDigest, EntryName, HistosPlugin, PluginCoordinate, PluginIo, PluginLoadError}
import histos.workspace.{IoFailure, Workspace}
import histos.{Dep, DepKind}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.util.zip.{ZipEntry, ZipOutputStream}

import zio.{Chunk, ZIO}
import zio.test.*

/** A plugin jar is one entry line and one api digest. A class under histos, zio, or scala is a leak.
  */
object PluginJarLawSpec extends ZIOSpecDefault:
  private val dep   = Dep.trusted("org", "name", "1.0.0", DepKind.Binary)
  private val coord = "org::name:1.0.0"
  private val entry = "demo.Plugin$"
  private val hex   = "ab" * 32

  def spec = suite("plugin jar")(
    test("the direct jar is its entry, the api it was built against, and a reserved class when one is present"):
      ZIO.scoped {
        Workspace.work("histos-plugin-jar").flatMap { root =>
          val jar = root.resolve("plugin.jar")
          for
            digest <- digestOf(hex)
            name   <- entryOf(entry)
            _      <- pack(
              jar,
              HistosPlugin.EntryResourceName -> s"\n$entry\n",
              ApiDigest.resource             -> hex.toUpperCase,
              "demo/Ok.class"                -> "x",
              "histos/Foo.class"             -> "x",
              "histos/readme.txt"            -> "x",
            )
            read <- PluginJar.read(dep, jar)
          yield assertTrue(
            read.entry == name,
            read.published == digest,
            read.leak == Some("histos.Foo"),
          )
        }
      }
    ,
    test("a missing line, two lines, a reserved name, and a bad digest are those load failures"):
      ZIO.scoped {
        Workspace.work("histos-plugin-jar-bad").flatMap { root =>
          for
            empty     <- packed(root.resolve("empty.jar"), HistosPlugin.EntryResourceName -> "")
            many      <- packed(root.resolve("many.jar"), HistosPlugin.EntryResourceName -> s"$entry\nother.Plugin$$")
            reserved  <- packed(root.resolve("reserved.jar"), HistosPlugin.EntryResourceName -> "histos.Nope$")
            badName   <- packed(root.resolve("bad.jar"), HistosPlugin.EntryResourceName -> "not a name")
            noVersion <- packed(root.resolve("none.jar"), HistosPlugin.EntryResourceName -> entry)
            two       <- packed(
              root.resolve("two.jar"),
              HistosPlugin.EntryResourceName -> entry,
              ApiDigest.resource             -> s"$hex\n$hex",
            )
            badHex <- packed(
              root.resolve("hex.jar"),
              HistosPlugin.EntryResourceName -> entry,
              ApiDigest.resource             -> "zz",
            )
            opened <- PluginJar.read(dep, root).either
          yield assertTrue(
            empty == Left(LoadError.Plugin(PluginLoadError.NoEntry(coord))),
            many == Left(LoadError.Plugin(PluginLoadError.AmbiguousEntry(coord))),
            reserved == Left(LoadError.Plugin(PluginLoadError.ReservedPackage("histos.Nope$"))),
            badName == Left(LoadError.Plugin(PluginLoadError.NoEntry(coord))),
            noVersion == Left(LoadError.Plugin(PluginLoadError.Unversioned(coord))),
            two == Left(LoadError.Plugin(PluginLoadError.Unversioned(coord))),
            badHex == Left(LoadError.Plugin(PluginLoadError.Unversioned(coord))),
            opened == Left(LoadError.Plugin(PluginLoadError.Io(PluginIo.Open(root)))),
          )
        }
      }
    ,
    test("a path plugin is the entry file in its class directory, and a reserved class file is the engine leak"):
      ZIO.scoped {
        Workspace.work("histos-plugin-tree").flatMap { root =>
          val classes = root.resolve("classes")
          val path    = histos.ProjectPath.trusted("plugins/extra")
          val where   = PluginCoordinate.Path(path)
          for
            name    <- entryOf(entry)
            _       <- write(classes.resolve(HistosPlugin.EntryResourceName), s"  $entry  \n")
            read    <- PluginJar.readTree(where, classes)
            missing <- PluginJar.readTree(where, root.resolve("absent")).either
            _       <- write(classes.resolve("scala").resolve("a").resolve("B.class"), "x")
            leaked  <- PluginCompile.engineClass(classes)
            plain   <- PluginCompile.engineClass(root.resolve("plain"))
            _       <- write(root.resolve("plain").resolve("demo").resolve("A.class"), "x")
            demo    <- PluginCompile.engineClass(root.resolve("plain"))
            file    <- PluginCompile.engineClass(root.resolve("nope.txt"))
          yield assertTrue(
            read == name,
            missing == Left(LoadError.Plugin(PluginLoadError.NoEntry(path.value))),
            leaked == Some("scala.a.B"),
            plain == None,
            demo == None,
            file == None,
          )
        }
      },
  )

  private def digestOf(raw: String) =
    ZIO.fromEither(ApiDigest.from(raw)).catchAll(error => ZIO.dieMessage(error.message))

  private def entryOf(raw: String) =
    ZIO.fromEither(EntryName.from(raw)).catchAll(error => ZIO.dieMessage(error.message))

  private def packed(path: Path, entries: (String, String)*) =
    pack(path, entries*).flatMap(_ => PluginJar.read(dep, path).either)

  private def pack(path: Path, entries: (String, String)*): zio.UIO[Unit] =
    ZIO.attemptBlocking {
      Option(path.getParent).foreach(parent => Files.createDirectories(parent))
      val out = new ZipOutputStream(Files.newOutputStream(path))
      try
        entries.foreach { (name, text) =>
          out.putNextEntry(new ZipEntry(name))
          out.write(text.getBytes(StandardCharsets.UTF_8))
          out.closeEntry()
        }
      finally out.close()
    }.orDie

  private def write(path: Path, text: String) =
    Workspace.writeString(path, text).catchAll((error: IoFailure) => ZIO.dieMessage(error.message))
end PluginJarLawSpec
