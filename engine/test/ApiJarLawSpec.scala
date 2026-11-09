package histos.load

import histos.workspace.Workspace

import java.nio.file.{Files, Path}
import java.util.zip.{ZipEntry, ZipFile, ZipOutputStream}

import scala.jdk.CollectionConverters.*

import histos.workspace.IoFailure

import zio.{ZIO, Scope}
import zio.test.{TestSystem, ZIOSpecDefault, assertTrue, suite, test}

/** `dist/histos-api.jar` wins. One classes directory is packed. None, or more than one, is missing. */
object ApiJarLawSpec extends ZIOSpecDefault:
  def spec = suite("api jar laws")(
    test("a packed jar wins, one classes directory is packed, and none or two are missing"):
      Workspace
        .work("histos-api-jar")
        .flatMap: root =>
          val packed = root.resolve("packed")
          val single = root.resolve("single")
          val bare   = root.resolve("bare")
          val both   = root.resolve("both")
          val jar    = packed.resolve("dist").resolve("histos-api.jar")
          val clazz  = single
            .resolve(".histos")
            .resolve("classes")
            .resolve("api")
            .resolve("3.10.0")
            .resolve("jvm")
            .resolve("histos")
            .resolve("Module.class")
          val older = both
            .resolve(".histos")
            .resolve("classes")
            .resolve("api")
            .resolve("3.9.0")
            .resolve("jvm")
            .resolve("A.class")
          val newer = both
            .resolve(".histos")
            .resolve("classes")
            .resolve("api")
            .resolve("3.10.0")
            .resolve("jvm")
            .resolve("A.class")
          for
            _           <- writeZip(jar, "marker.txt")
            _           <- writeBytes(clazz)
            _           <- writeBytes(older)
            _           <- writeBytes(newer)
            _           <- TestSystem.putEnv("HISTOS_REPO", packed.toString)
            foundPacked <- ApiJar.locate.either
            packedFacts <- facts(foundPacked)
            _           <- TestSystem.putEnv("HISTOS_REPO", single.toString)
            foundSingle <- ApiJar.locate.either
            singleFacts <- facts(foundSingle)
            _           <- TestSystem.putEnv("HISTOS_REPO", bare.toString)
            foundBare   <- ApiJar.locate.either
            _           <- TestSystem.putEnv("HISTOS_REPO", both.toString)
            foundBoth   <- ApiJar.locate.either
          yield assertTrue(
            packedFacts.exists((path, names) =>
              path.getFileName.toString == "histos-api.jar" && names.contains("marker.txt") && !names.contains(
                "histos/Module.class"
              )
            ),
            singleFacts.exists((path, names) =>
              path.getFileName.toString == "histos-api.jar" &&
                Option(path.getParent).exists(_.getFileName.toString != "dist") &&
                names.contains("histos/Module.class")
            ),
            foundBare == Left(LoadError.MissingFile(Path.of("dist/histos-api.jar"))),
            foundBoth == Left(LoadError.MissingFile(Path.of("dist/histos-api.jar"))),
          )
  )

  private def facts(found: Either[LoadError, Path]): ZIO[Any, Nothing, Option[(Path, Set[String])]] =
    found match
      case Left(_)     => ZIO.succeed(None)
      case Right(path) =>
        ZIO.attemptBlocking {
          val zip   = new ZipFile(path.toFile)
          val names =
            try zip.entries().asScala.map(_.getName).toSet
            finally zip.close()
          Some(path -> names)
        }.orDie

  private def writeBytes(path: Path): ZIO[Any, Nothing, Unit] =
    (Workspace.createDirectories(parentOf(path)) *> ZIO.attemptBlocking(Files.write(path, Array[Byte](1, 2, 3))).unit)
      .catchAll(die)

  private def writeZip(path: Path, entry: String): ZIO[Any, Nothing, Unit] =
    (Workspace.createDirectories(parentOf(path)) *> ZIO.attemptBlocking {
      val stream = new ZipOutputStream(Files.newOutputStream(path))
      try
        stream.putNextEntry(new ZipEntry(entry))
        stream.write(Array[Byte](1))
        stream.closeEntry()
      finally stream.close()
    }).catchAll(die)

  private def die(error: IoFailure | Throwable): ZIO[Any, Nothing, Nothing] =
    error match
      case failure: IoFailure => ZIO.dieMessage(failure.message)
      case thrown: Throwable  => ZIO.die(thrown)

  private def parentOf(path: Path): Path =
    Option(path.getParent).getOrElse(path)
end ApiJarLawSpec
