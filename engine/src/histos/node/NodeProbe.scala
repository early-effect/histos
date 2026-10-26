package histos.node

import histos.process.{ChildEvent, ChildProcess}

import java.nio.file.{Files, Path}

import zio.{IO, ZIO}

object NodeProbe:
  /** The executable a home launches. */
  def binary(home: Path): IO[NodeError, NodeBinary] =
    ZIO
      .attemptBlocking(launcher(home))
      .mapError(_ =>
        NodeError.Io(
          histos.workspace.IoFailure(home, histos.workspace.IoFault.Stat)
        )
      )
      .flatMap:
        case Some(file) => ZIO.succeed(NodeBinary(file))
        case None       =>
          ZIO.fail(
            NodeError.Version(
              home.resolve("bin").resolve("node"),
              NodeVersionError.Missing,
            )
          )

  private def launcher(dir: Path): Option[Path] =
    val binary  = dir.resolve("bin").resolve("node")
    val windows = dir.resolve("bin").resolve("node.exe")
    if Files.isRegularFile(binary) then Some(binary)
    else if Files.isRegularFile(windows) then Some(windows)
    else None

  def home(dir: Path): IO[NodeError, NodeHome] =
    ZIO
      .attemptBlocking(launcher(dir))
      .mapError(_ =>
        NodeError.Io(
          histos.workspace.IoFailure(dir, histos.workspace.IoFault.Stat)
        )
      )
      .flatMap:
        case None =>
          ZIO.succeed(
            NodeHome(
              dir,
              hasBinary = false,
              version = Left(NodeVersionError.Missing),
            )
          )
        case Some(file) =>
          ZIO
            .attemptBlocking(Files.isExecutable(file))
            .mapError(_ =>
              NodeError.Io(
                histos.workspace.IoFailure(file, histos.workspace.IoFault.Stat)
              )
            )
            .flatMap:
              case false =>
                ZIO.succeed(
                  NodeHome(
                    dir,
                    hasBinary = true,
                    version = Left(NodeVersionError.NotExecutable(file)),
                  )
                )
              case true =>
                versionOf(file).foldZIO(
                  {
                    case NodeError.Version(_, error) =>
                      ZIO.succeed(
                        NodeHome(dir, hasBinary = true, version = Left(error))
                      )
                    case other => ZIO.fail(other)
                  },
                  version =>
                    ZIO.succeed(
                      NodeHome(dir, hasBinary = true, version = Right(version))
                    ),
                )

  private def versionOf(binary: Path): IO[NodeError, NodeVersion] =
    ZIO.scoped:
      for
        work <- histos.workspace.Workspace
          .work("histos-node-version")
          .mapError(NodeError.Io(_))
        events <- ChildProcess
          .command(List(binary.toString, "--version"), work, Map.empty)
          .runCollect
          .mapError(error => NodeError.Version(binary, NodeVersionError.Process(error)))
        code <- ZIO
          .fromOption(events.collect { case ChildEvent.Exited(code) =>
            code
          }.lastOption)
          .orElseFail(NodeError.Version(binary, NodeVersionError.MissingStatus))
        lines = events.collect:
          case ChildEvent.Out(line) => line
        text = lines.mkString("\n").trim
        version <-
          if code.value == 0 then
            ZIO
              .fromEither(NodeVersion.from(text))
              .mapError(error => NodeError.Version(binary, error))
          else
            ZIO.fail(
              NodeError.Version(binary, NodeVersionError.Status(code.value))
            )
      yield version
end NodeProbe
