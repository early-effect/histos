package histos.node

import histos.jdk.{Machine, Os}
import histos.process.{ChildEvent, ChildProcess}

import java.nio.file.{Files, Path}

import histos.toolchain.RuntimeFiles

import zio.{IO, ZIO}

final case class NodeScan(
    explicit: Option[Path],
    onPath: Option[Path],
    nvm: NodeManagerProbe,
    fnm: NodeManagerProbe,
    volta: NodeManagerProbe,
    cache: Path
)

object NodeLocations:
  def nvmRoot(userHome: String, env: Map[String, String]): Path =
    env.get("NVM_DIR").filter(_.nonEmpty) match
      case Some(dir) => Path.of(dir)
      case None      => Path.of(userHome, ".nvm")

  def fnmRoot(userHome: String, os: Os, env: Map[String, String]): Path =
    env.get("FNM_DIR").filter(_.nonEmpty) match
      case Some(dir) => Path.of(dir)
      case None      =>
        env.get("XDG_DATA_HOME").filter(_.nonEmpty) match
          case Some(dir) => Path.of(dir, "fnm")
          case None      =>
            os match
              case Os.Mac =>
                Path.of(userHome, "Library", "Application Support", "fnm")
              case Os.Linux | Os.Windows | Os.Other =>
                Path.of(userHome, ".local", "share", "fnm")

  def voltaRoot(userHome: String, env: Map[String, String]): Path =
    env.get("VOLTA_HOME").filter(_.nonEmpty) match
      case Some(dir) => Path.of(dir)
      case None      => Path.of(userHome, ".volta")

  def nvmVersions(root: Path): Path =
    root.resolve("versions").resolve("node")

  def fnmVersions(root: Path): Path =
    root.resolve("node-versions")

  def voltaVersions(root: Path): Path =
    root.resolve("tools").resolve("image").resolve("node")

  def probe(machine: Machine, explicit: Option[Path]): IO[NodeError, NodeScan] =
    val nvm = nvmRoot(machine.userHome, machine.env)
    val fnm = fnmRoot(machine.userHome, machine.os, machine.env)
    val volta = voltaRoot(machine.userHome, machine.env)
    for
      nvmDir <- isDir(nvm)
      fnmDir <- isDir(fnm)
      voltaDir <- isDir(volta)
      nvmBin <- toolOnPath(machine.env.get("PATH"), "nvm")
      fnmBin <- toolOnPath(machine.env.get("PATH"), "fnm")
      voltaBin <- toolOnPath(machine.env.get("PATH"), "volta")
      node <- firstNode(machine.env.get("PATH"), machine.os)
    yield NodeScan(
      explicit,
      node,
      manager(nvmDir || nvmBin, Some(nvmVersions(nvm))),
      manager(fnmDir || fnmBin, Some(fnmVersions(fnm))),
      manager(voltaDir || voltaBin, Some(voltaVersions(volta))),
      machine.cache.resolve("nodes")
    )

  private def manager(
      installed: Boolean,
      versions: Option[Path]
  ): NodeManagerProbe =
    if installed then NodeManagerProbe.Installed(versions)
    else NodeManagerProbe.Absent

  private def isDir(path: Path): IO[NodeError, Boolean] =
    ZIO
      .attemptBlocking(Files.isDirectory(path))
      .mapError(_ =>
        NodeError.Io(
          histos.workspace.IoFailure(path, histos.workspace.IoFault.Stat)
        )
      )

  private def toolOnPath(
      pathEnv: Option[String],
      name: String
  ): IO[NodeError, Boolean] =
    RuntimeFiles
      .firstOnPath(pathEnv, List(name, name + ".exe", name + ".bat"))
      .mapError(NodeError.Locations(_))
      .map(_.nonEmpty)

  private def firstNode(
      pathEnv: Option[String],
      os: Os
  ): IO[NodeError, Option[Path]] =
    val name = if os == Os.Windows then "node.exe" else "node"
    RuntimeFiles
      .firstOnPath(pathEnv, List(name))
      .mapError(NodeError.Locations(_))
      .map(_.flatMap(RuntimeFiles.homeOf))

object NodeProbe:
  private val MaxDepth: Int = 8

  def scan(input: NodeScan): IO[NodeError, NodeView] =
    for
      explicit <- locate(input.explicit)
      onPath <- locate(input.onPath)
      nvm <- managers(input.nvm)
      fnm <- managers(input.fnm)
      volta <- managers(input.volta)
      cached <- homes(input.cache)
    yield NodeView(explicit, onPath, nvm, fnm, volta, cached)

  def homes(root: Path): IO[NodeError, List[NodeHome]] = listHomes(root)

  def locate(path: Option[Path]): IO[NodeError, NodeLocated] =
    path match
      case None      => ZIO.succeed(NodeLocated.Unset)
      case Some(dir) =>
        ZIO
          .attemptBlocking(Files.isDirectory(dir))
          .mapError(_ =>
            NodeError.Io(
              histos.workspace.IoFailure(dir, histos.workspace.IoFault.Stat)
            )
          )
          .flatMap:
            case false => ZIO.succeed(NodeLocated.Missing(dir))
            case true  => home(dir).map(NodeLocated.Found(_))

  private def managers(
      probe: NodeManagerProbe
  ): IO[NodeError, NodeManagerSlot] =
    probe match
      case NodeManagerProbe.Absent => ZIO.succeed(NodeManagerSlot.Absent)
      case NodeManagerProbe.Installed(None) =>
        ZIO.succeed(NodeManagerSlot.Present(Nil))
      case NodeManagerProbe.Installed(Some(dir)) =>
        listHomes(dir).map(NodeManagerSlot.Present(_))

  private def listHomes(root: Path): IO[NodeError, List[NodeHome]] =
    RuntimeFiles
      .homes(root, MaxDepth)(dir =>
        Files.isRegularFile(dir.resolve("bin/node")) || Files.isRegularFile(
          dir.resolve("bin/node.exe")
        )
      )
      .mapError(NodeError.Io(_))
      .flatMap(dirs => ZIO.foreach(dirs)(home))

  /** The executable a home launches. A probed home that matched has one. */
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
              NodeVersionError.Missing
            )
          )

  private def launcher(dir: Path): Option[Path] =
    val binary = dir.resolve("bin").resolve("node")
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
              version = Left(NodeVersionError.Missing)
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
                    version = Left(NodeVersionError.NotExecutable(file))
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
                    )
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
          .mapError(error =>
            NodeError.Version(binary, NodeVersionError.Process(error))
          )
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
