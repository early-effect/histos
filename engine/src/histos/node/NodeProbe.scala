package histos.node

import histos.jdk.{Machine, Os}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import zio.{IO, ZIO}

final case class NodeScan(
    explicit: Option[Path],
    onPath: Option[Path],
    nvm: NodeManagerProbe,
    fnm: NodeManagerProbe,
    volta: NodeManagerProbe,
    cache: Path,
)

object NodeLocations:
  def nvmRoot(userHome: String, env: Map[String, String]): Path =
    env.get("NVM_DIR").filter(_.nonEmpty) match
      case Some(dir) => Path.of(dir)
      case None      => Path.of(userHome, ".nvm")

  def fnmRoot(userHome: String, os: Os, env: Map[String, String]): Path =
    env.get("FNM_DIR").filter(_.nonEmpty) match
      case Some(dir) => Path.of(dir)
      case None =>
        env.get("XDG_DATA_HOME").filter(_.nonEmpty) match
          case Some(dir) => Path.of(dir, "fnm")
          case None =>
            os match
              case Os.Mac => Path.of(userHome, "Library", "Application Support", "fnm")
              case Os.Linux | Os.Windows | Os.Other => Path.of(userHome, ".local", "share", "fnm")

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
    val nvm   = nvmRoot(machine.userHome, machine.env)
    val fnm   = fnmRoot(machine.userHome, machine.os, machine.env)
    val volta = voltaRoot(machine.userHome, machine.env)
    for
      nvmDir   <- isDir(nvm)
      fnmDir   <- isDir(fnm)
      voltaDir <- isDir(volta)
      nvmBin   <- toolOnPath(machine.env.get("PATH"), "nvm")
      fnmBin   <- toolOnPath(machine.env.get("PATH"), "fnm")
      voltaBin <- toolOnPath(machine.env.get("PATH"), "volta")
      node     <- firstNode(machine.env.get("PATH"), machine.os)
    yield
      NodeScan(
        explicit,
        node,
        manager(nvmDir || nvmBin, Some(nvmVersions(nvm))),
        manager(fnmDir || fnmBin, Some(fnmVersions(fnm))),
        manager(voltaDir || voltaBin, Some(voltaVersions(volta))),
        machine.cache.resolve("nodes"),
      )

  private def manager(installed: Boolean, versions: Option[Path]): NodeManagerProbe =
    if installed then NodeManagerProbe.Installed(versions) else NodeManagerProbe.Absent

  private def isDir(path: Path): IO[NodeError, Boolean] =
    ZIO.attempt(Files.isDirectory(path)).mapError(_ => NodeError.Unreadable(path))

  private def toolOnPath(pathEnv: Option[String], name: String): IO[NodeError, Boolean] =
    ZIO.attempt {
      entries(pathEnv).exists: dir =>
        Files.isRegularFile(dir.resolve(name)) || Files.isRegularFile(dir.resolve(name + ".exe"))
    }.mapError(_ => NodeError.PathScan)

  private def firstNode(pathEnv: Option[String], os: Os): IO[NodeError, Option[Path]] =
    val names = os match
      case Os.Windows => List("node.exe")
      case _          => List("node")
    ZIO.attempt {
      entries(pathEnv).flatMap { dir =>
        names.flatMap { name =>
          val bin = dir.resolve(name)
          if Files.isRegularFile(bin) then nodeHome(bin).toList else Nil
        }
      }.headOption
    }.mapError(_ => NodeError.PathScan)

  private def entries(pathEnv: Option[String]): List[Path] =
    pathEnv.filter(_.nonEmpty).toList.flatMap(_.split(java.io.File.pathSeparator).toList.filter(_.nonEmpty)).map(Path.of(_))

  private def nodeHome(binary: Path): Option[Path] =
    Option(binary.getParent).flatMap(parent => Option(parent.getParent))

object NodeProbe:
  private val MaxDepth: Int = 8

  def scan(input: NodeScan): IO[NodeError, NodeView] =
    for
      explicit <- locate(input.explicit)
      onPath   <- locate(input.onPath)
      nvm      <- managers(input.nvm)
      fnm      <- managers(input.fnm)
      volta    <- managers(input.volta)
      cached   <- homes(input.cache)
    yield NodeView(explicit, onPath, nvm, fnm, volta, cached)

  def homes(root: Path): IO[NodeError, List[NodeHome]] = listHomes(root)

  private def locate(path: Option[Path]): IO[NodeError, NodeLocated] =
    path match
      case None => ZIO.succeed(NodeLocated.Unset)
      case Some(dir) =>
        ZIO.attempt(Files.isDirectory(dir)).mapError(_ => NodeError.Unreadable(dir)).flatMap:
          case false => ZIO.succeed(NodeLocated.Missing(dir))
          case true  => readHome(dir).map(NodeLocated.Found(_))

  private def managers(probe: NodeManagerProbe): IO[NodeError, NodeManagerSlot] =
    probe match
      case NodeManagerProbe.Absent               => ZIO.succeed(NodeManagerSlot.Absent)
      case NodeManagerProbe.Installed(None)      => ZIO.succeed(NodeManagerSlot.Present(Nil))
      case NodeManagerProbe.Installed(Some(dir)) => listHomes(dir).map(NodeManagerSlot.Present(_))

  private def listHomes(root: Path): IO[NodeError, List[NodeHome]] =
    ZIO.attempt {
      if !Files.isDirectory(root) then Nil
      else
        val walk = Files.walk(root, MaxDepth)
        try
          val found = walk.iterator.asScala.filter(dir => Files.isDirectory(dir) && isHome(dir)).toList
          shallow(distinct(found))
        finally walk.close()
    }.mapError(_ => NodeError.Unreadable(root)).flatMap(dirs => ZIO.foreach(dirs)(readHome))

  private def isHome(dir: Path): Boolean =
    Files.isRegularFile(dir.resolve("bin").resolve("node")) ||
      Files.isRegularFile(dir.resolve("bin").resolve("node.exe"))

  private def distinct(dirs: List[Path]): List[Path] =
    dirs.foldLeft((List.empty[Path], Set.empty[String])):
      case ((kept, seen), dir) =>
        val key =
          try dir.toRealPath().normalize.toString
          catch case _: java.io.IOException => dir.normalize.toString
        if seen.contains(key) then (kept, seen) else (kept :+ dir, seen + key)
    ._1

  private def shallow(dirs: List[Path]): List[Path] =
    dirs.sortBy(path => (path.getNameCount, path.toString)).foldLeft(List.empty[Path]): (kept, dir) =>
      val normal = dir.normalize
      if kept.exists(parent => normal.startsWith(parent.normalize)) then kept else kept :+ dir

  private def readHome(dir: Path): IO[NodeError, NodeHome] =
    val binary = dir.resolve("bin").resolve("node")
    val windows = dir.resolve("bin").resolve("node.exe")
    val chosen =
      if Files.isRegularFile(binary) then Some(binary)
      else if Files.isRegularFile(windows) then Some(windows)
      else None
    chosen match
      case None => ZIO.succeed(NodeHome(dir, hasBinary = false, version = Left(NodeVersionError.Missing)))
      case Some(file) =>
        runnable(file) *> versionOf(file).foldZIO(
          {
            case NodeError.Version(_, error) => ZIO.succeed(NodeHome(dir, hasBinary = true, version = Left(error)))
            case other                       => ZIO.fail(other)
          },
          version => ZIO.succeed(NodeHome(dir, hasBinary = true, version = Right(version))),
        )

  private def versionOf(binary: Path): IO[NodeError, NodeVersion] =
    ZIO.attemptBlockingInterrupt {
      val process = new ProcessBuilder(binary.toString, "--version").redirectErrorStream(true).start()
      try
        val text = new String(process.getInputStream.readAllBytes, StandardCharsets.UTF_8).trim
        val code = process.waitFor()
        (code, text)
      finally process.destroy()
    }.mapError(_ => NodeError.Unreadable(binary)).flatMap:
      case (0, text) =>
        ZIO.fromEither(NodeVersion.from(text)).mapError(error => NodeError.Version(binary, error))
      case (code, _) => ZIO.fail(NodeError.Version(binary, NodeVersionError.Status(code)))

  /** A script unpacked from a zip has no execute bit. `node --version` is the check that it runs. */
  private def runnable(binary: Path): IO[NodeError, Unit] =
    ZIO.attempt(binary.toFile.setExecutable(true)).mapError(_ => NodeError.Unreadable(binary)).unit
