package histos.workflow

import histos.plugin.{CiExtra, CiHeader}
import histos.program.ClientBinary
import histos.resolve.PlatformToken
import histos.schedule.CacheNamespace
import histos.version.{Catalog, Ship}
import histos.workspace.{IoFailure, IoFault}
import histos.{
  Aggregate,
  Axis,
  DocCheck,
  FollowUp,
  Module,
  NodeFloor,
  Platform,
  ProjectGraph,
  ProjectName,
  Publishing,
  Resolution,
  RunnerTools,
  ScalaVersion,
  Splits,
  ToolRevision,
}

import java.nio.file.{AtomicMoveNotSupportedException, Files, Path, StandardCopyOption}

import scala.jdk.CollectionConverters.*

import zio.{IO, ZIO}

/** A pull request names the projects that changed. Anything else runs the full graph. */
enum CiEvent:
  case Other
  case PullRequest(changed: Set[ProjectName])

/** The module axis of one cell. `All` is not a split on [[Axis.Module]]. */
enum ModuleRef:
  case All
  case One(name: ProjectName)

/** The platform axis of one cell. `All` is not a split on [[Axis.Platform]]. */
enum PlatformRef:
  case All
  case One(platform: Platform)

/** The Scala axis of one cell. `All` is not a split on [[Axis.ScalaVersion]]. */
enum VersionRef:
  case All
  case One(version: ScalaVersion)

/** What `./histos test` is asked to run. `Entire` is the full graph, not a list of every name. */
enum TestTarget:
  case Entire
  case Named(first: ProjectName, rest: List[ProjectName])

enum TestJob:
  case Aggregate(target: TestTarget)
  case Split(module: ModuleRef, platform: PlatformRef, version: VersionRef)

/** One file `renderAll` emits. `name` is the file name under `.github/workflows`, not a path. */
final case class RenderedWorkflow(name: String, body: String)

object Workflow:
  val fileName: String = "ci.yml"

  /** Owned like a scheduled `histos-*.yml`. An empty catalog does not emit it, so update deletes a stale copy. */
  val releaseName: String = "histos-release.yml"

  /** `histos-` so update deletes a stale copy when no ship names an asset. Dispatch publishes snapshots. On the default
    * branch that command also uploads the dist channel.
    */
  val snapshotsName: String = "histos-snapshots.yml"

  /** `histos-` so update deletes a stale copy when `followUp` is Finished, on the same orphan pass as the release file.
    */
  val promoteName: String = "histos-promote.yml"

  /** `histos-` so update deletes a stale copy when no job asks for a manual deploy. */
  val pagesName: String = "histos-pages.yml"

  def path(repo: Path): Path =
    file(repo, fileName)

  /** A workflow file under `.github/workflows`. `name` is the file name, not a path. */
  def file(repo: Path, name: String): Path =
    repo.resolve(".github").resolve("workflows").resolve(name)

  /** Test cells for this event. Pull requests keep changed modules and their dependents. */
  def plan(graph: ProjectGraph, event: CiEvent): Either[CiError, List[TestJob]] =
    val modules = graph.projects.collect { case module: Module => module }
    event match
      case CiEvent.Other                => Right(jobs(graph.splits, CiEvent.Other, modules, modules))
      case CiEvent.PullRequest(changed) =>
        affected(graph, changed).map: names =>
          val chosen = modules.filter(module => names.contains(module.name))
          jobs(graph.splits, event, modules, chosen)

  /** No names is the whole graph. Names are modules, in the order given, without adding dependents. */
  def target(graph: ProjectGraph, raw: List[String]): Either[CiError, TestTarget] =
    if raw.isEmpty then Right(TestTarget.Entire)
    else
      raw
        .foldLeft[Either[CiError, List[ProjectName]]](Right(Nil)): (acc, item) =>
          acc.flatMap: seen =>
            ProjectName.from(item) match
              case Left(_)                            => Left(CiError.BadName(item))
              case Right(name) if seen.contains(name) => Left(CiError.Duplicate(name))
              case Right(name)                        =>
                graph.get(name) match
                  case None               => Left(CiError.Unknown(name))
                  case Some(_: Aggregate) => Left(CiError.NotModule(name))
                  case Some(_: Module)    => Right(seen :+ name)
        .flatMap:
          case first :: rest => Right(TestTarget.Named(first, rest))
          case Nil           => Right(TestTarget.Entire)

  def render(graph: ProjectGraph): Either[CiError, String] =
    prepared(graph).map(_._1)

  /** `ci.yml`, `histos-release.yml` when the catalog names a ship, `histos-promote.yml` when that release's `followUp`
    * is Deployment, `histos-snapshots.yml` when a ship names an asset, then one file per dispatch, cron, or trigger
    * that must not wake `ci.yml`.
    */
  def renderAll(graph: ProjectGraph, sona: Publishing): Either[CiError, List[RenderedWorkflow]] =
    prepared(graph).map: (body, files) =>
      val release = releaseWorkflow(graph, sona).toList
      val promote =
        release match
          case Nil => Nil
          case _   => promoteWorkflow(graph, sona).toList
      val snapshots = snapshotWorkflow(graph).toList
      RenderedWorkflow(fileName, body) :: release ++ promote ++ snapshots ++ SupplyYaml.files(graph) ++ files

  private def prepared(graph: ProjectGraph): Either[CiError, (String, List[RenderedWorkflow])] =
    for
      planned <- JobYaml.render(graph)
      jobs    <- plan(graph, CiEvent.Other)
    yield (document(graph, jobs, planned), planned.files)

  def differs(rendered: String, committed: String): Either[CiError, Unit] =
    if rendered == committed then Right(()) else Left(CiError.Drift)

  def check(repo: Path, graph: ProjectGraph, sona: Publishing): IO[CiError, Path] =
    for
      rendered <- ZIO.fromEither(renderAll(graph, sona))
      _        <- ZIO.foreachDiscard(rendered)(compare(repo, _))
      extra    <- orphans(repo, rendered.map(_.name).toSet)
      _        <- ZIO.when(extra.nonEmpty)(ZIO.fail(CiError.Drift))
      _        <- PinScan.check(repo, graph)
    yield path(repo)

  def update(repo: Path, graph: ProjectGraph, sona: Publishing): IO[CiError, Path] =
    for
      rendered <- ZIO.fromEither(renderAll(graph, sona))
      _        <- ZIO.foreachDiscard(rendered)(item => write(file(repo, item.name), item.body))
      extra    <- orphans(repo, rendered.map(_.name).toSet)
      _        <- ZIO.foreachDiscard(extra): orphan =>
        ZIO
          .attemptBlocking(Files.deleteIfExists(orphan))
          .mapError(_ => CiError.Io(IoFailure(orphan, IoFault.Delete)))
    yield path(repo)

  /** The process boundary. The command is `./histos test` plus the cell, nothing else. */
  def argv(job: TestJob): List[String] =
    val head = List("./histos", "test")
    job match
      case TestJob.Aggregate(TestTarget.Entire)             => head
      case TestJob.Aggregate(TestTarget.Named(first, rest)) =>
        head ++ (first :: rest).map(_.value)
      case TestJob.Split(module, platform, version) =>
        val projects = module match
          case ModuleRef.All       => Nil
          case ModuleRef.One(name) => List(name.value)
        val platforms = platform match
          case PlatformRef.All           => Nil
          case PlatformRef.One(selected) => List("--platform", PlatformToken.render(selected))
        val versions = version match
          case VersionRef.All           => Nil
          case VersionRef.One(selected) => List("--scala", selected.value)
        head ++ projects ++ platforms ++ versions

  private def affected(graph: ProjectGraph, changed: Set[ProjectName]): Either[CiError, List[ProjectName]] =
    val known = graph.projects.map(_.name).toSet
    changed.toList.filter(name => !known.contains(name)).sortBy(_.value) match
      case missing :: _ => Left(CiError.Unknown(missing))
      case Nil          =>
        val modules = graph.projects.collect { case module: Module => module }
        Right(close(modules, changed))

  private def close(modules: List[Module], roots: Set[ProjectName]): List[ProjectName] =
    def step(have: Set[ProjectName]): Set[ProjectName] =
      val extra = modules.collect {
        case module if module.upstreamNames.exists(have.contains) => module.name
      }.toSet
      if extra.subsetOf(have) then have else step(have ++ extra)
    val grown = step(roots)
    modules.collect { case module if grown.contains(module.name) => module.name }

  private def jobs(
      splits: Splits,
      event: CiEvent,
      all: List[Module],
      chosen: List[Module],
  ): List[TestJob] =
    if splits.axes.isEmpty then aggregate(event, all, chosen)
    else if chosen.isEmpty then Nil
    else matrix(splits, chosen)

  private def aggregate(event: CiEvent, all: List[Module], chosen: List[Module]): List[TestJob] =
    event match
      case CiEvent.Other          => List(TestJob.Aggregate(TestTarget.Entire))
      case CiEvent.PullRequest(_) =>
        val names = all.collect { case module if chosen.exists(_.name == module.name) => module.name }
        names match
          case Nil           => Nil
          case first :: rest => List(TestJob.Aggregate(TestTarget.Named(first, rest)))

  private def matrix(splits: Splits, modules: List[Module]): List[TestJob] =
    val splitModule   = splits.axes.contains(Axis.Module)
    val splitPlatform = splits.axes.contains(Axis.Platform)
    val splitVersion  = splits.axes.contains(Axis.ScalaVersion)
    val groups        =
      if splitModule then modules.map(module => (ModuleRef.One(module.name), List(module)))
      else List((ModuleRef.All, modules))
    groups.flatMap: (moduleRef, group) =>
      val platforms =
        if splitPlatform then distinct(group.flatMap(_.platforms.all)).map(PlatformRef.One(_))
        else List(PlatformRef.All)
      val versions =
        if splitVersion then distinct(group.flatMap(_.versions.all)).map(VersionRef.One(_))
        else List(VersionRef.All)
      platforms.flatMap: platform =>
        versions.map: version =>
          TestJob.Split(moduleRef, platform, version)

  private def distinct[A](values: List[A]): List[A] =
    values.foldLeft(List.empty[A]): (seen, value) =>
      if seen.contains(value) then seen else seen :+ value

  private def document(graph: ProjectGraph, jobs: List[TestJob], planned: JobYaml.Rendered): String =
    val modules = graph.projects.collect { case module: Module => module }
    val test    = jobs match
      case Nil => Nil
      case _   => testJob(jobs, graph)
    val versions = graph.catalog match
      case Catalog.Empty => Nil
      case _             => List(versionsJob(graph))
    val doc = graph.docCheck match
      case DocCheck.Off => Nil
      case DocCheck.On  => List(docJob(graph))
    val seed = if modules.nonEmpty then List(cacheSeed(jobs, graph)) else Nil
    val boot = if selfHosted(graph) then List(toolchainJob) else Nil
    val body =
      boot ++ List(affectedJob(graph)) ++ test ++ versions ++ doc ++ planned.ci ++ seed ++
        List(workflowJob(graph)) ++ List(SupplyYaml.supplyChain(graph)) ++ SupplyYaml.snapshot(graph).toList
    (header(planned.header, graph) ++ body).mkString("\n") + "\n"

  /** Dispatch only. No tag, no inputs, no `if`. Secrets on the release step are `secretNames`, in that order. A release
    * ship that has a native client then links that client and uploads the install files to the existing `dist` release.
    */
  private def releaseWorkflow(graph: ProjectGraph, sona: Publishing): Option[RenderedWorkflow] =
    graph.catalog.ships match
      case Nil   => None
      case ships => Some(RenderedWorkflow(releaseName, releaseDocument(graph, ships, sona)))

  /** A ship that names an asset. The workflow publishes snapshots, and on the default branch uploads the dist channel.
    * An empty asset list emits nothing, so update deletes a stale copy.
    */
  private def snapshotWorkflow(graph: ProjectGraph): Option[RenderedWorkflow] =
    if graph.catalog.ships.exists(ship => assetMember(graph, ship)) then
      Some(RenderedWorkflow(snapshotsName, snapshotDocument(graph)))
    else None

  /** `HISTOS_BRANCH` is `default` only on the repository's default branch. That is the session that names the pointer.
    * Another ref stores the commit coordinate and does not upload a channel. The assembly step is the same condition:
    * the channel reads the asset file, and `./histos assembly` writes it.
    */
  private def snapshotDocument(graph: ProjectGraph): String =
    val branch =
      "github.ref == format('refs/heads/{0}', github.event.repository.default_branch) && 'default' || 'other'"
    val lines = List(
      "name: snapshots",
      "on:",
      "  workflow_dispatch:",
      "permissions:",
      "  contents: read",
      "jobs:",
      "  snapshots:",
      s"    runs-on: $Runner",
      "    permissions:",
      "      contents: write",
      "    steps:",
      s"      - uses: $Checkout",
    ) ++ coursierCli(graph, "snapshots", build = true) ++ List(
      s"      - if: ${quote(OnDefaultBranch)}",
      s"        run: ${quote("./histos assembly")}",
      s"      - run: ${quote("./histos publish snapshots")}",
      "        env:",
      "          HISTOS_SESSION: snapshot",
      "          HISTOS_SHA: ${{ github.sha }}",
      s"          HISTOS_BRANCH: $${{ $branch }}",
      "          SONATYPE_USERNAME: ${{ secrets.SONATYPE_USERNAME }}",
      "          SONATYPE_PASSWORD: ${{ secrets.SONATYPE_PASSWORD }}",
      "          GITHUB_TOKEN: ${{ github.token }}",
      "          GITHUB_REPOSITORY: ${{ github.repository }}",
    )
    lines.mkString("\n") + "\n"

  /** Promote follows a release file. An empty catalog emits neither, so update deletes a stale copy. */
  private def promoteWorkflow(graph: ProjectGraph, sona: Publishing): Option[RenderedWorkflow] =
    sona.followUp match
      case FollowUp.Finished   => None
      case FollowUp.Deployment => Some(RenderedWorkflow(promoteName, promoteDocument(graph)))

  private def releaseDocument(graph: ProjectGraph, ships: List[Ship], sona: Publishing): String =
    val install = installShip(graph, ships)
    val lines   = List(
      "name: release",
      "on:",
      "  workflow_dispatch:",
      "permissions:",
      "  contents: read",
      "jobs:",
    ) ++ ships.flatMap(ship =>
      releaseJob(
        graph,
        ship,
        sona,
        if install.exists(_.name == ship.name) then ReleaseShape.WithInstall else ReleaseShape.Publish,
      )
    ) ++ install.toList
      .flatMap(ship => installJobs(graph, ships, ship))
    lines.mkString("\n") + "\n"

  private def releaseJob(graph: ProjectGraph, ship: Ship, sona: Publishing, shape: ReleaseShape): List[String] =
    val name      = ship.name.value
    val published = List(
      s"  release-$name:",
      s"    runs-on: $Runner",
      "    permissions:",
    ) ++ releasePermissions(shape) ++ List(
      "    steps:",
      s"      - uses: $Checkout",
    ) ++ coursierCli(graph, s"release-$name", build = true) ++ List(
      s"      - run: ${quote(s"./histos publish release $name")}"
    ) ++ secretEnv(sona.secretNames)
    shape match
      case ReleaseShape.Publish     => published
      case ReleaseShape.WithInstall => published ++ jarArtifact ++ bootstrapUpload(graph) ++ shimArtifact

  /** `upload-artifact` writes through the Actions API. A permissions block that omits `actions` sets it to none. */
  private def releasePermissions(shape: ReleaseShape): List[String] =
    shape match
      case ReleaseShape.Publish     => List("      contents: read")
      case ReleaseShape.WithInstall =>
        List(
          "      actions: write",
          "      contents: read",
        )

  /** The assembly the install manifest names. It is built after the portal upload, on the warm workspace. */
  private def jarArtifact: List[String] =
    List(
      s"      - run: ${quote("./histos assembly")}",
      s"      - uses: ${ActionPins.uploadArtifact.line}",
      "        with:",
      "          name: histos-jar",
      "          path: dist/histos.jar",
      "          if-no-files-found: error",
    )

  /** One non-snapshot ship that names an asset, and a native client the launcher can exec. */
  private def installShip(graph: ProjectGraph, ships: List[Ship]): Option[Ship] =
    val llvm = graph.runnerTools.llvm
    if !ClientBinary.present(graph) || llvm.isEmpty then None
    else
      ships.filter(ship => releaseVersion(ship) && assetMember(graph, ship)) match
        case ship :: Nil => Some(ship)
        case _           => None

  /** The catalog number is the next release. A snapshot is a session, not a catalog row. */
  private def releaseVersion(ship: Ship): Boolean =
    ship match
      case _: Ship.Line => true

  private def shipVersion(ship: Ship): String =
    ship match
      case Ship.Line(_, number, _, _, _) => number.render

  private def assetMember(graph: ProjectGraph, ship: Ship): Boolean =
    ship.members.toList.exists: name =>
      graph.get(name) match
        case Some(module: Module) => module.asset.isDefined
        case _                    => false

  private def installJobs(graph: ProjectGraph, ships: List[Ship], ship: Ship): List[String] =
    graph.runnerTools.llvm match
      case None         => Nil
      case Some(pinned) =>
        val revision = pinned.value
        val needed   = ships.map(row => s"release-${row.name.value}")
        clientJob(graph, needed, revision) ++ distJob(graph, shipVersion(ship))

  private def clientJob(graph: ProjectGraph, needed: List[String], revision: String): List[String] =
    val hosts = DistPlatforms.flatMap: (platform, runner, archive) =>
      List(
        s"          - runner: $runner",
        s"            platform: $platform",
        s"            archive: LLVM-$revision-$archive.tar.xz",
      )
    List(
      "  client:",
      s"    needs: [${needed.mkString(", ")}]",
      "    runs-on: ${{ matrix.runner }}",
      "    strategy:",
      "      fail-fast: false",
      "      matrix:",
      "        include:",
    ) ++ hosts ++ List(
      "    permissions:",
      "      actions: write",
      "      contents: read",
      "    steps:",
      s"      - uses: $Checkout",
    ) ++ clientSetup ++ List(s"      - uses: $Setup") ++ fetchAssembly(graph) ++ SupplyYaml.preflight(
      graph,
      "client",
    ) ++ clientLlvm(
      revision
    ) ++ List(
      s"      - run: ${quote("./histos client")}",
      s"      - uses: ${ActionPins.uploadArtifact.line}",
      "        with:",
      "          name: client-${{ matrix.platform }}",
      "          path: dist/histos-client",
      "          if-no-files-found: error",
    )

  /** The installer the README curls. A piped `sh` cannot see its own file, so the release publishes this copy. */
  private def shimArtifact: List[String] =
    List(
      s"      - uses: ${ActionPins.uploadArtifact.line}",
      "        with:",
      "          name: shim",
      "          path: install/histos",
      "          if-no-files-found: error",
    )

  /** Temurin 25. The process bootstrap compiles `java.lang.foreign`. JDK 21 still marks that API preview, and that is
    * the JDK image on `macos-26`. The unpacked LLVM clang does not find the Xcode SDK, and the linker child keeps only
    * `PATH`, so the macOS job puts a `clang` wrapper on `PATH` that passes `-isysroot`.
    */
  private val ClientJdk: String = "25"

  private def clientSetup: List[String] =
    List(
      s"      - uses: ${ActionPins.setupJava.line}",
      "        with:",
      "          distribution: temurin",
      s"          java-version: ${quote(ClientJdk)}",
      s"      - uses: $Setup",
    )

  private def clientLlvm(revision: String): List[String] =
    val key = "llvm-" + revision + "-${{ runner.os }}-${{ runner.arch }}"
    val url = "https://github.com/llvm/llvm-project/releases/download/llvmorg-" + revision + "/${{ matrix.archive }}"
    cacheStep(Some("llvm"), List("${{ runner.temp }}/llvm"), key) ++ List(
      "      - run: |",
      "          dir=\"${{ runner.temp }}/llvm\"",
      "          if [ \"${{ steps.llvm.outputs.cache-hit }}\" != \"true\" ]; then",
      "            mkdir -p \"$dir\"",
      "            curl -fsSL \"" + url + "\" -o \"$dir/llvm.tar.xz\"",
      "            tar -xJf \"$dir/llvm.tar.xz\" -C \"$dir\" --strip-components=1",
      "            rm \"$dir/llvm.tar.xz\"",
      "          fi",
      "          test -x \"$dir/bin/clang\"",
      "          echo \"$dir/bin\" >> \"$GITHUB_PATH\"",
      "          if [ \"$(uname -s)\" = Linux ]; then",
      "            if [ -n \"$LD_LIBRARY_PATH\" ]; then",
      "              echo \"LD_LIBRARY_PATH=$dir/lib:$LD_LIBRARY_PATH\" >> \"$GITHUB_ENV\"",
      "            else",
      "              echo \"LD_LIBRARY_PATH=$dir/lib\" >> \"$GITHUB_ENV\"",
      "            fi",
      "          fi",
      "          if [ \"$(uname -s)\" = Darwin ]; then",
      "            sdk=$(xcrun --show-sdk-path)",
      "            wrap=\"${{ runner.temp }}/clang-sdk\"",
      "            mkdir -p \"$wrap\"",
      "            for tool in clang clang++; do",
      "              printf '%s\\n' '#!/bin/sh' \"exec \\\"$dir/bin/$tool\\\" -isysroot \\\"$sdk\\\" \\\"\\$@\\\"\" > \"$wrap/$tool\"",
      "              chmod +x \"$wrap/$tool\"",
      "            done",
      "            echo \"$wrap\" >> \"$GITHUB_PATH\"",
      "          fi",
    )

  private def distJob(graph: ProjectGraph, version: String): List[String] =
    List(
      "  install:",
      "    needs: client",
      s"    runs-on: $Runner",
      "    permissions:",
      "      actions: read",
      "      contents: write",
      "    steps:",
      s"      - uses: $Checkout",
      s"      - uses: $Setup",
    ) ++ fetchAssembly(graph) ++ SupplyYaml.preflight(graph, "install") ++ List(
      s"      - uses: ${ActionPins.downloadArtifact.line}",
      "        with:",
      "          path: dist-in",
      "      - run: |",
      "          set -eu",
      s"          version=$version",
      "          jar=dist-in/histos-jar/histos.jar",
      "          linux=dist-in/client-linux-x86_64/histos-client",
      "          mac=dist-in/client-macos-aarch64/histos-client",
      "          sha() { sha256sum \"$1\" | awk '{print $1}'; }",
      "          jar_sha=$(sha \"$jar\")",
      "          linux_sha=$(sha \"$linux\")",
      "          mac_sha=$(sha \"$mac\")",
      "          cp \"$jar\" \"$version-histos.jar\"",
      "          cp \"$linux\" \"$version-client-linux-x86_64\"",
      "          cp \"$mac\" \"$version-client-macos-aarch64\"",
      "          shim_file=dist-in/shim/histos",
      "          shim_sha=$(sha \"$shim_file\")",
      "          cp \"$shim_file\" shim",
      "          printf '%s\\n' \"$shim_sha\" > shim.sha256",
      "          cat > \"$version-manifest.json\" <<EOF",
      "          {",
      "            \"version\": \"$version\",",
      "            \"jar\": \"$version-histos.jar\",",
      "            \"jarSha256\": \"$jar_sha\",",
      "            \"clients\": {",
      "              \"linux-x86_64\": \"$linux_sha\",",
      "              \"macos-aarch64\": \"$mac_sha\"",
      "            }",
      "          }",
      "          EOF",
      "          gh release upload dist \"$version-manifest.json\" \"$version-histos.jar\" \"$version-client-linux-x86_64\" \"$version-client-macos-aarch64\" shim shim.sha256 --clobber --repo \"$GITHUB_REPOSITORY\"",
      "        env:",
      "          GH_TOKEN: ${{ github.token }}",
    )

  /** Install platforms. The archive token is the LLVM release name for that runner. */
  private val DistPlatforms: List[(String, String, String)] = List(
    ("linux-x86_64", "ubuntu-26.04", "Linux-X64"),
    ("macos-aarch64", "macos-26", "macOS-ARM64"),
  )

  /** Promote does not sign, so this file does not list `secretNames`. */
  private val promoteSecrets: List[String] =
    List("SONATYPE_USERNAME", "SONATYPE_PASSWORD")

  private def promoteDocument(graph: ProjectGraph): String =
    val lines = List(
      "name: promote",
      "on:",
      "  workflow_dispatch:",
      "    inputs:",
      "      deployment:",
      "        description: deployment id",
      "        required: true",
      "        type: string",
      "permissions:",
      "  contents: read",
      "jobs:",
      "  promote:",
      s"    runs-on: $Runner",
      "    steps:",
      s"      - uses: $Checkout",
    ) ++ coursierCli(graph, "promote") ++ List(
      "      - run: \"./histos publish promote ${{ inputs.deployment }}\""
    ) ++ secretEnv(promoteSecrets)
    lines.mkString("\n") + "\n"

  private def secretEnv(names: List[String]): List[String] =
    names match
      case Nil => Nil
      case _   => "        env:" :: names.map(secret)

  private def secret(name: String): String =
    s"          $name: $${{ secrets.$name }}"

  /** A matrix step runs for every cell, or only for the cells that name the tool. */
  private enum MatrixScope:
    case Whole
    case Cell

  /** The native client is linked for the whole job, or only on the matrix cells that need it. */
  private enum ClientLink:
    case Skip
    case Run(scope: MatrixScope)

  /** Matrix rows are a name and a command, or those plus the tool flags. */
  private enum ToolColumns:
    case Off
    case On

  /** A release job publishes, or it also uploads the install artifacts. */
  private enum ReleaseShape:
    case Publish
    case WithInstall

  /** On a push, `github.ref` is `refs/heads/<branch>` and `repository.default_branch` is the short name. */
  private val OnDefaultBranch: String =
    "github.ref == format('refs/heads/{0}', github.event.repository.default_branch)"

  private val OnDefaultPush: String =
    s"github.event_name == 'push' && $OnDefaultBranch"

  /** Verify runs on a pull request or a dispatch. A push does not run the suite. */
  private val OnVerify: String =
    "github.event_name == 'pull_request' || github.event_name == 'workflow_dispatch'"

  private val NoneFailed: String =
    "!cancelled() && !contains(needs.*.result, 'failure')"

  private def verifyIf: List[String] =
    List(s"    if: ${quote(OnVerify)}")

  private def verifyNeeds: List[String] =
    List(s"    if: ${quote(s"($OnVerify) && ($NoneFailed)")}")

  /** `on.push.branches` is a static glob. The github context is not available there. `**` is every branch. Verify jobs
    * repeat [[OnVerify]], so a push starts a run whose suite skips. `workflow_dispatch` is on this file because verify
    * accepts it. A label, a path, a cron, or `workflow_run` is not: those are their own files.
    */
  private def header(extra: CiHeader, graph: ProjectGraph): List[String] =
    val tags =
      if extra.tags.isEmpty then Nil
      else "    tags:" :: extra.tags.map(tag => s"      - ${quote(tag)}")
    val merge   = if extra.extras.contains(CiExtra.MergeGroup) then List("  merge_group:") else Nil
    val release = if extra.extras.contains(CiExtra.Release) then List("  release:") else Nil
    List(
      "name: ci",
      "on:",
      "  pull_request:",
      "  push:",
      "    branches:",
      s"      - ${quote("**")}",
    ) ++ tags ++ dispatchInputs(extra.inputs) ++ merge ++ release ++ List(
      "permissions:",
      "  contents: read",
    ) ++ workflowEnv(graph) ++ List(
      "jobs:"
    )

  private def workflowEnv(graph: ProjectGraph): List[String] =
    val plain = graph.ci.env.filter {
      case (_, histos.plugin.EnvValue.Secret(_)) => false
      case _                                     => true
    }
    plain match
      case Nil => Nil
      case _   =>
        "env:" :: plain.map:
          case (name, value) => s"  ${name.value}: ${workflowValue(value)}"

  private def workflowValue(value: histos.plugin.EnvValue): String =
    value match
      case histos.plugin.EnvValue.Plain(text)   => quote(text)
      case histos.plugin.EnvValue.Secret(name)  => s"$${{ secrets.${name.value} }}"
      case histos.plugin.EnvValue.Github(field) =>
        field match
          case histos.plugin.GithubField.Sha     => "${{ github.sha }}"
          case histos.plugin.GithubField.RefName => "${{ github.ref_name }}"
          case histos.plugin.GithubField.Token   => "${{ github.token }}"
      case histos.plugin.EnvValue.Env(name) => s"$${{ env.${name.value} }}"

  private def dispatchInputs(inputs: List[histos.plugin.DispatchInput]): List[String] =
    inputs match
      case Nil => List("  workflow_dispatch:")
      case _   =>
        "  workflow_dispatch:" :: "    inputs:" :: inputs.flatMap: input =>
          val kind = input.kind match
            case histos.plugin.InputKind.Text    => "string"
            case histos.plugin.InputKind.Boolean => "boolean"
            case histos.plugin.InputKind.Choice  => "choice"
          List(
            s"      ${input.name.value}:",
            s"        description: ${input.name.value}",
            "        required: false",
            s"        type: $kind",
          ) ++ input.default.toList.map(value => s"        default: ${quote(value)}")

  /** The test job, and the doc check when a build asks for it. The doc job restores the store because it compiles, and
    * it does not save: the suite is the writer. It does not wait for the suite. Versions, plugin docs, deploy,
    * workflow, and release do not restore these. This workflow does not fetch a JDK, so it emits no JDK key. Node is
    * installed for a JavaScript module, or when Playwright is installed. `setup-node` does not set `cache: npm`: that
    * key is a package lock, and Chekhov does not read it. LLVM is installed for a native module. The runner is
    * `ubuntu-26.04`, so a miss downloads `LLVM-<revision>-Linux-X64.tar.xz` from the llvm-project release and unpacks
    * it. A hit does not. The step runs either way: it checks `clang` and puts `bin` on `PATH`. Chekhov reads Playwright
    * from `~/.cache/chekhov`. An unset `PLAYWRIGHT_BROWSERS_PATH` is `~/.cache/ms-playwright` on this runner. A
    * Playwright revision installs the browser even when no module is Firefox, because a JVM suite can launch it.
    * `npm install` and `install firefox` run when that cache misses. The step still runs on a hit, checks the package,
    * and runs `install-deps firefox`: it is apt, and a runner does not keep those packages. A step `if` that skips the
    * download leaves a job that did not run. Coursier is the Linux default of `Resolve.coursier` when `COURSIER_CACHE`
    * is unset. `hashFiles` names the build file while the build resolves, and the pin while it is locked. No step sets
    * `restore-keys`. Every job that runs `./histos` uses the setup action and does not install a JDK or the `cs`
    * binary.
    */
  private def steps(
      tools: RunnerTools,
      resolution: Resolution,
      need: ToolNeed,
      scope: MatrixScope,
  ): List[String] =
    nodeStep(if need.node then tools.node else None, scope) ++
      llvmStep(if need.llvm then tools.llvm else None, scope) ++
      playwrightStep(if need.playwright then tools.playwright else None, scope) ++
      coursierStep(resolution)

  private def cellFlag(scope: MatrixScope, flag: String): Option[String] =
    scope match
      case MatrixScope.Whole => None
      case MatrixScope.Cell  => Some(s"matrix.$flag == 'true'")

  /** Current major. v4 still targets Node 20. */
  private val ToolCache = ActionPins.cache.line

  private val CoursierCache = "~/.cache/coursier/v1"

  private val PlaywrightBrowsers = "~/.cache/ms-playwright"

  private def coursierKey(resolution: Resolution): String =
    val named = resolution match
      case Resolution.Solve => "histos.scala"
      case Resolution.Lock  => "dependencies.lock"
    s"coursier-$${{ hashFiles('$named') }}-$${{ runner.os }}-$${{ runner.arch }}"

  private def toolKey(name: String, revision: String): String =
    s"$name-$revision-$${{ runner.os }}-$${{ runner.arch }}"

  private def cacheStep(
      id: Option[String],
      paths: List[String],
      key: String,
      condition: Option[String] = None,
  ): List[String] =
    paths match
      case Nil => Nil
      case _   =>
        val ident = id match
          case None       => Nil
          case Some(name) => List(s"        id: $name")
        val guard = condition.toList.map(text => s"        if: $text")
        List(s"      - uses: $ToolCache") ++ ident ++ guard ++ List("        with:") ++ cachePaths(paths) ++ List(
          s"          key: ${quote(key)}"
        )

  private def cachePaths(paths: List[String]): List[String] =
    paths match
      case path :: Nil => List(s"          path: ${quote(path)}")
      case many        => "          path: |" :: many.map(path => s"            $path")

  private def coursierStep(resolution: Resolution): List[String] =
    cacheStep(None, List(CoursierCache), coursierKey(resolution))

  /** The composite action in this repo. It installs the shim and restores the caches. It does not install a JDK. */
  private val Setup = "./.github/actions/setup"

  /** This checkout's scm. Other builds do not assemble Histos in their own CI. */
  private val OwnScm = "https://github.com/early-effect/histos"

  private[workflow] def selfHosted(graph: ProjectGraph): Boolean =
    graph.facts.scm.exists(_.url.value == OwnScm)

  /** One job builds the jar. The rest download it. A version line that already lists `doctor` skips the build. */
  private def coursierCli(graph: ProjectGraph, job: String, build: Boolean = false): List[String] =
    val extra = if build then buildAssembly(graph) else fetchAssembly(graph)
    List(s"      - uses: $Setup") ++ extra ++ SupplyYaml.preflight(graph, job)

  private[workflow] def toolchainNeeds(graph: ProjectGraph): List[String] =
    if selfHosted(graph) then List("    needs: toolchain") else Nil

  private def needsAffected(graph: ProjectGraph): List[String] =
    if selfHosted(graph) then List("    needs: [affected, toolchain]") else List("    needs: affected")

  /** Download uses the Actions API. A permissions block that omits `actions` sets it to none. */
  private[workflow] def actionsRead(graph: ProjectGraph): List[String] =
    if selfHosted(graph) then List("      actions: read") else Nil

  private[workflow] def fetchAssembly(graph: ProjectGraph): List[String] =
    if !selfHosted(graph) then Nil
    else
      List(
        s"      - uses: ${ActionPins.downloadArtifact.line}",
        "        continue-on-error: true",
        "        with:",
        "          name: histos-bootstrap",
        "          path: histos-bootstrap",
        "      - run: |",
        "          if [ -f histos-bootstrap/dist/histos.jar ]; then",
        "            jar=histos-bootstrap/dist/histos.jar",
        "            sum=histos-bootstrap/dist/assembly.sha256",
        "          else",
        "            jar=histos-bootstrap/histos.jar",
        "            sum=histos-bootstrap/assembly.sha256",
        "          fi",
        "          if [ -f \"$jar\" ] && [ -f \"$sum\" ]; then",
        "            mkdir -p dist .histos",
        "            mv \"$jar\" dist/histos.jar",
        "            mv \"$sum\" .histos/assembly.sha256",
        "          fi",
      )

  /** The published jar is the one the version line names. Build this tree only when that jar has no `doctor`. */
  private[workflow] def buildAssembly(graph: ProjectGraph): List[String] =
    if !selfHosted(graph) then Nil
    else
      List(
        "      - run: |",
        "          usage=$(./histos 2>&1 || true)",
        "          case \"$usage\" in",
        "            *\" doctor \"*) ;;",
        "            *) ./histos assembly ;;",
        "          esac",
      )

  /** The assembly `ci.yml` shares. The release file uploads the same name after it has built the install jar. */
  private def bootstrapUpload(graph: ProjectGraph): List[String] =
    if !selfHosted(graph) then Nil
    else
      List(
        "      - run: cp .histos/assembly.sha256 dist/assembly.sha256",
        s"      - uses: ${ActionPins.uploadArtifact.line}",
        "        with:",
        "          name: histos-bootstrap",
        "          if-no-files-found: error",
        "          path: |",
        "            dist/histos.jar",
        "            dist/assembly.sha256",
      )

  private def toolchainJob: String =
    (List(
      "  toolchain:",
      s"    runs-on: $Runner",
      "    permissions:",
      "      actions: write",
      "      contents: read",
      "    steps:",
      s"      - uses: $Checkout",
      s"      - uses: $Setup",
      "      - id: jar",
      "        run: |",
      "          usage=$(./histos 2>&1 || true)",
      "          case \"$usage\" in",
      "            *\" doctor \"*)",
      "              echo \"built=false\" >> \"$GITHUB_OUTPUT\"",
      "              ;;",
      "            *)",
      "              ./histos assembly",
      "              cp .histos/assembly.sha256 dist/assembly.sha256",
      "              echo \"built=true\" >> \"$GITHUB_OUTPUT\"",
      "              ;;",
      "          esac",
      s"      - uses: ${ActionPins.uploadArtifact.line}",
      "        if: steps.jar.outputs.built == 'true'",
      "        with:",
      "          name: histos-bootstrap",
      "          if-no-files-found: error",
      "          path: |",
      "            dist/histos.jar",
      "            dist/assembly.sha256",
    )).mkString("\n")

  private def nodeStep(floor: Option[NodeFloor], scope: MatrixScope): List[String] =
    floor match
      case None        => Nil
      case Some(major) =>
        val guard = cellFlag(scope, "node").toList.map(text => s"        if: $text")
        List(s"      - uses: ${ActionPins.setupNode.line}") ++ guard ++ List(
          "        with:",
          s"          node-version: ${quote(major.value.toString)}",
        )

  private def llvmStep(revision: Option[ToolRevision], scope: MatrixScope): List[String] =
    revision match
      case None         => Nil
      case Some(pinned) =>
        val directory = "${{ runner.temp }}/llvm"
        val version   = pinned.value
        val archive   = s"LLVM-$version-Linux-X64.tar.xz"
        val url       = s"https://github.com/llvm/llvm-project/releases/download/llvmorg-$version/$archive"
        val cell      = cellFlag(scope, "llvm")
        cacheStep(Some("llvm"), List(directory), toolKey("llvm", version), cell) ++ List(
          "      - run: |",
          s"          dir=${quote(directory)}",
          "          if [ \"${{ steps.llvm.outputs.cache-hit }}\" != \"true\" ]; then",
          "            mkdir -p \"$dir\"",
          s"            curl -fsSL ${quote(url)} -o \"$$dir/llvm.tar.xz\"",
          "            tar -xJf \"$dir/llvm.tar.xz\" -C \"$dir\" --strip-components=1",
          "            rm \"$dir/llvm.tar.xz\"",
          "          fi",
          "          test -x \"$dir/bin/clang\"",
          "          echo \"$dir/bin\" >> \"$GITHUB_PATH\"",
          "          if [ -n \"$LD_LIBRARY_PATH\" ]; then",
          s"""            echo "LD_LIBRARY_PATH=$$dir/lib:$$LD_LIBRARY_PATH" >> "$$GITHUB_ENV"""",
          "          else",
          "            echo \"LD_LIBRARY_PATH=$dir/lib\" >> \"$GITHUB_ENV\"",
          "          fi",
        ) ++ cell.toList.map(text => s"        if: $text")

  private def playwrightStep(revision: Option[ToolRevision], scope: MatrixScope): List[String] =
    revision match
      case None         => Nil
      case Some(pinned) =>
        val version = pinned.value
        val cell    = cellFlag(scope, "playwright")
        cacheStep(
          Some("playwright"),
          List(s"~/.cache/chekhov/playwright/$version", PlaywrightBrowsers),
          toolKey("playwright", version),
          cell,
        ) ++ List(
          "      - run: |",
          s"          dir=\"$$HOME/.cache/chekhov/playwright/$version\"",
          "          if [ \"${{ steps.playwright.outputs.cache-hit }}\" != \"true\" ]; then",
          "            mkdir -p \"$dir\"",
          s"            npm install --prefix \"$$dir\" --no-fund --no-audit --no-progress playwright@$version",
          "            node \"$dir/node_modules/playwright/cli.js\" install firefox",
          "          fi",
          "          test -d \"$dir/node_modules/playwright\"",
          "          node \"$dir/node_modules/playwright/cli.js\" install-deps firefox",
        ) ++ cell.toList.map(text => s"        if: $text")

  private def affectedJob(graph: ProjectGraph): String =
    val script =
      """          base=all
          |          if [ "$GITHUB_EVENT_NAME" = "pull_request" ]; then
          |            base="$PR_BASE"
          |          elif [ "$GITHUB_EVENT_NAME" = "push" ]; then
          |            last=$(gh api "/repos/${GITHUB_REPOSITORY}/actions/workflows/ci.yml/runs?event=push&status=success&branch=${GITHUB_REF_NAME}&per_page=1" --jq '.workflow_runs[0].head_sha // empty' || true)
          |            if [ -n "$last" ] && git merge-base --is-ancestor "$last" HEAD; then
          |              base="$last"
          |            fi
          |          fi
          |          modules=$(./histos affected --base "$base" || echo '["all"]')
          |          echo "modules=$modules" >> "$GITHUB_OUTPUT"""".stripMargin
    (List(
      "  affected:"
    ) ++ toolchainNeeds(graph) ++ List(
      s"    runs-on: $Runner",
      "    permissions:",
      "      actions: read",
      "      contents: read",
      "    outputs:",
      "      modules: ${{ steps.affected.outputs.modules }}",
      "    steps:",
      s"      - uses: $Checkout",
      "        with:",
      "          fetch-depth: 0",
      s"      - uses: $Setup",
    ) ++ fetchAssembly(graph) ++ SupplyYaml.preflight(graph, "affected") ++ List(
      "      - id: affected",
      "        env:",
      "          GH_TOKEN: ${{ github.token }}",
      "          PR_BASE: ${{ github.event.pull_request.base.sha }}",
      "        run: |",
      script,
    )).mkString("\n")

  /** A push to the default branch compiles and saves the store. The suite does not run on that push. */
  private def cacheSeed(jobs: List[TestJob], graph: ProjectGraph): String =
    val modules = graph.projects.collect { case module: Module => module }
    val needs   = jobs.map(job => toolNeed(modulesOf(job, modules), graph.runnerTools))
    val any     = needs.foldLeft(ToolNeed(false, false, false))(_ || _)
    val prepare = steps(graph.runnerTools, graph.resolution, any, MatrixScope.Whole)
    (List(
      "  cache-seed:"
    ) ++ toolchainNeeds(graph) ++ List(
      s"    if: ${quote(OnDefaultPush)}",
      s"    runs-on: $Runner",
      "    permissions:",
      "      actions: write",
      "      contents: read",
      "    steps:",
      s"      - uses: $Checkout",
    ) ++ coursierCli(graph, "cache-seed") ++ prepare ++ storeRestore ++ List(
      "      - run: \"./histos compile\""
    ) ++ storeSave).mkString("\n")

  private def testJob(jobs: List[TestJob], graph: ProjectGraph): List[String] =
    val modules = graph.projects.collect { case module: Module => module }
    val needs   = jobs.map(job => toolNeed(modulesOf(job, modules), graph.runnerTools))
    val any     = needs.foldLeft(ToolNeed(false, false, false))(_ || _)
    val matrix  = jobs.exists {
      case TestJob.Split(_, _, _) => true
      case _                      => false
    }
    val client  = jobs.exists(job => linksClient(modulesOf(job, modules)))
    val scope   = if matrix then MatrixScope.Cell else MatrixScope.Whole
    val prepare = steps(graph.runnerTools, graph.resolution, any, scope)
    val link    = clientStep(if client then ClientLink.Run(scope) else ClientLink.Skip)
    val columns = if matrix && (any.any || client) then ToolColumns.On else ToolColumns.Off
    jobs match
      case (aggregate @ TestJob.Aggregate(_)) :: Nil =>
        List(
          "  test:"
        ) ++ needsAffected(graph) ++ verifyNeeds ++ List(
          s"    runs-on: $Runner",
          "    permissions:",
          "      actions: write",
          "      contents: read",
          "    steps:",
          s"      - uses: $Checkout",
        ) ++ coursierCli(graph, "test") ++ prepare ++ storeRestore ++ link ++ testRun(
          argv(aggregate).mkString(" ")
        ) ++ storeSave
      case _ =>
        List(
          "  test:"
        ) ++ needsAffected(graph) ++ verifyNeeds ++ List(
          s"    runs-on: $Runner",
          "    permissions:",
        ) ++ actionsRead(graph) ++ List(
          "      contents: read",
          "    strategy:",
          "      fail-fast: false",
          "      matrix:",
          "        include:",
        ) ++ jobs.flatMap(job => include(job, modules, graph.runnerTools, columns)) ++ List(
          "    steps:",
          s"      - uses: $Checkout",
        ) ++ coursierCli(graph, "test") ++ prepare ++ link ++ testRun("${{ matrix.command }}")

  /** Link `dist/histos-client` before the suite, so the launcher execs that binary. LLVM is already on `PATH`. */
  private def clientStep(link: ClientLink): List[String] =
    link match
      case ClientLink.Skip       => Nil
      case ClientLink.Run(scope) =>
        val guard = cellFlag(scope, "client").toList.map(text => s"        if: $text")
        List(s"      - run: ${quote("./histos client")}") ++ guard

  private def include(job: TestJob, modules: List[Module], tools: RunnerTools, columns: ToolColumns): List[String] =
    val rows = List(
      s"          - name: ${quote(slug(job))}",
      s"            command: ${quote(argv(job).mkString(" "))}",
    )
    columns match
      case ToolColumns.Off => rows
      case ToolColumns.On  =>
        val need   = toolNeed(modulesOf(job, modules), tools)
        val chosen = modulesOf(job, modules)
        rows ++ List(
          s"            node: ${quote(need.node.toString)}",
          s"            llvm: ${quote(need.llvm.toString)}",
          s"            playwright: ${quote(need.playwright.toString)}",
          s"            client: ${quote(linksClient(chosen).toString)}",
        )

  private final case class ToolNeed(node: Boolean, llvm: Boolean, playwright: Boolean):
    def ||(other: ToolNeed): ToolNeed =
      ToolNeed(node || other.node, llvm || other.llvm, playwright || other.playwright)

    def any: Boolean = node || llvm || playwright
  end ToolNeed

  private def toolNeed(modules: List[Module], tools: RunnerTools): ToolNeed =
    val native     = modules.exists(_.platforms.all.contains(Platform.Native))
    val javascript = modules.exists(_.platforms.all.contains(Platform.Js))
    val browser    = tools.playwright.isDefined
    ToolNeed(
      tools.node.isDefined && (javascript || browser),
      tools.llvm.isDefined && native,
      browser,
    )

  private def modulesOf(job: TestJob, modules: List[Module]): List[Module] =
    job match
      case TestJob.Aggregate(TestTarget.Entire)             => modules
      case TestJob.Aggregate(TestTarget.Named(first, rest)) =>
        val names = first :: rest
        modules.filter(module => names.contains(module.name))
      case TestJob.Split(module, platform, _) =>
        val named = module match
          case ModuleRef.All       => modules
          case ModuleRef.One(name) => modules.filter(_.name == name)
        platform match
          case PlatformRef.All           => named
          case PlatformRef.One(selected) => named.filter(_.platforms.all.contains(selected))

  private def linksClient(modules: List[Module]): Boolean =
    modules.exists: module =>
      module.name.value == ClientBinary.moduleName &&
        module.platforms.all.contains(Platform.Native) &&
        module.application.nonEmpty

  private def slug(job: TestJob): String =
    job match
      case TestJob.Aggregate(_)                     => "all"
      case TestJob.Split(module, platform, version) =>
        val parts = List(modulePart(module), platformPart(platform), versionPart(version)).flatten
        if parts.isEmpty then "all" else parts.mkString("-")

  private def modulePart(module: ModuleRef): Option[String] =
    module match
      case ModuleRef.All       => None
      case ModuleRef.One(name) => Some(name.value)

  private def platformPart(platform: PlatformRef): Option[String] =
    platform match
      case PlatformRef.All           => None
      case PlatformRef.One(selected) => Some(PlatformToken.render(selected))

  private def versionPart(version: VersionRef): Option[String] =
    version match
      case VersionRef.All           => None
      case VersionRef.One(selected) => Some(selected.value)

  /** Restore, then the suite, then save. The suite writes new products into the restored store. A taken key is deleted
    * and saved again, because GitHub will not replace one. The delete step always runs. It calls the API only when the
    * restore hit, because a miss has no key to replace. A matrix is more than one writer, so it does not open this
    * scope. The doc check restores and does not save. Versions, plugin docs, deploy, workflow, and release do not
    * either.
    */
  private val StorePath = "~/.cache/histos"

  private val StoreRestore = s"actions/cache/restore@${ActionPins.cache.pin.sha.value} # ${ActionPins.cache.pin.tag}"

  private val StoreSave = s"actions/cache/save@${ActionPins.cache.pin.sha.value} # ${ActionPins.cache.pin.tag}"

  private def storeKey: String =
    val namespace =
      "${{ case(github.event_name == 'pull_request', format('pr-{0}', github.event.pull_request.number), 'main') }}"
    s"histos-store-$namespace-$${{ runner.os }}-$${{ runner.arch }}"

  private def storeRestore: List[String] =
    List(
      s"      - uses: $StoreRestore",
      "        id: store",
      "        with:",
      s"          path: ${quote(StorePath)}",
      s"          key: ${quote(storeKey)}",
    )

  private def storeSave: List[String] =
    List(
      "      - if: \"always()\"",
      "        env:",
      "          GH_TOKEN: ${{ github.token }}",
      s"          STORE_KEY: ${quote(storeKey)}",
      "        run: |",
      "          if [ \"${{ steps.store.outputs.cache-hit }}\" = \"true\" ]; then",
      "            gh api --method DELETE /repos/${GITHUB_REPOSITORY}/actions/caches?key=${STORE_KEY}",
      "          fi",
      "      - if: \"always()\"",
      s"        uses: $StoreSave",
      "        with:",
      s"          path: ${quote(StorePath)}",
      s"          key: ${quote(storeKey)}",
    )

  /** The test step only. A pull request is `pr/<n>`. Any other run of this file is a push to the default branch, and
    * that event is the `main` cache namespace: `CacheNamespace.Main`, not the git branch name. Docs, deploy, versions,
    * workflow, and release do not set it. None of them `needs` a cache job or restores a remote disk. This render does
    * not emit that job: the remote URI is configuration.
    */
  private val cacheNamespace: String =
    "${{ case(github.event_name == 'pull_request', format('pr/{0}', github.event.pull_request.number), 'main') }}"

  private def testRun(command: String): List[String] =
    val narrowed = s"$command --affected '$${{ needs.affected.outputs.modules }}'"
    List(
      s"      - run: ${quote(narrowed)}",
      "        env:",
      s"          ${CacheNamespace.Key}: ${quote(cacheNamespace)}",
    )

  /** Pinned image. `ubuntu-latest` moves, and this render should not move with it. */
  private val Runner = "ubuntu-26.04"

  /** Current major. v4 still targets Node 20, which the runner rewrites to Node 24. */
  private val Checkout = ActionPins.checkout.line

  private[workflow] def runner: String = Runner

  private[workflow] def onDefaultPush: String = OnDefaultPush

  /** Beside the suite. It does not wait for the suite, so a scaladoc warning does not hold the tests. This repository
    * also waits for the shared assembly.
    */
  private def docJob(graph: ProjectGraph): String =
    val modules   = graph.projects.collect { case module: Module => module }
    val published = graph.catalog.ships.flatMap(_.members.toList)
    val chosen    = modules.filter(module => published.contains(module.name))
    val prepare   = steps(graph.runnerTools, graph.resolution, docNeed(chosen, graph.runnerTools), MatrixScope.Whole)
    val lines     = List(
      "  doc:"
    ) ++ toolchainNeeds(graph) ++ verifyIf ++ List(
      s"    runs-on: $Runner",
      "    permissions:",
    ) ++ actionsRead(graph) ++ List(
      "      contents: read",
      "    steps:",
      s"      - uses: $Checkout",
    ) ++ coursierCli(graph, "doc") ++ prepare ++ storeRestore ++ List(
      s"      - run: ${quote("./histos run doc")}"
    )
    lines.mkString("\n")

  private def docNeed(modules: List[Module], tools: RunnerTools): ToolNeed =
    val native     = modules.exists(_.platforms.all.contains(Platform.Native))
    val javascript = modules.exists(_.platforms.all.contains(Platform.Js))
    ToolNeed(tools.node.isDefined && javascript, tools.llvm.isDefined && native, false)

  /** The required check. It runs on a pull request, reuses this repository's assembly, and runs the full doctor. */
  private def workflowJob(graph: ProjectGraph): String =
    (List(
      "  workflow:"
    ) ++ toolchainNeeds(graph) ++ verifyIf ++ List(
      s"    runs-on: $Runner",
      "    permissions:",
    ) ++ actionsRead(graph) ++ List(
      "      contents: read",
      "    steps:",
      s"      - uses: $Checkout",
      s"      - uses: $Setup",
    ) ++ fetchAssembly(graph) ++ SupplyYaml.preflight(graph, "workflow") ++ List(
      "      - run: ./histos doctor",
      "      - run: \"./histos workflow --check\"",
    )).mkString("\n")

  private def versionsJob(graph: ProjectGraph): String =
    (List(
      "  versions:"
    ) ++ toolchainNeeds(graph) ++ verifyIf ++ List(
      s"    runs-on: $Runner",
      "    permissions:",
    ) ++ actionsRead(graph) ++ List(
      "      contents: read",
      "    steps:",
      s"      - uses: $Checkout",
      s"      - uses: $Setup",
    ) ++ fetchAssembly(graph) ++ SupplyYaml.preflight(graph, "versions") ++ List(
      "      - run: \"./histos run versions\""
    )).mkString("\n")

  private def quote(text: String): String =
    "\"" + text.replace("\\", "\\\\").replace("\"", "\\\"") + "\""

  private def compare(repo: Path, rendered: RenderedWorkflow): IO[CiError, Unit] =
    read(file(repo, rendered.name)).foldZIO(
      {
        case CiError.Missing(_) if rendered.name != fileName => ZIO.fail(CiError.Drift)
        case other                                           => ZIO.fail(other)
      },
      committed => ZIO.fromEither(differs(rendered.body, committed)),
    )

  /** `histos-*.yml` files this graph did not emit. Other workflow names stay. */
  private def orphans(repo: Path, emitted: Set[String]): IO[CiError, List[Path]] =
    val dir = repo.resolve(".github").resolve("workflows")
    ZIO
      .attemptBlocking {
        if Files.isDirectory(dir) then histosFiles(dir).filter(path => !emitted.contains(fileNameOf(path)))
        else Nil
      }
      .mapError(_ => CiError.Io(IoFailure(dir, IoFault.Walk)))

  private def histosFiles(dir: Path): List[Path] =
    val stream = Files.list(dir)
    val found  =
      try stream.iterator.asScala.toList
      finally stream.close()
    found.filter: path =>
      val name = fileNameOf(path)
      name.startsWith("histos-") && name.endsWith(".yml")

  private def fileNameOf(path: Path): String =
    Option(path.getFileName).map(_.toString).getOrElse("")

  private def read(file: Path): IO[CiError, String] =
    ZIO
      .attemptBlocking {
        if Files.isRegularFile(file) then Right(Files.readString(file)) else Left(CiError.Missing(file))
      }
      .mapError(_ => CiError.Io(IoFailure(file, IoFault.Read)))
      .flatMap(ZIO.fromEither)

  private def write(file: Path, text: String): IO[CiError, Unit] =
    Option(file.getParent) match
      case None         => ZIO.fail(CiError.Io(IoFailure(file, IoFault.Write)))
      case Some(parent) =>
        ZIO
          .attemptBlocking(replace(parent, file, text))
          .mapError(_ => CiError.Io(IoFailure(file, IoFault.Write)))

  private def replace(parent: Path, file: Path, text: String): Unit =
    Files.createDirectories(parent)
    val name = Option(file.getFileName).map(_.toString).getOrElse(fileName)
    val temp = parent.resolve(s"$name.tmp")
    Files.writeString(temp, text)
    try Files.move(temp, file, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING)
    catch
      case _: AtomicMoveNotSupportedException =>
        Files.move(temp, file, StandardCopyOption.REPLACE_EXISTING)
end Workflow
