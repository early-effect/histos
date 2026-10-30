package histos.resolve

import histos.{Dep, DepKind, JsVersion, NativeVersion, ScalaVersion, JsEnv, Platform}

enum ToolRole:
  case Compiler, Semanticdb, JsLibraries, JsLinker, JsEnvironment, JsSuite,
    JsTestBridge, NativeLibraries, NativeLinker, NativeSuite, NativeTestInterface
  def token: String = productPrefix.toLowerCase(java.util.Locale.ROOT)

object ToolRole:
  def from(raw: String): Option[ToolRole] = ToolRole.values.find(_.token == raw)

enum ToolVariant:
  case Jvm
  case Js(revision: JsVersion)
  case Suite(revision: JsVersion, env: JsEnv)
  case Node, Jsdom, Firefox
  case Native(revision: NativeVersion)
  def token: String = this match
    case Jvm                  => "jvm"
    case Js(revision)         => s"js:${revision.value}"
    case Suite(revision, env) =>
      s"suite:${revision.value}:${env.toString.toLowerCase(java.util.Locale.ROOT)}"
    case Node             => "node"
    case Jsdom            => "jsdom"
    case Firefox          => "firefox"
    case Native(revision) => s"native:${revision.value}"

object ToolVariant:
  def from(raw: String): Option[ToolVariant] = raw match
    case "jvm"                               => Some(ToolVariant.Jvm)
    case "node"                              => Some(ToolVariant.Node)
    case "jsdom"                             => Some(ToolVariant.Jsdom)
    case "firefox"                           => Some(ToolVariant.Firefox)
    case value if value.startsWith("suite:") =>
      value.split(":").toList match
        case _ :: revision :: env :: Nil =>
          for
            version     <- JsVersion.from(revision).toOption
            environment <- JsEnv.values.find(_.toString.equalsIgnoreCase(env))
          yield ToolVariant.Suite(version, environment)
        case _ => None
    case value if value.startsWith("js:") =>
      JsVersion.from(value.drop(3)).toOption.map(ToolVariant.Js(_))
    case value if value.startsWith("native:") =>
      NativeVersion.from(value.drop(7)).toOption.map(ToolVariant.Native(_))
    case _ => None

final case class ToolId(
    role: ToolRole,
    scalaVersion: ScalaVersion,
    variant: ToolVariant,
):
  def token: String = s"${role.token} ${scalaVersion.value} ${variant.token}"

enum Transitivity:
  case Closure, Single
  def token: String = this match
    case Closure => "closure"
    case Single  => "single"

object Transitivity:
  def from(raw: String): Option[Transitivity] = raw match
    case "closure" => Some(Transitivity.Closure)
    case "single"  => Some(Transitivity.Single)
    case _         => None

/** Published artifact coordinates, after the shared cross-version naming rule.
  */
final case class ToolRoot(
    organization: String,
    name: String,
    revision: String,
    transitivity: Transitivity,
    origin: Option[Repository] = None,
):
  def dependency: Dep = Dep.trusted(organization, name, revision, DepKind.Java)

object ToolRoot:
  def of(
      dependency: Dep,
      version: ScalaVersion,
      platform: Platform,
      transitivity: Transitivity,
      origin: Option[Repository],
  ): ToolRoot = ToolRoot(
    dependency.organization,
    ArtifactName.of(dependency, version, platform),
    dependency.revision,
    transitivity,
    origin,
  )

final case class ToolRequest(
    id: ToolId,
    roots: List[ToolRoot],
    repositories: Repositories,
)
final case class LockedTool(
    id: ToolId,
    roots: List[ToolRoot],
    repositories: List[Repository],
    artifacts: List[LockedArtifact],
    evictions: List[Eviction],
)

enum ToolRootFault:
  case Missing, Duplicated
