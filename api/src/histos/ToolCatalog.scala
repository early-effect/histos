package histos

final case class CompilerTools(
    semanticdb: ToolRevision,
    scoverage: ToolRevision = ToolRevision("2.5.2"),
)
final case class JsTools(
    revision: JsVersion,
    bridgeScala: ScalaVersion,
    browserScala: ScalaVersion,
    node: ToolRevision,
    jsdom: ToolRevision,
    chekhov: ToolRevision,
)
final case class NativeTools(revision: NativeVersion, bridgeScala: ScalaVersion)

/** `dynamic` is the scalafmt-dynamic-core revision. `scala` is the Scala version that revision loads scalafmt-core for.
  * The scalafmt-core revision is the `version` line in the configuration.
  */
final case class FormatTools(dynamic: ToolRevision, scala: ScalaVersion)

/** Build-level defaults. A module may override its platform toolchain revision.
  */
final case class ToolCatalog(
    compilers: CompilerTools,
    js: JsTools,
    native: NativeTools,
    format: FormatTools,
)

object ToolCatalog:
  val default: ToolCatalog = ToolCatalog(
    CompilerTools(ToolRevision("4.17.3")),
    JsTools(
      JsVersion("1.23.0"),
      ScalaVersion("2.13.18"),
      ScalaVersion("3.10.1"),
      ToolRevision("1.7.0"),
      ToolRevision("1.1.1"),
      ToolRevision("0.1.3"),
    ),
    NativeTools(NativeVersion("0.5.13-20261122-fa4437d-SNAPSHOT"), ScalaVersion("3.10.1")),
    FormatTools(ToolRevision("3.11.5"), ScalaVersion("2.13.18")),
  )
