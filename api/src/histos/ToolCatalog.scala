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
      JsVersion("1.22.0"),
      ScalaVersion("2.13.18"),
      ScalaVersion("3.9.0"),
      ToolRevision("1.6.0"),
      ToolRevision("1.1.1"),
      ToolRevision("0.1.3"),
    ),
    // A nightly: the release aborts in libunwind on Apple Silicon, and its module-initialization wait recurses
    // until a thread's stack runs out (scala-native#4366, fixed by #5221).
    NativeTools(NativeVersion("0.5.13-20261111-9be93b7-SNAPSHOT"), ScalaVersion("3.9.0")),
    FormatTools(ToolRevision("3.11.5"), ScalaVersion("2.13.18")),
  )
