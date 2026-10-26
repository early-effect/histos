package histos

final case class CompilerTools(semanticdb: ToolRevision)
final case class JsTools(
    revision: JsVersion,
    bridgeScala: ScalaVersion,
    browserScala: ScalaVersion,
    node: ToolRevision,
    jsdom: ToolRevision,
    chekhov: ToolRevision,
)
final case class NativeTools(revision: NativeVersion, bridgeScala: ScalaVersion)

/** Build-level defaults. A module may override its platform toolchain revision.
  */
final case class ToolCatalog(
    compilers: CompilerTools,
    js: JsTools,
    native: NativeTools,
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
    NativeTools(NativeVersion("0.5.12"), ScalaVersion("3.9.0")),
  )
