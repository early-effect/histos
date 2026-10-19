package histos.zinc

import java.nio.file.Path
import java.util.zip.ZipFile

/** A class file that identifies a compiler, a bridge, or histos. */
private[zinc] enum ClassMarker:
  case DottyDriver
  case DottyBridge
  case NscGlobal
  case XsbtBridge
  case JavaUniverse
  case HistosBuild
  case PluginEntry

  def entry: String = this match
    case DottyDriver  => "dotty/tools/dotc/Driver.class"
    case DottyBridge  => "dotty/tools/xsbt/CompilerBridge.class"
    case NscGlobal    => "scala/tools/nsc/Global.class"
    case XsbtBridge   => "scala/tools/xsbt/CompilerBridge.class"
    case JavaUniverse => "scala/reflect/runtime/JavaUniverse.class"
    case HistosBuild  => "histos/HistosBuild.class"
    case PluginEntry  => "META-INF/histos/plugin"

  def under(root: Path): Path =
    entry.split("/").foldLeft(root)(_.resolve(_))

  def present(zip: ZipFile): Boolean =
    Option(zip.getEntry(entry)).isDefined

  def visibleTo(loader: ClassLoader): Boolean =
    Option(loader.getResource(entry)).isDefined

  def classpathBan: Option[ClasspathBan] = this match
    case DottyDriver                          => Some(ClasspathBan.Scala3Compiler)
    case NscGlobal                            => Some(ClasspathBan.Scala2Compiler)
    case HistosBuild | PluginEntry           => Some(ClasspathBan.Histos)
    case DottyBridge | XsbtBridge | JavaUniverse => None

private[zinc] object ClassMarker:
  val userBans: List[ClassMarker] =
    List(ClassMarker.DottyDriver, ClassMarker.NscGlobal, ClassMarker.HistosBuild, ClassMarker.PluginEntry)
