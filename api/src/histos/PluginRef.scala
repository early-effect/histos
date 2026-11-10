package histos

import _root_.scala.annotation.publicInBinary
import _root_.scala.quoted.*

import histos.plugin.LinkStrategy

/** A plugin coordinate before [[Projects]] checks that a module name is a module. */
enum PluginRef:
  case Module(name: String, position: SourcePos)
  case Ship(dependency: Dep, link: LinkStrategy, position: SourcePos)

object PluginRef:
  private[histos] def normalize(value: PluginRef | ShipPlugin): PluginRef = value match
    case ship: ShipPlugin => ship.link(LinkStrategy.BinaryOrSource)
    case ref: PluginRef   => ref

/** `module` names a module value. `ship` names a `Dep`. Omitting [[ShipPlugin.link]] means `BinaryOrSource`. */
object PluginSources:
  def module(project: ModuleDef[?]): PluginRef =
    PluginRef.Module(project.name, project.position)

  inline def ship(dependency: Dep): ShipPlugin =
    ${ PluginSourceMacro.ship('dependency) }
end PluginSources

/** A ship pin that still needs a link, or takes the default when passed to [[Projects.plugins]]. */
final class ShipPlugin private (val dependency: Dep, val position: SourcePos):
  def link(strategy: LinkStrategy): PluginRef =
    PluginRef.Ship(dependency, strategy, position)

object ShipPlugin:
  @publicInBinary private[histos] def make(dependency: Dep, position: SourcePos): ShipPlugin =
    new ShipPlugin(dependency, position)

private object PluginSourceMacro:
  def ship(dependency: Expr[Dep])(using Quotes): Expr[ShipPlugin] =
    val at = position
    '{ ShipPlugin.make($dependency, $at) }

  private def position(using Quotes): Expr[SourcePos] =
    import quotes.reflect.Position
    val at = Position.ofMacroExpansion
    '{ SourcePos(${ Expr(at.sourceFile.path) }, ${ Expr(at.startLine + 1) }, ${ Expr(at.startColumn + 1) }) }
end PluginSourceMacro
