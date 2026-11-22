import histos.*
import histos.plugin.*

object plugins extends HistosPlugins:
  override def resolution = Resolution.Lock

  override def resolvers =
    PluginResolvers.Listed(
      List(
        Resolver.Maven(RepositoryUrl("https://central.sonatype.com/repository/maven-snapshots/")),
        Resolver.Central,
      )
    )

  def sources =
    PluginCatalog(
      PluginSources
        .ship(dep"rocks.earlyeffect::specular-histos:0.0.3-782bbef123b1-SNAPSHOT")
        .link(LinkStrategy.BinaryOrSource)
    )
end plugins
