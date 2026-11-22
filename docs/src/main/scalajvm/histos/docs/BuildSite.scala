package histos.docs

import earlyeffect.docs.EarlyEffectTheme
import specular.site.*

import java.nio.file.{Files, Path, StandardCopyOption}

import zio.*

/** The Histos site. `./histos run dev.earlyeffect.specular/site` forks this main. */
object BuildSite extends DocsSite:

  @navLabel("Start")
  final case class Start(home: Home.type, install: Install.type, first: FirstProject.type, model: BuildModel.type)

  @navLabel("Reference")
  final case class Reference(
      info: CommandsInfo.type,
      build: CommandsBuild.type,
      test: CommandsTest.type,
      project: CommandsProject.type,
      publish: CommandsPublish.type,
      dependencies: CommandsDependencies.type,
      ci: CommandsCi.type,
      cache: CommandsCache.type,
      toolchain: CommandsToolchain.type,
      server: CommandsServer.type,
      preview: CommandsPreview.type,
  )

  @navLabel("Why")
  final case class Why(
      lineage: Lineage.type,
      metals: Metals.type,
      mcp: Mcp.type,
      cache: Cache.type,
      workflows: Workflows.type,
      publishing: Publishing.type,
      plugins: Plugins.type,
  )

  final case class HistosNav(start: Start, reference: Reference, why: Why) derives SiteNav

  private val siteNav: NavModel = SiteNav[HistosNav].toNavModel

  def pages: Vector[specular.DocPage] = siteNav.pages

  override def site(settings: DocsSettings): SiteModel =
    val branded = EarlyEffectTheme.brand(super.site(settings))
    branded.copy(
      nav = Some(siteNav),
      clientScript = Some("assets/client.js"),
      summaryMarkdown = Some(
        "Histos is a Scala build tool. The build is a program that returns a graph. The work is an effect. A person, an editor, and an agent ask for the same operations."
      ),
      brand = Some(
        Brand(
          name = settings.meta.displayTitle,
          links = Vector(EarlyEffectTheme.github("https://github.com/early-effect/histos")),
        )
      ),
    )

  override def layers: ZLayer[Any, Nothing, SiteBuilder] =
    EarlyEffectTheme.layers

  override def afterBuild(out: Path, result: SiteOutput): IO[SiteError, Unit] =
    val _       = result
    val front   = out.resolve(s"${Home.doc.slug}.html")
    val index   = out.resolve("index.html")
    val promote =
      ZIO.attemptBlockingIO(Files.copy(front, index, StandardCopyOption.REPLACE_EXISTING)).mapError { err =>
        SiteError.WriteFailed(index, err)
      }
    promote *> EarlyEffectTheme.writeLogo(out)
end BuildSite
