package histos.format

import org.scalafmt.interfaces.RepositoryPackage

import java.io.File

import scala.jdk.CollectionConverters.*

import zio.test.*

object FormatDownloaderLawSpec extends ZIOSpecDefault:
  private val core    = ScalafmtPackage("org.scalameta", "scalafmt-core_2.13", "3.11.5")
  private val reflect = ScalafmtPackage("org.scala-lang", "scala-reflect", "2.13.18")
  private val files   = List(File("core.jar"), File("reflect.jar"), File("scala-library.jar"))

  def spec = suite("locked downloader")(
    test("the locked packages, in any order, get the locked jars and leave no drift"):
      val downloader = LockedDownloader(List(core, reflect), files)
      val handed     = downloader.download("2.13.18", "3.11.5", SilentReporter, asked(List(reflect, core)))
      assertTrue(handed.asScala.toList == files, downloader.drift.isEmpty)
    ,
    test("any other ask gets no jars and is kept as the drift"):
      val moved      = core.copy(version = "3.11.4")
      val downloader = LockedDownloader(List(core, reflect), files)
      val handed     = downloader.download("2.13.18", "3.11.4", SilentReporter, asked(List(moved, reflect)))
      assertTrue(handed.asScala.isEmpty, downloader.drift.contains(List(moved, reflect))),
  )

  private def asked(packages: List[ScalafmtPackage]): java.lang.Iterable[RepositoryPackage] =
    packages.map(found => RepositoryPackage(found.group, found.artifact, found.version)).asJava
end FormatDownloaderLawSpec
