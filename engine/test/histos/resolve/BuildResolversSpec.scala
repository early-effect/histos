package histos.resolve

import histos.suite.HistosSpec

import zio.test.*

object BuildResolversSpec extends HistosSpec:
  private val snapshots =
    Repository.Maven(RepoUrl.trusted("https://central.sonatype.com/repository/maven-snapshots/"))

  private val extra = histos.Resolver.Maven(histos.RepositoryUrl("https://example.test/repo"))

  private val listed = Repository.Maven(RepoUrl.trusted("https://example.test/repo"))

  def spec = suite("resolve repositories")(
    test("ivy local, sonatype snapshots, and central are always searched") {
      assertTrue(BuildResolvers(Nil).all == List(Repository.IvyLocal, snapshots, Repository.Central))
    },
    test("a listed repository sits after ivy local and before snapshots") {
      assertTrue(
        BuildResolvers(List(extra)).all == List(Repository.IvyLocal, listed, snapshots, Repository.Central)
      )
    },
    test("a repository the build already placed is not added again") {
      val placed = histos.Resolver.Maven(
        histos.RepositoryUrl("https://central.sonatype.com/repository/maven-snapshots")
      )
      assertTrue(
        BuildResolvers(List(placed, histos.Resolver.Central)).all == List(
          Repository.IvyLocal,
          Repository.Maven(RepoUrl.trusted("https://central.sonatype.com/repository/maven-snapshots")),
          Repository.Central,
        )
      )
    },
    test("central stays where the build placed it") {
      assertTrue(
        BuildResolvers(List(histos.Resolver.Central, extra)).all == List(
          Repository.IvyLocal,
          Repository.Central,
          listed,
          snapshots,
        )
      )
    },
  )
end BuildResolversSpec
