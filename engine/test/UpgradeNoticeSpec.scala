package histos.publish

import histos.version.ShipNumber
import histos.workspace.Workspace

import java.time.Instant

import zio.{Ref, ZIO}
import zio.test.{TestClock, TestSystem, ZIOSpecDefault, assertTrue}

object UpgradeNoticeSpec extends ZIOSpecDefault:
  def spec = suite("upgrade notice")(
    test("a newer release names launcher --latest and a release that is not newer is quiet"):
      val xml =
        "<versions><version>0.1.0</version><version>0.2.0-SNAPSHOT</version><version>0.2.0</version></versions>"
      assertTrue(
        UpgradeNotice.newest(xml).map(_.render) == Some("0.2.0"),
        UpgradeNotice.text(ShipNumber("0.1.0"), ShipNumber("0.2.0")).contains("histos launcher --latest"),
        UpgradeNotice.text(ShipNumber("0.2.0"), ShipNumber("0.2.0")) == "",
        UpgradeNotice.inCi(Map("CI" -> "true")),
        UpgradeNotice.inCi(Map("GITHUB_ACTIONS" -> "true")),
        !UpgradeNotice.inCi(Map("CI" -> "false")),
        UpgradeNotice.step(Map("CI" -> "1"), Instant.EPOCH, None) == UpgradeNotice.Step.Quiet,
      )
    ,
    test("a fresh cache is reused and ci does not fetch"):
      ZIO.scoped:
        for
          home <- Workspace.work("histos-upgrade")
          repo <- Workspace.createDirectories(home.resolve("repo"))
          _    <- Workspace.writeString(repo.resolve(LauncherPin.file), "histos.version=0.1.0\n")
          _    <- TestSystem.putEnv("HISTOS_HOME", home.toString)
          _    <- TestSystem.putProperty("user.home", home.toString)
          _    <- TestClock.setTime(Instant.EPOCH)
          _    <- Workspace.writeString(
            UpgradeNotice.cache(Map("HISTOS_HOME" -> home.toString), home.toString),
            UpgradeNotice.render(UpgradeNotice.Record(Instant.EPOCH, Some(ShipNumber("0.2.0")))),
          )
          hits  <- Ref.make(0)
          fetch = hits.update(_ + 1).as(Option.empty[String])
          text  <- UpgradeNotice.line(repo, fetch)
          _     <- TestSystem.putEnv("CI", "true")
          quiet <- UpgradeNotice.line(repo, hits.update(_ + 1) *> ZIO.dieMessage("fetched"))
          count <- hits.get
        yield assertTrue(
          text.contains("0.2.0"),
          text.contains("histos launcher --latest"),
          quiet == "",
          count == 0,
        )
    ,
  )
