package histos

import histos.workflow.{ActionPins, CiBranch, CiError, CiInstall, RenderingLaw, RenderingShip, Workflow}

import zio.test.*

/** Remote names the rendering ship's full commit. Wrapper caches, then `./histos`, and does not name a Histos action.
  */
object CiSetupLawSpec extends ZIOSpecDefault:
  private val scala3 = scala"3.8.1"

  given CiBranch      = CiBranch("main")
  given RenderingShip = RenderingLaw.ship

  private val app = module("app", Jvm)(scala3)

  def spec = suite("ci setup")(
    test("remote is the default and pins the rendering ship's full commit"):
      val built    = Projects(app)
      val rendered = built.flatMap(Workflow.render)
      val ship     = RenderingLaw.ship
      val uses     = s"      - uses: ${CiInstall.remote(ship)}"
      val body     = text(rendered)
      val rest     = body.replace(ship.commit.value, "")
      assertTrue(
        built.exists(_.ciSetup == CiSetup.Remote),
        rendered.isRight,
        body.contains(uses),
        ship.commit.value.length == 40,
        !rest.contains(ship.commit.abbreviate.value),
        !body.contains("./.github/actions/setup"),
        !body.contains("origin-head"),
        body.contains("git remote set-head origin --auto"),
      )
    ,
    test("wrapper is pinned caches and ./histos, with no external Histos action"):
      val built    = Projects.ciSetup(CiSetup.Wrapper)(app)
      val rendered = built.flatMap(Workflow.render)
      val body     = text(rendered)
      val cacheAt  = body.indexOf(ActionPins.cache.line)
      val runAt    = body.indexOf("./histos")
      assertTrue(
        built.exists(_.ciSetup == CiSetup.Wrapper),
        rendered.isRight,
        cacheAt >= 0,
        runAt > cacheAt,
        body.contains("histos-toolchains-${{ hashFiles('build.properties') }}-${{ runner.os }}-${{ runner.arch }}"),
        body.contains(
          "coursier-${{ hashFiles('dependencies.lock', 'histos.scala') }}-${{ runner.os }}-${{ runner.arch }}"
        ),
        body.contains("~/.histos/toolchains"),
        body.contains("~/.cache/coursier/v1"),
        body.contains("git remote set-head origin --auto"),
        !body.contains("early-effect/histos/.github/actions"),
        !body.contains("./.github/actions/setup"),
        !body.contains("origin-head"),
      ),
  )

  private def text(rendered: Either[CiError | BuildError, String]): String =
    rendered match
      case Right(body) => body
      case Left(error) =>
        error match
          case ci: CiError       => ci.message
          case graph: BuildError => graph.message
end CiSetupLawSpec
