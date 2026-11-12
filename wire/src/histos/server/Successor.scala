package histos.server

import java.nio.file.Path

import zio.{UIO, ZIO}
import zio.json.*
import histos.report.WireJson.given JsonCodec[Path]

/** The identity a reboot's successor starts with: the run version `HISTOS_VERSION` names and the jar `HISTOS_ASSEMBLY`
  * names, each the requester's. An absent one is left to the successor's launcher, as on a cold start. A successor
  * never keeps the identity of the server it replaces.
  */
final case class Successor(version: Option[String], jar: Option[Path]) derives JsonCodec:
  /** `inherited` with this identity in place of the replaced server's. Every other variable is kept. */
  def environment(inherited: Map[String, String]): Map[String, String] =
    inherited -- List(Successor.versionName, Successor.jarName) ++
      version.map(Successor.versionName -> _) ++ jar.map(path => Successor.jarName -> path.toString)

object Successor:
  val versionName: String = "HISTOS_VERSION"
  val jarName: String     = "HISTOS_ASSEMBLY"

  /** This process's own identity. A blank variable is none. An environment value holds no NUL, which is the only text
    * `Path.of` refuses on macOS and Linux.
    */
  val here: UIO[Successor] =
    for
      version <- named(versionName)
      raw     <- named(jarName)
      jar     <- ZIO.foreach(raw)(text => ZIO.attempt(Path.of(text)).orDie)
    yield Successor(version, jar)

  private def named(name: String): UIO[Option[String]] =
    zio.System.env(name).orElseSucceed(None).map(_.filter(_.trim.nonEmpty))
end Successor
