package histos.server

import zio.{UIO, ZIO}
import zio.json.*

/** The version a reboot's successor starts with. It is the requester's `HISTOS_VERSION`. An absent one is left to the
  * successor's launcher, as on a cold start. A successor never keeps the identity of the server it replaces, including
  * an inherited `HISTOS_ASSEMBLY` from an older launcher.
  */
final case class Successor(version: Option[String]) derives JsonCodec:
  /** `inherited` with this version in place of the replaced server's. Every other variable is kept, except
    * `HISTOS_ASSEMBLY`, which is dropped.
    */
  def environment(inherited: Map[String, String]): Map[String, String] =
    inherited - Successor.versionName - Successor.assemblyName ++
      version.map(Successor.versionName -> _)

object Successor:
  val versionName: String  = "HISTOS_VERSION"
  val assemblyName: String = "HISTOS_ASSEMBLY"

  /** This process's own version. A blank variable is none. */
  val here: UIO[Successor] =
    named(versionName).map(Successor(_))

  private def named(name: String): UIO[Option[String]] =
    zio.System.env(name).orElseSucceed(None).map(_.filter(_.trim.nonEmpty))
end Successor
