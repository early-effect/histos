package histos

import zio.ZIO

/** The Sonatype publishing a workflow law reads when it has no build: the release secrets and the mode's follow up.
  * `publish` fails `Version`.
  */
object SonaProbe:
  def apply(mode: ReleaseMode): Publishing =
    Publishing.fromEffect(
      request => ZIO.fail(PublishFault.Version(request.revision)),
      Sona.secrets,
      Sona.followUp(mode),
    )
