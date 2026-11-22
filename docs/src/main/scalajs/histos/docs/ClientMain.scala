package histos.docs

import specular.client.SpecularClient
import zio.*

/** The browser half. These pages are static, so the mount map is empty. `mountAll` installs the preview reload. */
object ClientMain extends ZIOAppDefault:
  def run =
    ZIO.scoped:
      SpecularClient.mountAll(Map.empty) *> ZIO.never
