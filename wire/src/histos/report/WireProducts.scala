package histos.report

import histos.load.{NamedProduct, OutputManifest, ProductEntry, ProductError}
import histos.plugin.DeclaredOutput
import zio.json.*

/** Owned-product data remains structured at the socket boundary. */
private[histos] object WireProducts:
  import WireLeaves.given
  import WireContract.given
  import WireStages.given
  import WireBuild.given
  import WireCall.given

  given JsonCodec[DeclaredOutput] = JsonCodec.derived
  given JsonCodec[ProductEntry]   = JsonCodec.derived
  given JsonCodec[NamedProduct]   = JsonCodec.derived
  given JsonCodec[OutputManifest] = JsonCodec.derived
  given JsonCodec[ProductError]   = JsonCodec.derived
end WireProducts
