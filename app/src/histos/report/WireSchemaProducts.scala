package histos.report

import heddle.endpoint.Schema
import histos.load.{NamedProduct, OutputManifest, ProductEntry, ProductEntryPath, ProductError}
import histos.plugin.DeclaredOutput

private object WireSchemaProducts extends WireSchemaSupport:
  import WireSchemaLeaves.given
  import WireSchemaContract.given
  import WireSchemaStages.given
  import WireSchemaBuild.given
  import WireSchemaCall.given

  given Schema[ProductEntryPath] = text("ProductEntryPath")
  given Schema[DeclaredOutput]   = Schema.derived
  given Schema[ProductEntry]     = Schema.derived
  given Schema[NamedProduct]     = Schema.derived
  given Schema[OutputManifest]   = Schema.derived
  given Schema[ProductError]     = Schema.derived
end WireSchemaProducts
