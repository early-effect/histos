# Heddle POM metadata

The authoritative metadata is Heddle's `build.sbt`. Publication preserves the Apache-2.0 HTTP license URL, developer id/name/email/homepage, organization display name/homepage, project homepage, SCM connections, `early-semver`, and each of the nine published families' unsuffixed names and descriptions.

## Audit ledger

| Done | Category / hit | Owning step / proof |
| --- | --- | --- |
| [x] | HTTPS-only network HttpUrl cannot represent the original HTTP license metadata | MetadataUri, literal and typed runtime parser tests; HttpUrl remains unchanged |
| [x] | Developer email/homepage, organization identity, and version scheme are missing from POM projection | Publication facts, builder setters, explicit wire codecs, written POM assertions |
| [x] | Organization/ship-only description loses each module's description | Module setting and module > ship > organization precedence assertions |
| [x] | POM name uses the suffixed artifact id instead of the declared publication name | Module artifact/name projection, nine-family XML assertions |
| [x] | Publication metadata must survive XML escaping and JSON transport | Written XML parsing and codec round trips/refusals |
| [x] | Main-only dependencies and source JAR projection must remain intact | Existing ProjectionSpec, ThinPublishSpec and PublishJarSpec |

There is no PackageDocModel or metadata JSON projection in this checkout. Explicit codecs cover the publication metadata model instead. No networking capability or dependency is added.

Existing configuration defaults in OrganizationFacts, Module and the private ModuleBody revision configuration remain consistent with the builder. The new metadata contracts use typed errors and immutable data, with no casts, partial collection access, or exception inheritance.

## Deferred adjacent audit

These are outside the metadata-field deliverable, not incomplete checks for this change.

| Done | Category / hit | Owning step |
| --- | --- | --- |
| [ ] | Existing HttpUrl checks an HTTPS prefix but does not validate a host; existing Email and ScmConnection permit non-whitespace control characters | Separate parser hardening. Their validators are unchanged; Heddle's declared values pass the new metadata tests |
| [ ] | Existing adjacent publication execution has broad environment/error signatures and a dieMessage branch | Separate publication execution audit |

## Proof

The missing-fields baseline executed against a written local Maven POM and failed the developer email/homepage, organization and version-scheme assertions. The original HTTP Apache license URL now remains HTTP, without weakening HttpUrl.

The installed Histos API predates MetadataUri. Bootstrap compilation used the original root build declarations, then the final MetadataUri declarations were loaded by the source-built launcher. Verification ran the actual compiled API/wire/central/engine/app outputs with a dependencies-only copy of the existing installed runtime. No toolchain, dependency, version pin or remote changed.

For a fresh checkout, follow [the matching snapshot-toolchain handoff](../../README.md#builds-that-use-new-apis). The old installed shim was not validated against the final root build. Publication URL parameters now take `MetadataUri` literals or the typed `MetadataUri.from` parser; `url"..."` remains `HttpUrl` for networking. Do not reuse a temporary launcher path as an installation instruction.

The following recorded commands used the matching source-built launcher, with compiled project classes ahead of its dependencies-only runtime:

- `fmt`, `compile api wire central engine app`, `fmt --check`, and `git diff --check`: exit 0. The final compilation includes every publication consumer; formatting reports no faults or unformatted files.
- `test api wire`: 65 JVM and 63 Native API tests, 46 JVM and 46 Native wire tests. Zero failed, ignored or cached tests.
- `test engine --only '*PomMetadataSpec*' --only '*ProjectionSpec*' --only '*ThinPublishSpec*' --only '*PublishJarSpec*' --only '*VersionStampSpec*' --only '*PluginPublicationSpec*'`: 29 JVM tests, zero failed, ignored or cached tests. Nine local Maven POM files are parsed as XML and all metadata fields are asserted. Description precedence, escaping, artifact-name overrides, source-JAR selection and Main-only dependency projection pass.

Downstream Heddle build adoption and publication through the snapshot channel remain with the integrating owner. This unit does not publish or release artifacts.
