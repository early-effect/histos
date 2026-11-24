# Publication compatibility boundary

Scope: assembled review F1 and F2. Ordinary compile/test execution is unchanged and does not acquire registry capabilities.

## Audit ledger

| Category | Hit | Owning step | Status |
| --- | --- | --- | --- |
| Product ordering | Cold preflight selected `ship.binary` before compilation. | F1 | [x] Actual cold DoctorSurface red/green and complete binary gate assertions. |
| Publication authorization | Direct publication reached `Publisher.publish` without complete ship evidence. | F2 | [x] Recording publication boundary tests for all write modes, exact per-artifact baselines, cross-version breaks, missing products, initialization failures, first publication, and covering bumps. |
| Typed failures | Publication needs member/version/coordinate context and distinct incomplete/policy outcomes. | F2 | [x] Local publication and socket-result JSON round trips/refusals on JVM and Native; recursive application schema assertions. |
| Existing failure adapters | `LocalPublish.execute` has a `dieMessage` branch for a skipped scheduler step; legacy `Stock` adapters still flatten unrelated failures. | Later execution-error cleanup, outside F1/F2 | [ ] Deferred explicitly. |
| Existing setup edge | `SetupSurface` performs synchronous filesystem I/O outside a typed effect. The DoctorSurface selector change does not touch that setup path. | Later setup-edge audit, outside F1/F2 | [ ] Deferred explicitly. |
| Caller projection | Release-stage mapping in `Calls` is the separate F8 projection unit. | Projection worker | [ ] Not edited here. |

## Contracts

Preflight removes bump, pin, and binary checks. The complete doctor still selects binary verification. Generated workflow compatibility steps continue compiling published JVM members before `doctor release`.

`LocalPublish.runUsing` checks every JVM member and Scala version of each ship that contains a selected publication module, including members omitted from the publish token. The existing scoped executor compiles those additional JVM stages. Only the selected modules become publication jobs. Their generated-product leases remain alive through verification and packaging.

Before any artifact publication callback, `PublicationCompatibility` reuses `CompatibilityVerification` and `BinaryCheck` against each member's exact artifact coordinate and stable baseline. No stable baseline means explicit first publication. Missing current products, unreachable listings, unavailable expected baselines, analyzer failures, and uncovered early-semver breaks fail publication. A covering bump permits publication.

`LocalPublishError.Compatibility` carries nonempty typed failures with ship, catalog number, member, Scala version, Maven coordinate, and incomplete or policy outcome. Neutral wire data does not reference engine/analyzer or Central implementation types. Baseline failures retain the exact previous revision alongside the target coordinate. Platform exception objects are not exposed in this wire contract. Existing release staging wraps the typed error in `ReleaseError.Stage`.

## Verification

The frozen matching-source launcher `/tmp/heddle-21153a27-compatibility-source-histos` compiled `wire central engine app --platform jvm`. Focused engine tests executed 78 tests across publication/preflight, analyzer/bump, projection/POM, workflow/selection, and real scoped compile/Test suites. Focused wire transport tests executed 12 JVM and 12 Native tests. The recursive application schema suite executed one test. All 103 final tests passed, with no ignored or cached tests. `fmt --check` and `git diff --check` passed.

The original preflight selector was reproduced with an actual cold `DoctorSurface.run` request, then restored to the corrected selector. The non-enforcing publication boundary reproduced callbacks after second-member failures before enforcement was added. Both safety boundaries have durable green assertions.

## Proof boundary

Focused fixtures use the production `LocalPublish.runUsing` publication path, the scoped compiler seam, the real JVM analyzer, injected baseline/feed services, and a recording `Publisher.local` writing only under temporary directories. They do not upload anything. The assembled full test command and the separate F8 caller projection remain integration-owner gates.
