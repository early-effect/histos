# Release

Histos publishes the way it is meant to replace sbt and zipx. One catalog row is a ship. `publishLocal` writes this machine. `publish snapshots` uploads to the Central snapshot repository. `publish release` signs a bundle and uploads it to the Central Portal. A release is that bundle after an explicit promote. A tool people install can also be a GitHub release asset. The library version stays off Central until promote.

This file is the decision. Snapshot upload already exists. The portal upload does not. Do not cut Histos `0.0.1` as part of building this.

The build file says what a ship publishes. The invocation says which portal, which signer, and whether this run promotes. A mode stored on the ship would fire on the next real upload.

## Catalog

A row is [ship](../api/src/histos/Literals.scala):

```scala
ship("specular", ship"0.21.2")(core, site, plugin)
ship("specular-histos", ship"0.0.1")(histosAdapter)
```

The ship name is the release. The members are the publications. The Maven artifact id stays the module's artifact. There is no second constructor for a lone module. One member is a ship of one.

The number in the file is the next release. Edit it, then publish. Publish does not rewrite the file and does not open a bump pull request. [PublishAction.Release](../api/src/histos/version/Versions.scala) carries the release version and nothing else.

An early-effect ship releases its ship number. A manual `major.minor.patch` releases as written. A manual snapshot and a `-ci` coordinate are refused by `publish release`. Histos itself stays `manual("histos", manual"0.0.1-SNAPSHOT")` until a later decision cuts it.

## Publications

A module publishes one artifact. Unspecified, it is a library jar for each platform that module builds: the Scala suffix, the class jar, a sources jar, a javadoc jar, and a pom. The javadoc jar contains a README. Real scaladoc is a later product. The pom lists that module's dependencies.

Central requires the javadoc jar, the sources jar, `.md5`, `.sha1`, and `.asc` on the jar, the pom, the sources jar, and the javadoc jar. `.sha256` stays. An `.asc` is not checksummed, and a checksum is not signed. The pom carries `name`, `description`, `url`, one license, one developer, and `scm`. `name` defaults to the artifact id. License, developer, and scm are written once on the organization. A ship overrides description and url. The git remote and a `LICENSE` file are not read. Forks and a detached checkout lie.

These are the same packer with a different coordinate:

- An sbt plugin is a module whose artifact id carries the sbt series. The descriptor is already inside the jar. The portal code has no sbt case. The first series this packs is sbt 2 on Scala 3. The publication names the series, so another series is the same surface.
- A Histos plugin is that library jar. The loader already resolves a published coordinate. The marker is inside the jar.
- A CLI module picks one form. Assembly is the fat jar: no Scala suffix, a real pom with an empty dependency list, `Main-Class` in the manifest. That is `java -jar` and the histos launcher. Application is the thin jar whose pom carries the classpath, which is `coursier launch`.

`histos-api_3` and the `histos` assembly stay two modules in one ship, and one deployment. [Projects.assembly](../api/src/histos/Graph.scala) as a graph-level side door goes away. The empty pom it writes for snapshots will not pass Central.

An asset publication is a file the build already produced, plus a name. For Histos the file is `dist/histos.jar`. A native image and a native launcher stay later work. The asset is not a GAV and it is not in the portal zip.

## Commands

`publishLocal` is its own command, reserved the way `testQuick` and `cleanFull` are.

```text
./histos publishLocal
./histos publishLocal target/repo
./histos publishLocal specular

./histos publish snapshots
./histos publish release
./histos publish release specular
./histos publish release specular --automatic
./histos publish promote <deploymentId>
./histos publish drop <deploymentId>
./histos publish assets specular

./histos portal
```

`publishLocal` writes a Maven repository. The default root is `~/.m2/repository`. It does not touch the network and it does not sign. The coordinate is the one this session compiles, so development writes `1.4.2-ci`. A snapshot session writes the snapshot coordinate. A release session writes the release number, unsigned. An already-released number is still written. `publishLocal` does not read Central.

`publish snapshots` stays the PUT to `https://central.sonatype.com/repository/maven-snapshots`. It reads `SONATYPE_USERNAME` and `SONATYPE_PASSWORD`. A release revision is refused before a request.

`publish release` signs and uploads one bundle per ship to the Central Portal. The default is hold: `publishingType=USER_MANAGED`, poll until `VALIDATED` or `FAILED`, print the deployment id, and write it to `.histos/deployments/<ship>`. That file is not [`.histos/releases`](../engine/src/histos/version/VersionTask.scala). `--automatic` sends `publishingType=AUTOMATIC` and polls until `PUBLISHED` or `FAILED`. That makes the version immutable as soon as validation passes. The catalog does not store the mode.

`publish promote` and `publish drop` are the second act. They take a deployment id because that is the portal's input, and they use the portal base URL of this run. Promote publishes a `VALIDATED` deployment. Drop drops it, so the version stays free. Drop then deletes the GitHub asset uploaded for that ship, when one was uploaded. The deployment is dropped first. An asset deletion that fails is reported after the version is free.

`publish release` refuses `-ci` and a manual snapshot. It refuses a ship that depends on a snapshot. That refusal is that ship. The other ships in the same invocation still publish. A number the registry already has is a hold and is not uploaded again. One failed deployment is that ship.

`publish` no longer takes a directory. The directory form of [PublishWhere](../engine/src/histos/publish/PublishWhere.scala) moves to `publishLocal`.

A token is resolved in this order. A ship name publishes that whole ship. An aggregate name publishes the modules it lists. A module name publishes that module, and a module that is not in a ship is an error. On `publishLocal`, a token that is none of those is a Maven directory, one per invocation, relative to the repo or absolute. A ship name wins when a ship and a module share it.

No name on `publishLocal` or `publish snapshots` is every ship the session can write. No name on `publish release` is every ship whose number is not already on the registry this run is using. No name on `publish assets` is every ship that has an asset publication and a recorded `VALIDATED` deployment. A ship with no recorded deployment fails, and the binary is not uploaded.

`./histos portal` serves the rehearsal portal. `publish release` against it does not upload assets and does not call GitHub.

## Aggregates

An aggregate has no version. [aggregate](../api/src/histos/Builder.scala) stays a name over modules.

```scala
aggregate("root", core, site, adapter)

./histos publish release root
```

`core` and `site` publish as `0.21.2`. `adapter` publishes as `0.0.1`. Each platform that module builds is a jar at that module's number. Naming the aggregate does not pull the other members of those ships. A published `dependsOn` still joins the run, at its own ship version. A child that is not in a ship is reported and skipped. Naming that module directly remains an error.

Central validates one zip as one deployment. An aggregate that spans two ship numbers is two deployments, so one bad pom fails that ship.

## Tags

A tag is a label on a commit. It does not upload. Several tags may point at one commit:

```text
specular/v0.21.2
specular-histos/v0.0.1
```

One ship in the catalog uses a bare `v0.21.2`. More than one ship uses `<ship>/v<number>`. The publisher does not require a tag. The workflow slice runs the command when a tag appears, and that tag must match the number in the file at that commit.

A matching tag runs `publish release <ship>` in hold, then `publish assets <ship>`. Promote is a separate workflow dispatch. The tag workflow does not pass `--automatic`.

## Portal

The release runs on any machine that has the secrets. CI is one such machine. Histos does not install `gpg`.

The portal base URL defaults to `https://central.sonatype.com`. `HISTOS_PORTAL` overrides it. Tests and a rehearsal set that override. The client is one implementation.

Authorization is `Bearer` with the base64 of `SONATYPE_USERNAME:SONATYPE_PASSWORD`. The upload is `POST /api/v1/publisher/upload` as `multipart/form-data` with one part named `bundle`. The client polls the deployment until the state this mode waits for. The states are `PENDING`, `VALIDATING`, `VALIDATED`, `PUBLISHING`, `PUBLISHED`, and `FAILED`. Promote is `POST /api/v1/publisher/deployment/<deploymentId>`. Drop is the portal's drop for that id. HTTP goes through heddle.

`./histos portal` is a heddle server bound to loopback. It speaks those routes, requires the Bearer token, and keeps deployments on disk so promote can be a second process. It fails a bundle Central would fail, and the deployment carries the validation report: Maven layout, the jar, sources, and javadoc, `.md5` and `.sha1`, `.asc` on the artifacts, a pom with the required fields, checksums that match, no snapshot version, no snapshot dependency, a zip under the 1GB cap, and a GAV the fake has not already published. An empty pom is `FAILED`.

Tests start that server in a `Scope` and pass the URL into the layer. They do not call Central.

On a real portal, two reads run before an upload. A HEAD to repo1 refuses a GAV Central already has. A lookup of the signing key on `keyserver.ubuntu.com` refuses a key Central cannot fetch. Either miss fails before a deployment is opened. A rehearsal skips both reads. The fake is the only registry for that run, so a public version can be rehearsed. Histos does not upload the public key. Sending it remains `gpg --send-keys`, once.

Central is the registry a real release reads. `.histos/releases` stays the evidence the versions task reads until that task asks this same registry. Tests of that task keep the file.

## Signing

`Signer` is a layer. The build file does not name it.

Unset `HISTOS_SIGNER`, with key material present, signs in process. The secret is `PGP_SECRET` (base64, the sbt-ci-release form) or `PGP_KEY_HEX`. Both set means the same key. `PGP_PASSPHRASE` unlocks it. The signer picks the subkey flagged for signing. No signing key, or several and none flagged, fails before an upload. Bouncy Castle is an explicit provider on those calls. It is not registered on the JVM. This is the CI path. The private key is already in the environment. In-process signing does not import a key into a runner keyring.

`HISTOS_SIGNER=gpg` shells out to `gpg --batch --detach-sign --armor --pinentry-mode loopback`. The passphrase is stdin. It is not an argument. A missing `gpg` fails before an upload. A non-zero exit fails the same way. The signer does not read gpg's prose and does not import a key. This is the laptop path: an agent or a hardware token. `gpg` on `PATH` does not select it. A GitHub-hosted runner uses the in-process signer.

The required suite proves the in-process signer against the fake portal. The assertion is that the signature verifies. Armor bytes are not a golden file. The gpg layer's required test is the argument list and the passphrase on stdin. A round-trip through real `gpg` runs when the binary is present.

## Assets

`publish assets` uploads the asset publication to the GitHub release for the tag. It runs at `VALIDATED`, before promote. The fat jar is installable while the library version can still be dropped. `java -jar` does not need the library on Central.

Humans and downstream CI fetch it with `gh release download`. `./histos` keeps the Maven pin in [LauncherPin](../engine/src/histos/publish/LauncherPin.scala) and the sha256 check. The pin is a Maven URL. A GitHub asset URL does not fit it, and the script does not grow a second shape. Two ways in, the same bytes.

Inside this repository, `./histos` already prefers `dist/histos.jar` when the digest matches. The asset is for a checkout that is not this repo, and for CI of another build.

The asset client uses heddle. Tests bind a loopback server. They do not call GitHub. `--automatic` does not upload assets. The caller runs `publish assets`.

The README teaches this for Histos development: rehearse against `./histos portal`, install the asset after `VALIDATED`, promote on a later dispatch, and keep the launcher on Maven. It also says when another project wants the same split. That project has a tool people install and a library other builds depend on. A library-only ship publishes no asset.

## Slices

1. Publisher. `publishLocal`, `publish release` in both modes, promote, drop, the in-process signer and the gpg signer, the bundle, the portal client, the heddle portal, the repo1 and keyserver preflights, and the publications above. The asset is named on the module. The GitHub client is the next slice. Same selection for snapshots. No workflow. No tag of Histos.
2. Assets. `publish assets`, delete-on-drop, and the README section.
3. Workflow. A matching tag or a named dispatch runs `./histos publish release <ship>` in hold, then `./histos publish assets <ship>`. A second dispatch runs `./histos publish promote`. The command stays the thing that publishes.

## Checklist

Top to bottom. Finish one item and prove it before starting the next. The proof is a ZIO Test suite run with `./histos test` for the modules that item touches. Tests bind loopback. The Histos catalog stays `manual("histos", manual"0.0.1-SNAPSHOT")`. No item uploads to Central, sends a public key, or tags Histos.

### Publisher

- [x] **Withdraw `CatalogBump`.** `PublishAction.Release` carries the release version only. The version specs show that publish leaves the catalog file alone.

- [x] **Split the command.** `publishLocal` writes a Maven directory, default `~/.m2/repository`. `publish snapshots` is the snapshot upload. A directory argument is rejected by `publish`. The CLI, preview, and the MCP publish tool share that split. A `-ci` coordinate writes locally and is refused by snapshots. Grammar and local-publish specs cover both.

- [x] **Selection.** A ship name publishes that ship. An aggregate publishes the modules it lists, at each module's own ship version, and does not pull the other members of those ships. A module outside a ship is an error. A directory token is legal only on `publishLocal`. A ship name wins when a ship and a module share it. No name on `publishLocal` or `publish snapshots` is every ship the session can write. A snapshot dependency refuses that ship and the rest of the invocation continues. One spec covers each of those.

- [x] **Publication and Central pom.** A module has one publication. Unspecified, it is a library. The organization carries license, developer, and scm. A ship may override description and url. `name` defaults to the artifact id. The release pom includes those fields and the module's dependencies. Missing license, developer, or scm fails before signing. `publishLocal` still writes the coordinate the session compiles, including `-ci`. Specs cover the default, the ship override, and the refusal.

- [x] **Stage a library, unsigned.** One library publication stages the jar, a sources jar, a javadoc jar that contains a README, the pom, and `.md5`, `.sha1`, and `.sha256` on each of those four. The spec reads the directory and the pom. No `.asc` yet, and no HTTP.

- [x] **Stage the other Maven forms.** An sbt 2 plugin on Scala 3 stages under the sbt 2 artifact id, and the descriptor stays in the jar. Assembly stages a fat jar with no Scala suffix, a pom whose dependency list is empty, and `Main-Class` in the manifest. Application stages the thin jar and a pom that lists the classpath. A module with two publications is refused at load. `Projects.assembly` is removed. This repo's launcher coordinate still publishes from the app module's assembly publication, with a real pom. The existing snapshot specs stay green without a live upload.

- [x] **Signer.** `Signer.local` signs with a generated key and the public half verifies. A secret with no signing subkey fails before a client exists. `PGP_SECRET` and `PGP_KEY_HEX`, when both are set, must be the same key. `HISTOS_SIGNER=gpg` is the argument list and the passphrase on stdin. `gpg` on `PATH` with the variable unset stays on the local signer. Bouncy Castle is passed as a provider and is not registered on the JVM. The gpg round-trip runs when `gpg` is present.

- [x] **Bundle.** Zip the signed stage in Maven layout. The spec unzips it and finds every required name. A declared size over 1GB is refused without writing a 1GB file.

- [x] **Portal client.** One heddle client. A scripted loopback server checks the Bearer token and the multipart part named `bundle`. Hold waits until `VALIDATED`. `--automatic` waits until `PUBLISHED`. `FAILED` stops the poll. Promote and drop call the deployment route. The suite makes no connection to `central.sonatype.com`.

- [ ] **Rehearsal portal.** `./histos portal` binds loopback, stores deployments on disk, and returns the validation report from the Portal section: layout, file set, checksums, pom fields, snapshot version, snapshot dependency, a GAV already in the store, an empty pom, and the 1GB cap. A second process promotes and drops against that disk. The suite starts the server in a `Scope`.

- [ ] **Preflights.** When the portal URL is Central, HEAD repo1 and look up the signing key on `keyserver.ubuntu.com`, both through heddle against loopback stand-ins. Either miss fails before upload. When `HISTOS_PORTAL` points at the rehearsal portal, neither stand-in is called, and a version that is public on the stand-in still uploads to the fake.

- [ ] **Wire release, promote, and drop.** Hold writes `.histos/deployments/<ship>`. That path is not `.histos/releases`. Against the rehearsal portal, a two-ship fixture leaves the snapshot-dependent ship refused and the other `VALIDATED`. A `FAILED` bundle is that ship. A GAV the fake has published is a hold and opens no second deployment. `--automatic` reaches `PUBLISHED` on the fake. Drop leaves the version free to upload again. Missing secrets fail before a socket.

- [ ] **Histos-shaped rehearsal.** A fixture with a library module and an assembly module, at release numbers, reaches `VALIDATED` and then `PUBLISHED` on the rehearsal portal. `publish release` in this repository refuses before any HTTP, because the catalog is still the manual snapshot. No Central upload and no tag.

### Assets

- [ ] **Name the asset.** The assembly module names `dist/histos.jar` as an asset. The portal zip has no such entry. A library-only ship has no asset to upload.

- [ ] **`publish assets`.** A loopback stand-in receives the jar only when `.histos/deployments/<ship>` records `VALIDATED`. A ship with no recorded deployment fails and uploads nothing. `--automatic` does not upload an asset. The stored bytes match the jar.

- [ ] **Drop deletes the asset second.** The deployment is dropped first. The stand-in then shows the asset gone. When the asset call fails, the deployment stays dropped and the reported error is that failure.

- [ ] **README.** Histos development: rehearse with `./histos portal`, install the asset after `VALIDATED`, promote on a later dispatch, and leave the launcher on the Maven pin. Other projects: use the same split when people install a tool and builds depend on a library. A library-only ship publishes no asset.

### Workflow

- [ ] **Generated workflow.** A matching tag, or a named dispatch, runs `./histos publish release <ship>` and then `./histos publish assets <ship>`. A second dispatch runs `./histos publish promote`. The generated file does not pass `--automatic`. `./histos workflow --check` is the proof. No tag of Histos is created.

### Ready

- [ ] **One rehearsal, any Histos project.** A fixture that is not this repo's snapshot catalog publishes a library, an sbt 2 plugin, and an assembly, as two ships. One ship is refused for a snapshot dependency. The other reaches `VALIDATED` on the rehearsal portal, the asset lands on the GitHub stand-in, promote reaches `PUBLISHED`, and a later drop frees the version. A run with no `HISTOS_PORTAL` and no Sonatype secrets fails before a connection. `publish release` in this repository still refuses. The same commands are what another Histos project runs, and what this repository runs the day its catalog leaves `0.0.1-SNAPSHOT`.
