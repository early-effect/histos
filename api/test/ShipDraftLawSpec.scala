package histos

import histos.version.{CommitSha, ShipName, ShipNumber}

import zio.test.*

object ShipDraftLawSpec extends ZIOSpecDefault:
  private val number = ShipNumber.trusted(1, 4, 2)
  private val name   = ShipName.trusted("histos")
  private val sha    = CommitSha.trusted("0123456789abcdef0123456789abcdef01234567")
  private val org    = Organization.trusted("rocks.earlyeffect")
  private val api    = ArtifactBase.trusted("histos-api")
  private val app    = ArtifactBase.trusted("histos-app")
  private val here   = SourcePos("law", 2, 3)

  def spec = suite("ship draft laws")(
    test("one draft keeps its name, and the three publications share that number"):
      val start   = ShipStart.trusted(name)
      val draft   = ShipDraft.trusted(start.name, number)
      val pointer = draft.pointer
      val commit  = CommitPin.trusted(draft.number, sha)
      val release = draft.released
      val pointed = pointer.dep(org, api)
      val stored  = commit.dep(org, app)
      assertTrue(
        start.name == name,
        draft.name == name,
        draft.number == number,
        pointer.number == draft.number,
        commit.number == draft.number,
        release.number == draft.number,
        commit.sha == sha,
        !commit.revision.contains(sha.value),
        pointed.kind == DepKind.Binary,
        pointed.organization == org.value,
        pointed.name == api.value,
        pointed.revision == pointer.revision,
        stored.kind == DepKind.Binary,
        stored.organization == org.value,
        stored.name == app.value,
        stored.revision == commit.revision,
        release.revisionValue.render == release.revision,
        !release.revision.contains("SNAPSHOT"),
      )
    ,
    test("a description replaces a description, and a url does not clear it"):
      val core      = module("core", Jvm)(scala"3.10.0")
      val row       = ShipDef.line(name.value, number, ::(core, Nil), here)
      val described = row.description(pom"library").url(url"https://example.com/histos")
      val replaced  = described.description(pom"replaced")
      assertTrue(
        row.description == None,
        row.url == None,
        described.description.map(_.value) == Some("library"),
        described.url.map(_.value) == Some("https://example.com/histos"),
        replaced.description.map(_.value) == Some("replaced"),
        replaced.url == described.url,
        replaced.name == name.value,
        replaced.number == number,
        replaced.position == here,
        replaced.members.map(_.name) == ::("core", Nil),
        CatalogSpec.empty.ships == Nil,
        CatalogSpec(replaced, described).ships.map(_.description.map(_.value)) == List(
          Some("replaced"),
          Some("library"),
        ),
      )
    ,
    test("a ship builds clients on releases unless it asks for snapshots too, and the catalog row keeps that choice"):
      val core  = module("core", Jvm)(scala"3.10.0")
      val row   = ShipDef.line(name.value, number, ::(core, Nil), here)
      val asked = row.clients(ClientBuilds.ReleasesAndSnapshots)
      assertTrue(
        row.clients == ClientBuilds.Releases,
        asked.clients == ClientBuilds.ReleasesAndSnapshots,
        asked.description == row.description,
        linked(row, core) == Some(ClientBuilds.Releases),
        linked(asked, core) == Some(ClientBuilds.ReleasesAndSnapshots),
      ),
  )

  private def linked(row: ShipDef, member: ProjectDef): Option[ClientBuilds] =
    Projects(CatalogSpec(row), member).toOption.flatMap(_.catalog.get(name)).map(_.clients)
end ShipDraftLawSpec
