package histos.plugin

import histos.jdk.{JdkMajor, JdkVersion}
import histos.report.WireJson.given
import histos.zinc.{CompilationCapture, CompilationSnapshot}
import histos.suite.HistosSpec
import histos.{Platform, ProjectName, ScalaVersion}

import java.nio.charset.StandardCharsets

import zio.json.*
import zio.json.ast.Json
import zio.test.*

object PluginWireSpec extends HistosSpec:
  private val bytes = "histos-wire".getBytes(StandardCharsets.UTF_8)
  private val hash  = Digests.content(bytes)
  private val jdk   = JdkVersion.trusted("21", JdkMajor.trusted(21))

  private val manifest = SealedArtifactManifest.from(
    List(ArtifactEntry(ArtifactPath("pkg/A.class"), bytes.length.toLong, hash))
  )

  private def snapshot(outputs: SealedArtifactManifest): CompilationSnapshot =
    CompilationSnapshot(
      ProjectName("app"),
      ScalaVersion("3.10.1"),
      Platform.Jvm,
      List(
        SelectedInput(
          RepoPath("src/A.scala"),
          SourceKind.Scala,
          SourceSet.Main,
          SourceProvenance.Unmanaged,
          hash,
          bytes.length.toLong,
        )
      ),
      Nil,
      Nil,
      Nil,
      Nil,
      Nil,
      Nil,
      Nil,
      Nil,
      jdk,
      outputs,
      Nil,
    )

  def spec = suite("plugin wire contracts")(
    test("a snapshot round-trips and a dropped length does not decode") {
      manifest match
        case Left(error)    => assertTrue(error.message.isEmpty && false)
        case Right(outputs) =>
          val shot    = snapshot(outputs)
          val encoded = shot.toJson
          val dropped = drop(encoded, "length")
          assertTrue(
            encoded.fromJson[CompilationSnapshot] == Right(shot),
            dropped.flatMap(_.fromJson[CompilationSnapshot]).isLeft,
          )
    },
    test("unrecorded capture is not a recorded empty source list") {
      manifest match
        case Left(_)        => assertTrue(false)
        case Right(outputs) =>
          val shot                        = snapshot(outputs).copy(sources = Nil)
          val absent: CompilationCapture  = CompilationCapture.Unrecorded
          val present: CompilationCapture = CompilationCapture.Recorded(shot)
          val unrecorded                  = absent.toJson
          val recorded                    = present.toJson
          assertTrue(
            unrecorded != recorded,
            unrecorded.fromJson[CompilationCapture] == Right(absent),
            recorded.fromJson[CompilationCapture] == Right(present),
            absent.record == InputRecord.Unrecorded,
            present.record != InputRecord.Unrecorded,
            recordedSources(present.record).exists(_.isEmpty),
          )
    },
    test("a manifest codec recomputes identity and does not accept a repeated path") {
      manifest match
        case Left(_)      => assertTrue(false)
        case Right(found) =>
          val encoded  = found.toJson
          val decoded  = encoded.fromJson[SealedArtifactManifest]
          val repeated = """{"version":1,"entries":[
            {"path":"pkg/A.class","length":1,"digest":"%s"},
            {"path":"pkg/A.class","length":1,"digest":"%s"}
          ]}""".format(hash.value, hash.value)
          assertTrue(
            decoded.map(_.identity) == Right(found.identity),
            !encoded.contains(""""identity""""),
            repeated.fromJson[SealedArtifactManifest].isLeft,
          )
    },
  )

  private def recordedSources(record: InputRecord): Option[List[SelectedInput]] =
    record match
      case InputRecord.Unrecorded   => None
      case InputRecord.Frozen(body) => Some(body.sources)

  private def drop(encoded: String, field: String): Either[String, String] =
    encoded
      .fromJson[Json]
      .flatMap:
        case obj: Json.Obj => Right(remove(obj, field).toJson)
        case _             => Left("snapshot json is not an object")

  private def remove(json: Json, field: String): Json =
    json match
      case Json.Obj(fields) =>
        Json.Obj(fields.collect:
          case (key, value) if key != field => key -> remove(value, field))
      case Json.Arr(items) => Json.Arr(items.map(remove(_, field)))
      case other           => other
end PluginWireSpec
