package histos.bsp

import zio.json.*
import zio.json.ast.Json

/** JSON-RPC and the BSP messages we actually speak. Payloads are codecs. `id` is the one untagged sum. */
object Rpc:
  enum RpcId:
    case Num(value: Int)
    case Text(value: String)
    case Absent

  object RpcId:
    given JsonCodec[RpcId] =
      JsonCodec[Json].transformOrFail(
        {
          case Json.Num(value) =>
            try Right(RpcId.Num(value.intValueExact))
            catch case _: ArithmeticException => Left("bsp id is not an int")
          case Json.Str(value) => Right(RpcId.Text(value))
          case Json.Null       => Right(RpcId.Absent)
          case _               => Left("bsp id is not a string or a number")
        },
        {
          case RpcId.Num(value)  => Json.Num(value)
          case RpcId.Text(value) => Json.Str(value)
          case RpcId.Absent      => Json.Null
        },
      )

  final case class Incoming(
      method: Option[String] = None,
      id: Option[RpcId] = None,
      params: Option[Json] = None,
  ) derives JsonCodec

  final case class ErrBody(code: Int, message: String) derives JsonCodec
  final case class RpcErr(jsonrpc: String, id: RpcId, error: ErrBody) derives JsonCodec
  final case class RpcOk[A](jsonrpc: String, id: RpcId, result: A)

  object RpcOk:
    given [A: JsonCodec]: JsonCodec[RpcOk[A]] = JsonCodec.derived

  /** `build/shutdown` result is JSON null, not an object. */
  final case class NullResult()

  object NullResult:
    given JsonCodec[NullResult] =
      JsonCodec[Json].transformOrFail(
        {
          case Json.Null => Right(NullResult())
          case _         => Left("expected null")
        },
        _ => Json.Null,
      )

  final case class TargetId(uri: String) derives JsonCodec
  final case class Targets(targets: List[TargetId]) derives JsonCodec
  final case class CompileParams(targets: List[TargetId], originId: Option[String] = None) derives JsonCodec

  final case class CompileProvider(languageIds: List[String]) derives JsonCodec
  final case class Capabilities(
      compileProvider: CompileProvider,
      dependencySourcesProvider: Boolean,
      jvmRunEnvironmentProvider: Boolean,
      jvmTestEnvironmentProvider: Boolean,
  ) derives JsonCodec
  final case class InitializeResult(
      displayName: String,
      version: String,
      bspVersion: String,
      capabilities: Capabilities,
  ) derives JsonCodec

  final case class TargetCapabilities(canCompile: Boolean, canTest: Boolean, canRun: Boolean) derives JsonCodec
  final case class ScalaBuildTarget(
      scalaOrganization: String,
      scalaVersion: String,
      scalaBinaryVersion: String,
      platform: Int,
      jars: List[String],
  ) derives JsonCodec
  final case class BuildTarget(
      id: TargetId,
      displayName: String,
      baseDirectory: String,
      tags: List[String],
      languageIds: List[String],
      dependencies: List[TargetId],
      capabilities: TargetCapabilities,
      dataKind: String,
      data: ScalaBuildTarget,
  ) derives JsonCodec
  final case class BuildTargetsResult(targets: List[BuildTarget]) derives JsonCodec

  final case class SourceItem(uri: String, kind: Int, generated: Boolean) derives JsonCodec
  final case class SourcesItem(target: TargetId, sources: List[SourceItem]) derives JsonCodec
  final case class SourcesResult(items: List[SourcesItem]) derives JsonCodec

  final case class ScalacOptionsItem(
      target: TargetId,
      options: List[String],
      classpath: List[String],
      classDirectory: String,
  ) derives JsonCodec
  final case class ScalacOptionsResult(items: List[ScalacOptionsItem]) derives JsonCodec
  final case class CompileResult(statusCode: Int, originId: Option[String] = None) derives JsonCodec

  final case class TextDocument(uri: String) derives JsonCodec
  final case class Position(line: Int, character: Int) derives JsonCodec
  final case class Range(start: Position, end: Position) derives JsonCodec
  final case class Diagnostic(range: Range, severity: Int, source: String, message: String) derives JsonCodec
  final case class PublishDiagnostics(
      textDocument: TextDocument,
      buildTarget: TargetId,
      diagnostics: List[Diagnostic],
      reset: Boolean,
  ) derives JsonCodec
  final case class LogMessage(`type`: Int, message: String) derives JsonCodec
  final case class DependencySourcesItem(target: TargetId, sources: List[String]) derives JsonCodec
  final case class DependencySourcesResult(items: List[DependencySourcesItem]) derives JsonCodec

  /** JSON key `class` is the BSP field. Scala will not use that word as a parameter. */
  final case class ScalaMainClass(className: String, arguments: List[String], jvmOptions: List[String])

  object ScalaMainClass:
    given JsonCodec[ScalaMainClass] =
      JsonCodec[Json].transformOrFail(
        {
          case Json.Obj(fields) =>
            val map = fields.toMap
            for
              name <- text(map, "class")
              arguments <- texts(map, "arguments")
              options <- texts(map, "jvmOptions")
            yield ScalaMainClass(name, arguments, options)
          case _ => Left("a main class is an object")
        },
        main =>
          Json.Obj(
            "class" -> Json.Str(main.className),
            "arguments" -> Json.Arr(main.arguments.map(Json.Str(_))*),
            "jvmOptions" -> Json.Arr(main.jvmOptions.map(Json.Str(_))*),
          ),
      )

    private def text(map: Map[String, Json], name: String): Either[String, String] =
      map.get(name) match
        case Some(Json.Str(value)) => Right(value)
        case _                     => Left(s"$name is required")

    private def texts(map: Map[String, Json], name: String): Either[String, List[String]] =
      map.get(name) match
        case Some(Json.Arr(values)) =>
          values.toList.foldLeft[Either[String, List[String]]](Right(Nil)):
            case (Left(error), _)              => Left(error)
            case (Right(acc), Json.Str(value)) => Right(acc :+ value)
            case (Right(_), _)                 => Left(s"$name is a list of strings")
        case _ => Left(s"$name is required")

  final case class ScalaMainClassesItem(target: TargetId, classes: List[ScalaMainClass]) derives JsonCodec
  final case class ScalaMainClassesResult(items: List[ScalaMainClassesItem]) derives JsonCodec
  final case class ScalaTestClassesItem(target: TargetId, classes: List[String], framework: Option[String]) derives JsonCodec
  final case class ScalaTestClassesResult(items: List[ScalaTestClassesItem]) derives JsonCodec
  final case class JvmEnvironmentItem(
      target: TargetId,
      classpath: List[String],
      jvmOptions: List[String],
      workingDirectory: String,
      environmentVariables: Map[String, String],
  ) derives JsonCodec
  final case class JvmEnvironmentResult(items: List[JvmEnvironmentItem]) derives JsonCodec

  final case class RpcNote[A](jsonrpc: String, method: String, params: A)

  object RpcNote:
    given [A: JsonCodec]: JsonCodec[RpcNote[A]] = JsonCodec.derived

  def note[A: JsonCodec](method: String, params: A): String =
    RpcNote("2.0", method, params).toJson

  def ok[A: JsonCodec](id: RpcId, result: A): String =
    RpcOk("2.0", id, result).toJson

  def fail(id: RpcId, code: Int, message: String): String =
    RpcErr("2.0", id, ErrBody(code, message)).toJson

  enum ParamsDecode:
    case Missing
    case Rejected

  def idOf(incoming: Incoming): RpcId =
    incoming.id.getOrElse(RpcId.Absent)

  def targets(params: Option[Json]): Either[ParamsDecode, List[String]] =
    params match
      case None => Left(ParamsDecode.Missing)
      case Some(json) =>
        json.as[Targets].left.map(_ => ParamsDecode.Rejected).map(_.targets.map(_.uri))

  def compileParams(params: Option[Json]): Either[ParamsDecode, CompileParams] =
    params match
      case None       => Left(ParamsDecode.Missing)
      case Some(json) => json.as[CompileParams].left.map(_ => ParamsDecode.Rejected)
end Rpc
