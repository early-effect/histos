package histos.report

import scala.compiletime.constValueTuple
import scala.deriving.Mirror

import heddle.endpoint.{Schema, SchemaDoc, SchemaField}
import histos.load.PluginFault
import histos.native.NativeFetch
import histos.publish.LocalPublishError
import histos.testrun.ModuleTestError
import histos.resolve.{ClasspathScope, LockField, ToolRole, Transitivity}
import histos.schedule.ScheduleError
import histos.version.TagRelation
import histos.{BuildError, JsEnv, Platform}

import WireJson.given

import zio.test.*

object SchemaSpec extends ZIOSpecDefault:
  def spec = suite("hand-written schemas")(
    test("a sum schema names every case"):
      assertTrue(
        variants[BuildError].sorted == names[BuildError].sorted,
        variants[LocalPublishError].sorted == names[LocalPublishError].sorted,
        variants[ModuleTestError].sorted == names[ModuleTestError].sorted,
        variants[BuildFault].sorted == names[BuildFault].sorted,
        variants[ScheduleError[PluginFault]].sorted == names[ScheduleError[PluginFault]].sorted,
        variants[NativeFetch].sorted == names[NativeFetch].sorted,
      )
    ,
    test("an enum schema names every token"):
      assertTrue(
        enumerated[Platform] == Platform.values.toList.map(_.token),
        enumerated[JsEnv] == JsEnv.values.toList.map(_.token),
        enumerated[ToolRole] == ToolRole.values.toList.map(_.token),
        enumerated[Transitivity] == Transitivity.values.toList.map(_.token),
        enumerated[ClasspathScope] == ClasspathScope.values.toList.map(_.header),
        enumerated[LockField] == LockField.values.toList.map(LockField.token),
        enumerated[TagRelation] == TagRelation.values.toList.map(relationToken),
        enumerated[Severity] == Severity.values.toList.map(_.token),
      ),
  )

  private def variants[A](using schema: Schema[A]): List[String] =
    schema.doc match
      case SchemaDoc.OneOf(_, cases) =>
        cases.flatMap:
          case SchemaDoc.Object(_, List(SchemaField(name, _, _)), _) => List(name)
          case _                                                     => Nil
      case _ => Nil

  private def enumerated[A](using schema: Schema[A]): List[String] =
    schema.doc match
      case SchemaDoc.Enum(_, values) => values
      case _                         => Nil

  private def relationToken(relation: TagRelation): String = relation match
    case TagRelation.Matches => "matches"
    case TagRelation.Moved   => "moved"
    case TagRelation.Unknown => "unknown"

  private inline def names[A](using m: Mirror.SumOf[A]): List[String] =
    constValueTuple[m.MirroredElemLabels].toList.map(_.toString)
end SchemaSpec
