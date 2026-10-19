package histos.plugin

import _root_.scala.annotation.publicInBinary

import java.util.Locale

/** Five whitespace-separated GitHub Actions cron fields. The value is those fields, single-spaced. */
opaque type Cron = String

object Cron:
  given CanEqual[Cron, Cron] = CanEqual.derived

  inline def apply(inline raw: String): Cron = ${ PluginMacros.cron('raw) }

  def from(raw: String): Either[CronError, Cron] =
    if raw.trim.isEmpty then Left(CronError.Empty)
    else
      val fields = raw.trim.split("\\s+").toList
      if fields.length != 5 then Left(CronError.Fields(fields.length))
      else
        val bad = fields.zip(bounds).zipWithIndex.collectFirst:
          case ((field, (min, max, names)), index) if !fieldOk(field, min, max, names) =>
            CronError.Field(index, field)
        bad match
          case Some(error) => Left(error)
          case None        => Right(trusted(fields.mkString(" ")))

  extension (cron: Cron) def value: String = cron

  @publicInBinary private[histos] def trusted(raw: String): Cron = raw

  private val months: Map[String, Int] = Map(
    "JAN" -> 1, "FEB" -> 2, "MAR" -> 3, "APR" -> 4, "MAY" -> 5, "JUN" -> 6,
    "JUL" -> 7, "AUG" -> 8, "SEP" -> 9, "OCT" -> 10, "NOV" -> 11, "DEC" -> 12,
  )

  private val days: Map[String, Int] = Map(
    "SUN" -> 0, "MON" -> 1, "TUE" -> 2, "WED" -> 3, "THU" -> 4, "FRI" -> 5, "SAT" -> 6,
  )

  private val bounds: List[(Int, Int, Map[String, Int])] = List(
    (0, 59, Map.empty),
    (0, 23, Map.empty),
    (1, 31, Map.empty),
    (1, 12, months),
    (0, 6, days),
  )

  private def fieldOk(raw: String, min: Int, max: Int, names: Map[String, Int]): Boolean =
    raw.nonEmpty && raw.split(",", -1).toList.forall(item => itemOk(item, min, max, names))

  private def itemOk(raw: String, min: Int, max: Int, names: Map[String, Int]): Boolean =
    raw.split("/", -1).toList match
      case List("*")           => true
      case List("*", step)     => positive(step)
      case List(body)          => rangeOk(body, min, max, names)
      case List(body, step)    => rangeOk(body, min, max, names) && positive(step)
      case _                   => false

  private def rangeOk(raw: String, min: Int, max: Int, names: Map[String, Int]): Boolean =
    raw.split("-", -1).toList match
      case List(one) => bound(one, min, max, names).isDefined
      case List(start, end) =>
        (bound(start, min, max, names), bound(end, min, max, names)) match
          case (Some(lower), Some(upper)) => lower <= upper
          case _                          => false
      case _ => false

  private def bound(raw: String, min: Int, max: Int, names: Map[String, Int]): Option[Int] =
    names.get(raw.toUpperCase(Locale.ROOT)).orElse(integer(raw)).filter(value => value >= min && value <= max)

  private def integer(raw: String): Option[Int] =
    if raw.nonEmpty && raw.forall(_.isDigit) then raw.toIntOption else None

  private def positive(raw: String): Boolean =
    integer(raw).exists(_ >= 1)

enum CronError:
  case Empty
  case Fields(count: Int)
  case Field(index: Int, raw: String)

  def message: String = this match
    case Empty              => "a cron expression is not empty"
    case Fields(count)      => s"a cron expression has five fields, and this one has $count"
    case Field(index, raw)  => s"cron field ${index + 1} is not a GitHub cron field: '$raw'"
