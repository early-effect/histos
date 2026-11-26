package histos

import _root_.scala.quoted.*

/** How long a generated job may run.
  *
  * [[JobTimeout.Unbounded]] is the default. It writes no `timeout-minutes`, so GitHub applies its own default of 360
  * minutes. [[JobTimeout.Minutes]] is a cap a build sets with [[JobTimeout.minutes]] or [[JobTimeout.from]].
  */
enum JobTimeout:
  case Minutes(count: Int)
  case Unbounded

object JobTimeout:
  val default: JobTimeout = Unbounded

  /** GitHub rejects a `timeout-minutes` outside this range. */
  val ceiling: Int = 360

  def admits(count: Int): Boolean =
    count >= 1 && count <= ceiling

  def from(count: Int): Either[JobTimeoutError, JobTimeout] =
    if admits(count) then Right(Minutes(count))
    else Left(JobTimeoutError(count))

  inline def minutes(inline count: Int): JobTimeout = ${ JobTimeoutMacro('count) }
end JobTimeout

final case class JobTimeoutError(count: Int):
  def message: String =
    s"a job timeout is between 1 and ${JobTimeout.ceiling} minutes, not $count"

private object JobTimeoutMacro:
  def apply(count: Expr[Int])(using Quotes): Expr[JobTimeout] =
    import quotes.reflect.report
    count.value match
      case None =>
        report.errorAndAbort(
          "JobTimeout.minutes takes a literal. Use JobTimeout.from at runtime.",
          count,
        )
      case Some(value) =>
        JobTimeout.from(value) match
          case Left(error) => report.errorAndAbort(error.message, count)
          case Right(_)    => '{ JobTimeout.Minutes(${ Expr(value) }) }
