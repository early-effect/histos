package histos.zinc

import dotty.tools.dotc.Driver
import dotty.tools.dotc.reporting.StoreReporter

import zio.{IO, ZIO}

private[histos] enum DottyError:
  case Failed

private[histos] object Dotty:
  def process(args: List[String]): IO[DottyError, StoreReporter] =
    ZIO
      .attemptBlocking:
        val reporter = new StoreReporter(null)
        new Driver().process(args.toArray, reporter)
        reporter
      .mapError(_ => DottyError.Failed)
