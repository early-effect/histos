package com.example.reviewed

import histos.*
import histos.lock.*
import histos.plugin.*
import zio.{IO, ZIO, ZLayer}

enum ReviewMode:
  case Ask, Update, Local, Frozen

final case class ReviewConfig(
    local: ReviewMode = ReviewMode.Ask,
    ci: ReviewMode = ReviewMode.Frozen,
)

final class ReviewedLocks(config: ReviewConfig) extends LockStrategy:
  def decide(context: LockContext): IO[LockStrategyError, LockDecision] =
    val selected = context.execution match
      case ExecutionKind.Local => config.local
      case ExecutionKind.Ci    => config.ci
    ZIO.succeed:
      selected match
        case ReviewMode.Ask    => LockDecision.OfferRepair
        case ReviewMode.Update => LockDecision.Update
        case ReviewMode.Local  => LockDecision.LocalSolve
        case ReviewMode.Frozen => LockDecision.Frozen

object ReviewedLocks:
  def layer(config: ReviewConfig): ZLayer[Any, Nothing, LockStrategy] =
    ZLayer.succeed[LockStrategy](ReviewedLocks(config))

trait ReviewLabel:
  def value: String

final class LiveReviewLabel(val value: String) extends ReviewLabel

object ReviewLabel:
  def layer(value: String): ZLayer[Any, Nothing, ReviewLabel] =
    ZLayer.succeed[ReviewLabel](LiveReviewLabel(value))

extension [P](module: ModuleDef[P])
  def reviewed: ModuleDef[P] =
    module.note(PluginId("reviewed-locks"), NoteKey("review"), NoteText("required"))
