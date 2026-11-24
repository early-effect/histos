package histos.resolve

import histos.lock.{DeclarationDrift, LockDiff, LockTransactionError}
import histos.plugin.CommandIdentity
import histos.ProjectName

/** Which retained locators a refresh re-reads. Selection is pure. The bytes are read separately and are not fetched. */
object SnapshotRefresh:
  /** Locators whose on-disk digest differs from the retained record. */
  final case class Revalidated(changed: List[SnapshotLocator])

  def select(
      document: LockDocument,
      selectors: List[ProjectName],
  ): Either[LockTransactionError, List[SnapshotLocator]] =
    selectors match
      case Nil   => Right(document.retained)
      case names =>
        names.foldLeft[Either[LockTransactionError, List[SnapshotLocator]]](Right(Nil)): (acc, name) =>
          acc.flatMap: chosen =>
            document.retained.find(_.name.value == name.value) match
              case Some(locator) => Right(chosen :+ locator)
              case None          => Left(LockTransactionError.UnknownSelector(name))

  def selectors(command: CommandIdentity): List[ProjectName] =
    command match
      case CommandIdentity.Refresh(names) => names
      case CommandIdentity.Compile(_, _) | CommandIdentity.Test(_) | CommandIdentity.Run(_, _, _) |
          CommandIdentity.Resolve(_) | CommandIdentity.Task(_) | CommandIdentity.Format | CommandIdentity.Publish |
          CommandIdentity.Other(_) =>
        Nil

  def drift(document: LockDocument, changed: List[SnapshotLocator]): DeclarationDrift =
    if changed.isEmpty then DeclarationDrift.Same
    else
      val updated = LockDocument(document.pins, replace(document.retained, changed))
      DeclarationDrift.Changed(LockDocument.candidate(updated))

  def preview(document: LockDocument, changed: List[SnapshotLocator]): LockDiff =
    val updated = LockDocument(document.pins, replace(document.retained, changed))
    LockDocument.closureDiff(document, updated, Nil)

  def replace(retained: List[SnapshotLocator], changed: List[SnapshotLocator]): List[SnapshotLocator] =
    retained.map: locator =>
      changed.find(_.name == locator.name) match
        case Some(next) => next
        case None       => locator
end SnapshotRefresh
