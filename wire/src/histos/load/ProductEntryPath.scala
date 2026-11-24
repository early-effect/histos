package histos.load

import histos.ResourceName
import _root_.scala.annotation.publicInBinary
import _root_.scala.quoted.*

/** A relative product-tree entry, not an archive-specific filename. */
opaque type ProductEntryPath = String

object ProductEntryPath:
  given CanEqual[ProductEntryPath, ProductEntryPath] = CanEqual.derived

  inline def apply(inline raw: String): ProductEntryPath = ${ ProductEntryPathMacro.literal('raw) }

  def from(raw: String): Either[ProductError, ProductEntryPath] =
    val drive = raw.length >= 2 && raw.charAt(0).isLetter && raw.charAt(1) == ':'
    if raw.contains('\u0000') || drive then Left(ProductError.InvalidEntry(raw))
    else ResourceName.from(raw).left.map(_ => ProductError.InvalidEntry(raw)).map(name => trusted(name.value))

  extension (path: ProductEntryPath) def value: String = path

  @publicInBinary private[histos] def trusted(raw: String): ProductEntryPath = raw
end ProductEntryPath

private object ProductEntryPathMacro:
  def literal(raw: Expr[String])(using Quotes): Expr[ProductEntryPath] =
    raw.value match
      case None => quotes.reflect.report.errorAndAbort("ProductEntryPath requires a literal; use from at runtime.", raw)
      case Some(value) =>
        ProductEntryPath.from(value) match
          case Left(error) => quotes.reflect.report.errorAndAbort(error.message, raw)
          case Right(path) => '{ ProductEntryPath.trusted(${ Expr(path.value) }) }
