package histos.splice

import org.scalajs.ir.Trees
import org.scalajs.ir.Trees.JSNativeLoadSpec
import org.scalajs.linker.interface.IRFile
import org.scalajs.linker.interface.unstable.IRFileImpl

import scala.concurrent.{ExecutionContext, Future}

/** Rewrite mapped `@JSImport` load specs to `Global(__splice_*)` before link. */
object SpliceIR:

  def fromIRFile(ir: IRFile, mapped: Set[String]): IRFile =
    SpliceIRFile(IRFileImpl.fromIRFile(ir), mapped)

  def remapSpec(spec: JSNativeLoadSpec, mapped: Set[String]): JSNativeLoadSpec =
    spec match
      case JSNativeLoadSpec.Import(module, path) if mapped.contains(module) =>
        JSNativeLoadSpec.Global(JsModules.ident(module), path)
      case JSNativeLoadSpec.ImportWithGlobalFallback(imp, glob) =>
        remapSpec(imp, mapped) match
          case g: JSNativeLoadSpec.Global            => g
          case JSNativeLoadSpec.Import(module, path) =>
            JSNativeLoadSpec.ImportWithGlobalFallback(
              JSNativeLoadSpec.Import(module, path),
              glob,
            )
          case other => other
      case other => other

  private final class SpliceIRFile(impl: IRFileImpl, mapped: Set[String]) extends IRFileImpl(impl.path, impl.version):

    def entryPointsInfo(implicit ec: ExecutionContext) =
      impl.entryPointsInfo

    def tree(implicit ec: ExecutionContext): Future[Trees.ClassDef] =
      impl.tree.map { classDef =>
        if classDef.jsNativeLoadSpec.isDefined || classDef.jsNativeMembers.nonEmpty then
          Trees.ClassDef(
            classDef.name,
            classDef.originalName,
            classDef.kind,
            classDef.jsClassCaptures,
            classDef.superClass,
            classDef.interfaces,
            classDef.jsSuperClass,
            classDef.jsNativeLoadSpec.map(remapSpec(_, mapped)),
            classDef.fields,
            classDef.methods,
            classDef.jsConstructor,
            classDef.jsMethodProps,
            classDef.jsNativeMembers.map(transformMember),
            classDef.topLevelExportDefs,
          )(classDef.optimizerHints)(using classDef.pos)
        else classDef
      }

    private def transformMember(member: Trees.JSNativeMemberDef): Trees.JSNativeMemberDef =
      Trees.JSNativeMemberDef(
        member.flags,
        member.name,
        remapSpec(member.jsNativeLoadSpec, mapped),
      )(using member.pos)
  end SpliceIRFile
end SpliceIR
