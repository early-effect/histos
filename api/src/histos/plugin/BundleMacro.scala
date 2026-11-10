package histos.plugin

import scala.quoted.*

import zio.{Tag, ZEnvironment, ZLayer}

/** Builds a `ZLayer` from the vals of a bundle object, or from the types `wire` names. */
private[histos] object BundleMacro:
  def derived[A: Type](using Quotes): Expr[Bundle[A]] =
    import quotes.reflect.*
    val symbol = TypeRepr.of[A].typeSymbol
    if !symbol.flags.is(Flags.Module) then
      report.errorAndAbort(s"${symbol.name} is not an object. `derives Bundle` applies to one object.")
    '{ new Bundle[A] {} }

  def bundle[A: Type](self: Expr[A])(using Quotes): Expr[ZLayer[Nothing, Any, Plugins]] =
    stageOf().bundle(self)

  def wire1[A <: HistosPlugin: Type](using Quotes): Expr[ZLayer[Nothing, Any, Plugins]] =
    val stage = stageOf()
    stage.finish(List(stage.bindNamed[A]))

  def wire2[A <: HistosPlugin: Type, B <: HistosPlugin: Type](using Quotes): Expr[ZLayer[Nothing, Any, Plugins]] =
    val stage = stageOf()
    stage.finish(List(stage.bindNamed[A], stage.bindNamed[B]))

  def wire3[A <: HistosPlugin: Type, B <: HistosPlugin: Type, C <: HistosPlugin: Type](using
      Quotes
  ): Expr[ZLayer[Nothing, Any, Plugins]] =
    val stage = stageOf()
    stage.finish(List(stage.bindNamed[A], stage.bindNamed[B], stage.bindNamed[C]))

  def wire4[A <: HistosPlugin: Type, B <: HistosPlugin: Type, C <: HistosPlugin: Type, D <: HistosPlugin: Type](using
      Quotes
  ): Expr[ZLayer[Nothing, Any, Plugins]] =
    val stage = stageOf()
    stage.finish(List(stage.bindNamed[A], stage.bindNamed[B], stage.bindNamed[C], stage.bindNamed[D]))

  /** One macro expansion. `TypeRepr` is path-dependent on this `Quotes`, so the helpers stay inside. */
  private final class Stage(using val quotes: Quotes):
    import quotes.reflect.*

    final class Bound(val requirement: TypeRepr, val error: TypeRepr, val output: TypeRepr, val layer: Term)

    def bundle[A: Type](self: Expr[A]): Expr[ZLayer[Nothing, Any, Plugins]] =
      val owner     = self.asTerm
      val ownerType = TypeRepr.of[A]
      val symbol    = ownerType.typeSymbol
      if !symbol.flags.is(Flags.Module) then
        report.errorAndAbort(s"${symbol.name} is not an object. `derives Bundle` applies to one object.")
      val fields = pluginFields(symbol)
      if fields.isEmpty then report.errorAndAbort(s"${symbol.name} derives Bundle and declares no plugin val.")
      finish(fields.map(field => bindVal(owner, field)))

    private def pluginFields(symbol: Symbol): List[Symbol] =
      symbol.declarations.collect:
        case field if field.isValDef && keep(field) => field

    private def keep(field: Symbol): Boolean =
      val flags = field.flags
      !flags.is(Flags.Synthetic) &&
      !flags.is(Flags.Private) &&
      !flags.is(Flags.Given) &&
      field.privateWithin.isEmpty &&
      !field.name.startsWith("$")

    private def bindVal(owner: Term, field: Symbol): Bound =
      val selected = Select(owner, field)
      val value    = selected.tpe.dealias
      layerMember(value.typeSymbol, value).map(shape => callLayer(selected, shape)) match
        case Some(bound)                                 => bound
        case None if value <:< TypeRepr.of[HistosPlugin] => succeed(selected, value)
        case None                                        =>
          abort(
            s"val ${field.name} has type ${value.show}, which is not a HistosPlugin and has no layer.",
            field,
          )

    def bindNamed[A: Type]: Bound = bindType(TypeRepr.of[A])

    def bindType(requested: TypeRepr): Bound =
      val dealias = requested.dealias
      val symbol  = dealias.typeSymbol
      if symbol.flags.is(Flags.Module) then
        val term = dealias.termSymbol
        val ref  = if term.exists then Ref(term) else Ref(symbol.companionModule)
        layerMember(symbol, dealias).map(shape => callLayer(ref, shape)).getOrElse(succeed(ref, dealias))
      else
        val companion = symbol.companionModule
        if !companion.exists then missing(dealias)
        else
          layerMember(companion, companion.typeRef)
            .map { shape =>
              if !(shape.output <:< dealias) then
                report.errorAndAbort(
                  s"${symbol.name}.layer provides ${shape.output.show}. wire[${symbol.name}] needs that type, or a subtype."
                )
              callLayer(Ref(companion), shape)
            }
            .getOrElse(missing(dealias))

    private def missing(requested: TypeRepr): Nothing =
      report.errorAndAbort(
        s"${requested.show} has no layer. Define def layer on the companion, or wire the object."
      )

    private final class Shape(val requirement: TypeRepr, val error: TypeRepr, val output: TypeRepr, val member: Symbol)

    private def layerMember(owner: Symbol, seenFrom: TypeRepr): Option[Shape] =
      val field  = owner.declaredField("layer")
      val found  = owner.declaredMethod("layer").headOption.orElse(if field.exists then Some(field) else None)
      val zlayer = TypeRepr.of[ZLayer[Any, Nothing, Any]].typeSymbol
      found.flatMap: member =>
        resultType(seenFrom.memberType(member).dealias) match
          case AppliedType(constructor, List(requirement, error, output)) if constructor.typeSymbol == zlayer =>
            Some(Shape(requirement, error, output.dealias, member))
          case _ => None

    private def resultType(tpe: TypeRepr): TypeRepr =
      tpe match
        case MethodType(_, _, result) => resultType(result)
        case ByNameType(result)       => result
        case PolyType(_, _, result)   => resultType(result)
        case other                    => other

    private def callLayer(owner: Term, shape: Shape): Bound =
      val selected = Select(owner, shape.member)
      val applied  = shape.member.paramSymss match
        case Nil       => selected
        case List(Nil) => selected.appliedToNone
        case _         =>
          abort(s"${shape.member.name} takes arguments. A plugin layer does not.", shape.member)
      Bound(shape.requirement, shape.error, shape.output, applied)

    private def succeed(value: Term, output: TypeRepr): Bound =
      output.asType match
        case '[o] =>
          val tag  = Expr.summon[Tag[o]].getOrElse(report.errorAndAbort(s"no Tag[${output.show}]"))
          val term = '{ ZLayer.succeed[o](${ value.asExprOf[o] })(using $tag) }.asTerm
          Bound(TypeRepr.of[Any], TypeRepr.of[Nothing], output, term)

    /** The splice claims this wide layer. The tree is the precise one, so a transparent inline still sees the real
      * requirement. `get` is a term, built when the service types are known: a quote cannot prove `P >:` the
      * intersection while this macro is compiled.
      */
    def finish(bounds: List[Bound]): Expr[ZLayer[Nothing, Any, Plugins]] =
      if bounds.isEmpty then report.errorAndAbort("a plugin layer names no plugin")
      val combined = bounds.reduce(join)
      val produced = and(combined.output, TypeRepr.of[Plugins])
      val body     = Lambda(
        Symbol.spliceOwner,
        MethodType(List("env"))(
          _ => List(environment(combined.output)),
          _ => environment(produced),
        ),
        {
          case (owner, (env: Term) :: Nil) =>
            assemble(env, bounds.map(_.output)).changeOwner(owner)
          case _ =>
            report.errorAndAbort("internal: plugin environment lambda")
        },
      )
      val trace = Expr.summon[zio.Trace].getOrElse(report.errorAndAbort("no Trace")).asTerm
      Select
        .unique(combined.layer, "map")
        .appliedToType(produced)
        .appliedTo(body)
        .appliedTo(trace)
        .asExprOf[ZLayer[Nothing, Any, Plugins]]

    private def environment(service: TypeRepr): TypeRepr =
      TypeRepr.of[ZEnvironment[Any]].typeSymbol.typeRef.appliedTo(service)

    private def assemble(env: Term, services: List[TypeRepr]): Term =
      val elems = services.map: service =>
        service.asType match
          case '[p] =>
            val tag = Expr.summon[Tag[p]].getOrElse(report.errorAndAbort(s"no Tag[${Type.show[p]}]")).asTerm
            Select.unique(env, "get").appliedToType(service).appliedTo(tag)
          case _ =>
            report.errorAndAbort(s"${service.show} is not a HistosPlugin")
      val empty = '{ List.empty[HistosPlugin] }.asTerm
      val list  = elems.foldRight(empty): (elem, tail) =>
        Select.unique(tail, "::").appliedToType(TypeRepr.of[HistosPlugin]).appliedTo(elem)
      val values = Select
        .unique(Ref(TypeRepr.of[zio.Chunk[HistosPlugin]].typeSymbol.companionModule), "fromIterable")
        .appliedToType(TypeRepr.of[HistosPlugin])
        .appliedTo(list)
      val plugins = Apply(
        Select.unique(Ref(TypeRepr.of[Plugins].typeSymbol.companionModule), "apply"),
        List(values),
      )
      val tag = Expr.summon[Tag[Plugins]].getOrElse(report.errorAndAbort("no Tag[Plugins]")).asTerm
      Select.unique(env, "add").appliedToType(TypeRepr.of[Plugins]).appliedTo(plugins).appliedTo(tag)

    private def join(left: Bound, right: Bound): Bound =
      val requirement = and(left.requirement, right.requirement)
      val error       = or(left.error, right.error)
      val output      = and(left.output, right.output)
      (
        left.requirement.asType,
        left.error.asType,
        left.output.asType,
        right.requirement.asType,
        right.error.asType,
        right.output.asType,
        error.asType,
      ) match
        case ('[lr], '[le], '[lo], '[rr], '[re], '[ro], _) =>
          val term = '{
            ${ left.layer.asExprOf[ZLayer[lr, le, lo]] }.++(${ right.layer.asExprOf[ZLayer[rr, re, ro]] })
          }.asTerm
          Bound(requirement, error, output, term)
        case _ =>
          report.errorAndAbort("internal: plugin layers did not reify")

    private def and(left: TypeRepr, right: TypeRepr): TypeRepr =
      if left =:= TypeRepr.of[Any] then right
      else if right =:= TypeRepr.of[Any] then left
      else AndType(left, right)

    private def abort(message: String, symbol: Symbol): Nothing =
      symbol.pos match
        case Some(at) => report.errorAndAbort(message, at)
        case None     => report.errorAndAbort(message)

    private def or(left: TypeRepr, right: TypeRepr): TypeRepr =
      if left =:= TypeRepr.of[Nothing] then right
      else if right =:= TypeRepr.of[Nothing] then left
      else OrType(left, right)
  end Stage

  private def stageOf()(using quotes: Quotes): Stage = new Stage(using quotes)
end BundleMacro
