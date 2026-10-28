package histos

import _root_.scala.quoted.*

private object ScalaVersionMacro:
  def literal(raw: Expr[String])(using Quotes): Expr[ScalaVersion] =
    import quotes.reflect.report
    raw.value match
      case None    => report.errorAndAbort("ScalaVersion(...) takes a literal. Use ScalaVersion.from at runtime.", raw)
      case Some(s) =>
        ScalaVersion.from(s) match
          case Left(err) => report.errorAndAbort(err.message, raw)
          case Right(_)  => '{ ScalaVersion.trusted(${ Expr(s) }) }

private object DepMacro:
  def literal(raw: Expr[String])(using Quotes): Expr[Dep] =
    import quotes.reflect.report
    raw.value match
      case None    => report.errorAndAbort("Dep(...) takes a literal. Use Dep.from at runtime.", raw)
      case Some(s) =>
        Dep.from(s) match
          case Left(err)  => report.errorAndAbort(err.message, raw)
          case Right(dep) =>
            val kind = kindExpr(dep.kind)
            '{ Dep.trusted(${ Expr(dep.organization) }, ${ Expr(dep.name) }, ${ Expr(dep.revision) }, $kind) }

  private def kindExpr(kind: DepKind)(using Quotes): Expr[DepKind] =
    kind match
      case DepKind.Java   => '{ DepKind.Java }
      case DepKind.Binary => '{ DepKind.Binary }
      case DepKind.Full   => '{ DepKind.Full }
end DepMacro

private object NativeVersionMacro:
  def literal(raw: Expr[String])(using Quotes): Expr[NativeVersion] =
    import quotes.reflect.report
    raw.value match
      case None => report.errorAndAbort("NativeVersion(...) takes a literal. Use NativeVersion.from at runtime.", raw)
      case Some(text) =>
        NativeVersion.from(text) match
          case Left(err) => report.errorAndAbort(err.message, raw)
          case Right(_)  => '{ NativeVersion.trusted(${ Expr(text) }) }

private object FrameworkClassMacro:
  def literal(raw: Expr[String])(using Quotes): Expr[FrameworkClass] =
    import quotes.reflect.report
    raw.value match
      case None => report.errorAndAbort("FrameworkClass(...) takes a literal. Use FrameworkClass.from at runtime.", raw)
      case Some(text) =>
        FrameworkClass.from(text) match
          case Left(err) => report.errorAndAbort(err.message, raw)
          case Right(_)  => '{ FrameworkClass.trusted(${ Expr(text) }) }

private object MainClassMacro:
  def literal(raw: Expr[String])(using Quotes): Expr[MainClass] =
    import quotes.reflect.report
    raw.value match
      case None       => report.errorAndAbort("MainClass(...) takes a literal. Use MainClass.from at runtime.", raw)
      case Some(text) =>
        MainClass.from(text) match
          case Left(err) => report.errorAndAbort(err.message, raw)
          case Right(_)  => '{ MainClass.trusted(${ Expr(text) }) }

private object TestMainMacro:
  def literal(raw: Expr[String])(using Quotes): Expr[TestMain] =
    import quotes.reflect.report
    raw.value match
      case None       => report.errorAndAbort("TestMain(...) takes a literal. Use TestMain.from at runtime.", raw)
      case Some(text) =>
        TestMain.from(text) match
          case Left(err) => report.errorAndAbort(err.message, raw)
          case Right(_)  => '{ TestMain.trusted(${ Expr(text) }) }

private object RepositoryUrlMacro:
  def literal(raw: Expr[String])(using Quotes): Expr[RepositoryUrl] =
    import quotes.reflect.report
    raw.value match
      case None => report.errorAndAbort("RepositoryUrl(...) takes a literal. Use RepositoryUrl.from at runtime.", raw)
      case Some(text) =>
        RepositoryUrl.from(text) match
          case Left(err) => report.errorAndAbort(err.message, raw)
          case Right(_)  => '{ RepositoryUrl.trusted(${ Expr(text) }) }

private object ArtifactBaseMacro:
  def literal(raw: Expr[String])(using Quotes): Expr[ArtifactBase] =
    import quotes.reflect.report
    raw.value match
      case None => report.errorAndAbort("ArtifactBase(...) takes a literal. Use ArtifactBase.from at runtime.", raw)
      case Some(text) =>
        ArtifactBase.from(text) match
          case Left(err) => report.errorAndAbort(err.message, raw)
          case Right(_)  => '{ ArtifactBase.trusted(${ Expr(text) }) }

private object OrganizationMacro:
  def literal(raw: Expr[String])(using Quotes): Expr[Organization] =
    import quotes.reflect.report
    raw.value match
      case None => report.errorAndAbort("Organization(...) takes a literal. Use Organization.from at runtime.", raw)
      case Some(text) =>
        Organization.from(text) match
          case Left(err) => report.errorAndAbort(err.message, raw)
          case Right(_)  => '{ Organization.trusted(${ Expr(text) }) }

private object PomTextMacro:
  def literal(raw: Expr[String])(using Quotes): Expr[PomText] =
    import quotes.reflect.report
    raw.value match
      case None       => report.errorAndAbort("PomText(...) takes a literal. Use PomText.from at runtime.", raw)
      case Some(text) =>
        PomText.from(text) match
          case Left(err) => report.errorAndAbort(err.message, raw)
          case Right(_)  => '{ PomText.trusted(${ Expr(text) }) }

private object HttpUrlMacro:
  def literal(raw: Expr[String])(using Quotes): Expr[HttpUrl] =
    import quotes.reflect.report
    raw.value match
      case None       => report.errorAndAbort("HttpUrl(...) takes a literal. Use HttpUrl.from at runtime.", raw)
      case Some(text) =>
        HttpUrl.from(text) match
          case Left(err) => report.errorAndAbort(err.message, raw)
          case Right(_)  => '{ HttpUrl.trusted(${ Expr(text) }) }

private object SbtSeriesMacro:
  def literal(raw: Expr[String])(using Quotes): Expr[SbtSeries] =
    import quotes.reflect.report
    raw.value match
      case None       => report.errorAndAbort("SbtSeries(...) takes a literal. Use SbtSeries.from at runtime.", raw)
      case Some(text) =>
        SbtSeries.from(text) match
          case Left(err) => report.errorAndAbort(err.message, raw)
          case Right(_)  => '{ SbtSeries.trusted(${ Expr(text) }) }

private object LicenseNameMacro:
  def literal(raw: Expr[String])(using Quotes): Expr[LicenseName] =
    import quotes.reflect.report
    raw.value match
      case None       => report.errorAndAbort("LicenseName(...) takes a literal. Use LicenseName.from at runtime.", raw)
      case Some(text) =>
        LicenseName.from(text) match
          case Left(err) => report.errorAndAbort(err.message, raw)
          case Right(_)  => '{ LicenseName.trusted(${ Expr(text) }) }

private object DeveloperIdMacro:
  def literal(raw: Expr[String])(using Quotes): Expr[DeveloperId] =
    import quotes.reflect.report
    raw.value match
      case None       => report.errorAndAbort("DeveloperId(...) takes a literal. Use DeveloperId.from at runtime.", raw)
      case Some(text) =>
        DeveloperId.from(text) match
          case Left(err) => report.errorAndAbort(err.message, raw)
          case Right(_)  => '{ DeveloperId.trusted(${ Expr(text) }) }

private object ScmConnectionMacro:
  def literal(raw: Expr[String])(using Quotes): Expr[ScmConnection] =
    import quotes.reflect.report
    raw.value match
      case None =>
        report.errorAndAbort("ScmConnection(...) takes a literal. Use ScmConnection.from at runtime.", raw)
      case Some(text) =>
        ScmConnection.from(text) match
          case Left(err) => report.errorAndAbort(err.message, raw)
          case Right(_)  => '{ ScmConnection.trusted(${ Expr(text) }) }

private object PromoteMacro:
  def literal(raw: Expr[String])(using Quotes): Expr[Promote] =
    import quotes.reflect.report
    raw.value match
      case None =>
        report.errorAndAbort("promote\"...\" takes a literal. Use Promote.from at runtime.", raw)
      case Some(text) =>
        Promote.from(text) match
          case Left(err)                => report.errorAndAbort(err.message, raw)
          case Right(Promote.Rebuild)   => '{ Promote.Rebuild }
          case Right(Promote.CopyIndex) => '{ Promote.CopyIndex }

private object LiteralMacro:
  def scalaVersion(sc: Expr[StringContext], args: Expr[Seq[String]])(using Quotes): Expr[ScalaVersion] =
    onePart(sc, args, "scala").map(raw => ScalaVersionMacro.literal(Expr(raw))) match
      case Left(err) =>
        import quotes.reflect.report
        report.errorAndAbort(err)
      case Right(out) => out

  def dep(sc: Expr[StringContext], args: Expr[Seq[String]])(using Quotes): Expr[Dep] =
    onePart(sc, args, "dep") match
      case Left(err) =>
        import quotes.reflect.report
        report.errorAndAbort(err)
      case Right(raw) => DepMacro.literal(Expr(raw))

  def declaredVersion(sc: Expr[StringContext], args: Expr[Seq[String]])(using
      Quotes
  ): Expr[histos.version.ManualVersion] =
    onePart(sc, args, "manual") match
      case Left(err) =>
        import quotes.reflect.report
        report.errorAndAbort(err)
      case Right(raw) => histos.version.ManualVersionMacro.literal(Expr(raw))

  def maven(sc: Expr[StringContext], args: Expr[Seq[String]])(using Quotes): Expr[Resolver] =
    onePart(sc, args, "maven") match
      case Left(err) =>
        import quotes.reflect.report
        report.errorAndAbort(err)
      case Right(raw) =>
        val url = RepositoryUrlMacro.literal(Expr(raw))
        '{ Resolver.Maven($url) }

  def artifact(sc: Expr[StringContext], args: Expr[Seq[String]])(using Quotes): Expr[ArtifactBase] =
    onePart(sc, args, "artifact") match
      case Left(err) =>
        import quotes.reflect.report
        report.errorAndAbort(err)
      case Right(raw) => ArtifactBaseMacro.literal(Expr(raw))

  def organization(sc: Expr[StringContext], args: Expr[Seq[String]])(using Quotes): Expr[Organization] =
    onePart(sc, args, "organization") match
      case Left(err) =>
        import quotes.reflect.report
        report.errorAndAbort(err)
      case Right(raw) => OrganizationMacro.literal(Expr(raw))

  def projectPath(sc: Expr[StringContext], args: Expr[Seq[String]])(using Quotes): Expr[ProjectPath] =
    onePart(sc, args, "path") match
      case Left(err) =>
        import quotes.reflect.report
        report.errorAndAbort(err)
      case Right(raw) => ProjectPathMacro.literal(Expr(raw))

  def asset(sc: Expr[StringContext], args: Expr[Seq[String]])(using Quotes): Expr[histos.plugin.RepoPath] =
    onePart(sc, args, "asset") match
      case Left(err) =>
        import quotes.reflect.report
        report.errorAndAbort(err)
      case Right(raw) => histos.plugin.PluginMacros.repoPath(Expr(raw))

  def shipNumber(sc: Expr[StringContext], args: Expr[Seq[String]])(using Quotes): Expr[histos.version.ShipNumber] =
    onePart(sc, args, "ship") match
      case Left(err) =>
        import quotes.reflect.report
        report.errorAndAbort(err)
      case Right(raw) => histos.version.ShipNumberMacro.literal(Expr(raw))

  def pomText(sc: Expr[StringContext], args: Expr[Seq[String]])(using Quotes): Expr[PomText] =
    onePart(sc, args, "pom") match
      case Left(err) =>
        import quotes.reflect.report
        report.errorAndAbort(err)
      case Right(raw) => PomTextMacro.literal(Expr(raw))

  def httpUrl(sc: Expr[StringContext], args: Expr[Seq[String]])(using Quotes): Expr[HttpUrl] =
    onePart(sc, args, "url") match
      case Left(err) =>
        import quotes.reflect.report
        report.errorAndAbort(err)
      case Right(raw) => HttpUrlMacro.literal(Expr(raw))

  def sbtSeries(sc: Expr[StringContext], args: Expr[Seq[String]])(using Quotes): Expr[SbtSeries] =
    onePart(sc, args, "sbt") match
      case Left(err) =>
        import quotes.reflect.report
        report.errorAndAbort(err)
      case Right(raw) => SbtSeriesMacro.literal(Expr(raw))

  def licenseName(sc: Expr[StringContext], args: Expr[Seq[String]])(using Quotes): Expr[LicenseName] =
    onePart(sc, args, "license") match
      case Left(err) =>
        import quotes.reflect.report
        report.errorAndAbort(err)
      case Right(raw) => LicenseNameMacro.literal(Expr(raw))

  def developerId(sc: Expr[StringContext], args: Expr[Seq[String]])(using Quotes): Expr[DeveloperId] =
    onePart(sc, args, "developer") match
      case Left(err) =>
        import quotes.reflect.report
        report.errorAndAbort(err)
      case Right(raw) => DeveloperIdMacro.literal(Expr(raw))

  def scmConnection(sc: Expr[StringContext], args: Expr[Seq[String]])(using Quotes): Expr[ScmConnection] =
    onePart(sc, args, "connection") match
      case Left(err) =>
        import quotes.reflect.report
        report.errorAndAbort(err)
      case Right(raw) => ScmConnectionMacro.literal(Expr(raw))

  def promote(sc: Expr[StringContext], args: Expr[Seq[String]])(using Quotes): Expr[Promote] =
    onePart(sc, args, "promote") match
      case Left(err) =>
        import quotes.reflect.report
        report.errorAndAbort(err)
      case Right(raw) => PromoteMacro.literal(Expr(raw))

  private def onePart(sc: Expr[StringContext], args: Expr[Seq[String]], what: String)(using
      Quotes
  ): Either[String, String] =
    import quotes.reflect.*
    def literal(term: Term): Option[String] =
      term match
        case Inlined(_, _, inner)                                                      => literal(inner)
        case Apply(_, List(Typed(Repeated(List(Literal(StringConstant(raw))), _), _))) => Some(raw)
        case _                                                                         => None
    args match
      case Varargs(Nil) =>
        literal(sc.asTerm) match
          case Some(raw) => Right(raw)
          case None      => Left(s"""$what"..." takes a literal""")
      case _ => Left(s"""$what"..." takes no arguments""")
end LiteralMacro

private object ProjectPathMacro:
  def literal(raw: Expr[String])(using Quotes): Expr[ProjectPath] =
    import quotes.reflect.report
    raw.value match
      case None       => report.errorAndAbort("ProjectPath(...) takes a literal. Use ProjectPath.from at runtime.", raw)
      case Some(text) =>
        ProjectPath.from(text) match
          case Left(err) => report.errorAndAbort(err.message, raw)
          case Right(_)  => '{ ProjectPath.trusted(${ Expr(text) }) }

private object ClangMajorMacro:
  def literal(raw: Expr[Int])(using Quotes): Expr[ClangMajor] =
    import quotes.reflect.report
    raw.value match
      case None    => report.errorAndAbort("ClangMajor(...) takes a literal. Use ClangMajor.from at runtime.", raw)
      case Some(n) =>
        ClangMajor.from(n) match
          case Left(err) => report.errorAndAbort(err.message, raw)
          case Right(_)  => '{ ClangMajor.trusted(${ Expr(n) }) }

private object BuilderMacro:
  def module(
      name: Expr[String],
      platform: Expr[Platform],
      platforms: Expr[Seq[Platform]],
      version: Expr[ScalaVersion],
      versions: Expr[Seq[ScalaVersion]],
  )(using Quotes): Expr[ModuleDef[?]] =
    import quotes.reflect.*
    val at    = pos
    val union = unite(distinct(read(platform) :: readAll(platforms)))
    union.asType match
      case '[p] =>
        // Expr is invariant. The cast is the wrapper, so the tree keeps `p`.
        '{
          ModuleDef.make[p](
            $name,
            Platforms($platform, $platforms.toList),
            ScalaVersions($version, $versions.toList),
            $at,
          )
        }.asInstanceOf[Expr[ModuleDef[?]]]

  /** First-seen order. A repeated case stays in the runtime list and is still [[BuildError.DuplicatePlatform]]. */
  private def distinct(using Quotes)(parts: List[quotes.reflect.TypeRepr]): List[quotes.reflect.TypeRepr] =
    parts.foldLeft(List.empty[quotes.reflect.TypeRepr]): (seen, part) =>
      if seen.exists(_ =:= part) then seen else seen :+ part

  private def unite(using Quotes)(parts: List[quotes.reflect.TypeRepr]): quotes.reflect.TypeRepr =
    import quotes.reflect.*
    parts match
      case first :: rest => rest.foldLeft(first)((left, right) => OrType(left, right))
      case Nil           => report.errorAndAbort("module takes Jvm, Js, or Native.")

  private def readAll(platforms: Expr[Seq[Platform]])(using Quotes): List[quotes.reflect.TypeRepr] =
    import quotes.reflect.*
    def loop(term: Term): List[TypeRepr] =
      term match
        case Inlined(_, _, inner)                         => loop(inner)
        case Typed(inner, _)                              => loop(inner)
        case Repeated(elems, _)                           => elems.map(platformType)
        case Apply(_, List(Typed(Repeated(elems, _), _))) => elems.map(platformType)
        case other                                        =>
          report.errorAndAbort("module takes Jvm, Js, or Native.", other.pos)
    platforms match
      case Varargs(args) => args.map(read).toList
      case other         => loop(other.asTerm)

  private def read(platform: Expr[Platform])(using Quotes): quotes.reflect.TypeRepr =
    import quotes.reflect.*
    def loop(term: Term): TypeRepr =
      term match
        case Inlined(_, _, inner) => loop(inner)
        case Typed(inner, _)      => loop(inner)
        case other                => platformType(other)
    loop(platform.asTerm)

  /** An enum case, or the package export whose result type is that case. Anything else is not a platform. */
  private def platformType(using Quotes)(term: quotes.reflect.Term): quotes.reflect.TypeRepr =
    import quotes.reflect.*
    val found = term.symbol.tree match
      case defn: DefDef => defn.returnTpt.tpe
      case _            => term.tpe
    if found =:= TypeRepr.of[Platform.Jvm.type] then TypeRepr.of[Platform.Jvm.type]
    else if found =:= TypeRepr.of[Platform.Js.type] then TypeRepr.of[Platform.Js.type]
    else if found =:= TypeRepr.of[Platform.Native.type] then TypeRepr.of[Platform.Native.type]
    else report.errorAndAbort("module takes Jvm, Js, or Native.", term.pos)

  def aggregate(
      name: Expr[String],
      project: Expr[ModuleDef[?]],
      projects: Expr[Seq[ModuleDef[?]]],
  )(using Quotes): Expr[AggregateDef] =
    val at = pos
    '{ AggregateDef.make($name, $project :: $projects.toList, $at) }

  def ship(
      name: Expr[String],
      number: Expr[histos.version.ShipNumber],
      member: Expr[ProjectDef],
      members: Expr[Seq[ProjectDef]],
  )(using Quotes): Expr[ShipDef] =
    val at = pos
    '{ ShipDef.line($name, $number, ::($member, $members.toList), $at) }

  def fixed(
      name: Expr[String],
      version: Expr[histos.version.ManualVersion],
      member: Expr[ProjectDef],
      members: Expr[Seq[ProjectDef]],
  )(using Quotes): Expr[ShipDef] =
    val at = pos
    '{ ShipDef.manual($name, $version, ::($member, $members.toList), $at) }

  private def pos(using Quotes): Expr[SourcePos] =
    import quotes.reflect.Position
    val at = Position.ofMacroExpansion
    '{ SourcePos(${ Expr(at.sourceFile.path) }, ${ Expr(at.startLine + 1) }, ${ Expr(at.startColumn + 1) }) }
end BuilderMacro
