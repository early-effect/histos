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

  def shipNumber(sc: Expr[StringContext], args: Expr[Seq[String]])(using Quotes): Expr[histos.version.ShipNumber] =
    onePart(sc, args, "ship") match
      case Left(err) =>
        import quotes.reflect.report
        report.errorAndAbort(err)
      case Right(raw) => histos.version.ShipNumberMacro.literal(Expr(raw))

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
  )(using Quotes): Expr[ModuleDef] =
    val at = pos
    '{ ModuleDef.make($name, Platforms($platform, $platforms.toList), ScalaVersions($version, $versions.toList), $at) }

  def aggregate(
      name: Expr[String],
      project: Expr[ModuleDef],
      projects: Expr[Seq[ModuleDef]],
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
