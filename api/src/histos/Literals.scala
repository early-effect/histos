package histos

/** `scala"3.9.0"` and `dep"dev.zio::zio:2.1.26"`. A bad literal fails compilation of the build file. */
extension (inline sc: StringContext)
  inline def scala(inline args: Any*): ScalaVersion = ${ LiteralMacro.scalaVersion('sc, 'args) }
  inline def dep(inline args: Any*): Dep            = ${ LiteralMacro.dep('sc, 'args) }
