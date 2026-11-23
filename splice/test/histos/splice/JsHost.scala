package histos.splice

import org.graalvm.polyglot.{Context, HostAccess, Value}

/** JVM-hosted JS eval for tests. Not a plugin feature; not Node. It runs a splice's output as written, as the classic
  * script a page loads, so module syntax left in it is a SyntaxError here too.
  */
object JsHost:

  private val documentShim =
    """var document = {
      |  getElementById: function(id) {
      |    this._n = this._n || {};
      |    this._n[id] = this._n[id] || { textContent: "" };
      |    return this._n[id];
      |  }
      |};
      |""".stripMargin

  def evalExpr(js: String, expr: String): String =
    withContext { ctx =>
      ctx.eval("js", documentShim + js)
      stringify(ctx.eval("js", expr), expr)
    }

  private def withContext[A](f: Context => A): A =
    val ctx = Context
      .newBuilder("js")
      .option("engine.WarnInterpreterOnly", "false")
      .allowHostAccess(HostAccess.NONE)
      .build()
    try f(ctx)
    finally ctx.close()

  private def stringify(value: Value, label: String): String =
    if value == null || value.isNull then sys.error(s"missing JS value: $label")
    else if value.isString then value.asString
    else value.toString
end JsHost
