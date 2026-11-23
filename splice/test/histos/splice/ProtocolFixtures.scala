package histos.splice

/** Vendor + Scala.js-shaped `class extends $superClass` fixtures for full-opt protocol tests. Method names are
  * unquoted, matching Scala.js (quoted names hid the rename hole).
  */
private[splice] object ProtocolFixtures:

  val DeadCodeMarker: String = "DEAD_CODE_MARKER"
  val LongLocal: String      = "veryLongLocalBindingName"

  /** Preact/React-shaped Component: mount calls willMount, render, didMount. Dead inner fn for DCE. */
  val classComponentVendor: String =
    s"""const __splice_foo = (() => {
       |  const module = { exports: {} };
       |  const exports = module.exports;
       |  function Component() {
       |    this.props = {};
       |    this.state = {};
       |    this.context = {};
       |  }
       |  Component.prototype.setState = function (update) {
       |    this.state = update;
       |    this._recorded = (this._recorded || "") + "STATE:" + JSON.stringify(update);
       |  };
       |  Component.prototype.forceUpdate = function () {
       |    this._recorded = (this._recorded || "") + "FORCE";
       |  };
       |  Component.prototype.render = function () { return "BASE_RENDER"; };
       |  Component.prototype.componentWillMount = function () {};
       |  Component.prototype.componentDidMount = function () { return "BASE_DID_MOUNT"; };
       |  Component.prototype.componentWillUnmount = function () {};
       |  Component.prototype.componentDidUpdate = function () {};
       |  Component.prototype.shouldComponentUpdate = function () { return true; };
       |  Component.prototype.componentWillUpdate = function () {};
       |  Component.prototype.componentWillReceiveProps = function () {};
       |  Component.prototype.getSnapshotBeforeUpdate = function () {};
       |  Component.prototype.componentDidCatch = function () {};
       |  exports.Component = Component;
       |  exports.mount = function mount(type) {
       |    var h = new type();
       |    if (h.componentWillMount) h.componentWillMount();
       |    var rendered = h.render();
       |    var did = h.componentDidMount ? h.componentDidMount() : "";
       |    return (h._recorded || "") + "|" + rendered + "|" + did;
       |  };
       |  exports.h = function h(tag) { return tag; };
       |  function unusedDead() { return "$DeadCodeMarker"; }
       |  return module.exports;
       |})();
       |""".stripMargin

  val classComponentEsm: String =
    s"""function Component() {
       |  this.props = {};
       |  this.state = {};
       |  this.context = {};
       |}
       |Component.prototype.setState = function (update) {
       |  this.state = update;
       |  this._recorded = (this._recorded || "") + "STATE:" + JSON.stringify(update);
       |};
       |Component.prototype.forceUpdate = function () {
       |  this._recorded = (this._recorded || "") + "FORCE";
       |};
       |Component.prototype.render = function () { return "BASE_RENDER"; };
       |Component.prototype.componentWillMount = function () {};
       |Component.prototype.componentDidMount = function () { return "BASE_DID_MOUNT"; };
       |Component.prototype.componentWillUnmount = function () {};
       |Component.prototype.componentDidUpdate = function () {};
       |Component.prototype.shouldComponentUpdate = function () { return true; };
       |function unusedDead() { return "$DeadCodeMarker"; }
       |function mount(type) {
       |  var h = new type();
       |  if (h.componentWillMount) h.componentWillMount();
       |  var rendered = h.render();
       |  var did = h.componentDidMount ? h.componentDidMount() : "";
       |  return (h._recorded || "") + "|" + rendered + "|" + did;
       |}
       |function h(tag) { return tag; }
       |export { Component, mount, h };
       |""".stripMargin

  /** Lazy `$a_*()` class factory with quoted methods, as Scala.js 1.22 full-opt emits. */
  val scalaJsClassSubclass: String =
    """function $s_LHelloComponent__componentWillMount(this$1) {
      |  this$1.setState({ "v": "from-will-mount" });
      |}
      |var $b_LHelloComponent;
      |function $a_LHelloComponent() {
      |  if (!$b_LHelloComponent) {
      |    $b_LHelloComponent = class $b_LHelloComponent extends __splice_foo.Component {
      |      constructor() { super(); }
      |      "componentWillMount"() { $s_LHelloComponent__componentWillMount(this); }
      |      "render"() { return "OVERRIDE_RENDER"; }
      |      "componentDidMount"() { return "OVERRIDE_DID_MOUNT"; }
      |    };
      |  }
      |  return $b_LHelloComponent;
      |}
      |document.getElementById("out").textContent = __splice_foo.mount($a_LHelloComponent());
      |""".stripMargin

  /** Scala.js full-opt runtime sits between the wrap IIFE and the lazy class factory. Closure unique-folds empty
    * lifecycle methods when that gap is large; keep this in the protocol test.
    */
  val scalaJsRuntimePadding: String =
    (0 until 600).map(i => s"function $$pad_$i(x) { return x + $i; }\n").mkString

  val classComponentOut: String = """STATE:{"v":"from-will-mount"}|OVERRIDE_RENDER|OVERRIDE_DID_MOUNT"""

  val customElementVendor: String =
    """const __splice_el = (() => {
      |  const module = { exports: {} };
      |  function HtmlElement() {}
      |  HtmlElement.prototype.connectedCallback = function () { return "BASE_CONNECTED"; };
      |  HtmlElement.prototype.disconnectedCallback = function () { return "BASE_DISCONNECTED"; };
      |  HtmlElement.prototype.attributeChangedCallback = function () { return "BASE_ATTR"; };
      |  module.exports.HtmlElement = HtmlElement;
      |  module.exports.upgrade = function upgrade(type) {
      |    return new type().connectedCallback();
      |  };
      |  return module.exports;
      |})();
      |""".stripMargin

  val customElementEsm: String =
    """function HtmlElement() {}
      |HtmlElement.prototype.connectedCallback = function () { return "BASE_CONNECTED"; };
      |HtmlElement.prototype.disconnectedCallback = function () { return "BASE_DISCONNECTED"; };
      |HtmlElement.prototype.attributeChangedCallback = function () { return "BASE_ATTR"; };
      |function upgrade(type) {
      |  return new type().connectedCallback();
      |}
      |export { HtmlElement, upgrade };
      |""".stripMargin

  val scalaJsCustomElementSubclass: String =
    """var $superClass = __splice_el.HtmlElement;
      |class HelloElement extends $superClass {
      |  constructor() { super(); }
      |  connectedCallback() { return "OVERRIDE_CONNECTED"; }
      |}
      |document.getElementById("out").textContent = __splice_el.upgrade(HelloElement);
      |""".stripMargin

  val customElementOut: String = "OVERRIDE_CONNECTED"

  val hCallLinker: String =
    """document.getElementById("out").textContent = __splice_foo.h("h1");
      |""".stripMargin

  /** Scala.js minify shape: `class extends Error`, `super()` then fields, getter-only `message` / `name` (`@JSExport`
    * on `java.lang.Throwable`). Not `Error.call(this); this.message = …`.
    */
  val errorSubclass: String =
    """class $c_jl_Throwable extends Error {
      |  constructor(msg) { super(); this.s$1 = msg; }
      |  get message() { return this.s$1 == null ? "" : this.s$1; }
      |  get name() { return "java.lang.Throwable"; }
      |  "getMessage"() { return this.s$1; }
      |}
      |class $c_jl_RuntimeException extends $c_jl_Throwable {
      |  constructor(msg) { super(msg); }
      |}
      |document.getElementById("out").textContent = (function () {
      |  try { throw new $c_jl_RuntimeException("boom"); }
      |  catch (e) { return e.getMessage() + "|" + e.message; }
      |})();
      |""".stripMargin

  val errorSubclassOut: String = "boom|boom"

  val minifyVendor: String =
    s"""const __splice_min = (() => {
       |  const module = { exports: {} };
       |  module.exports.used = function used() {
       |    var $LongLocal = 1;
       |    return $LongLocal;
       |  };
       |  module.exports.unusedDead = function unusedDead() { return "$DeadCodeMarker"; };
       |  return module.exports;
       |})();
       |""".stripMargin

  val minifyLinker: String =
    """document.getElementById("out").textContent = String(__splice_min.used());
      |""".stripMargin
end ProtocolFixtures
