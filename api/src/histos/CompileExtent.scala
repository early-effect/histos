package histos

/** Which sources one `compile` writes. `MainAndTest` compiles test sources after the main sources they import. It does
  * not run suites and it does not link.
  */
enum CompileExtent:
  case Main
  case MainAndTest
