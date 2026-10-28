package histos

/** Where a passing `test` is recorded. Off launches the suite. Local skips when the stored suite digest still matches.
  * Remote is refused when the module names a need. The remote URI is configuration, so Remote does not upload and does
  * not skip.
  */
enum TestStore:
  case Off, Local, Remote
