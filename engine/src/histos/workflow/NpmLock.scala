package histos.workflow

/** Whether `package-lock.json` at the repository root is a regular file. A directory of that name is absent. */
enum NpmLock:
  case Present
  case Absent
