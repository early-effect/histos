package histos.splice

/** Search list for remote JS, like sbt `resolvers`. First hit that verifies wins. */
enum SpliceResolver derives CanEqual:
  case Maven
  case Cdn(id: String, expand: (String, String, String) => String)
  case GitHub(expand: (String, String, String) => String)
