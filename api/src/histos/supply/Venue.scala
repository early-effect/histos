package histos.supply

/** Where doctor runs. Only Actions turns a check that could not see its fact into a failure. */
enum Venue derives CanEqual:
  case Actions, Laptop
