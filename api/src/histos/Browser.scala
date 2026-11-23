package histos

/** A browser a generated test job installs. Firefox is the one a build gets when it names none. */
enum Browser:
  case Chromium
  case Firefox
  case WebKit

  def token: String = this match
    case Chromium => "chromium"
    case Firefox  => "firefox"
    case WebKit   => "webkit"
end Browser
