package histos

/** Which publishes of a ship link the native client for every install platform. A release always does. A snapshot does
  * only under `ReleasesAndSnapshots`; otherwise its manifest names no client and the launcher runs `java -jar`.
  */
enum ClientBuilds:
  case Releases, ReleasesAndSnapshots
