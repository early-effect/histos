package histos.theme

import zio.json.*

/** Wire tokens for a theme. A bad color is rejected. Derived codecs would accept an index of 999. */
object ThemeJson:
  given JsonCodec[Color] =
    JsonCodec.string.transformOrFail(
      raw => Color.fromToken(raw).left.map(_.message),
      Color.token,
    )

  given JsonCodec[Role] =
    JsonCodec.string.transformOrFail(
      raw => Role.fromToken(raw).toRight(s"'$raw' is not a role"),
      _.token,
    )

  given JsonCodec[Emphasis] =
    JsonCodec.string.transformOrFail(
      raw => Emphasis.fromToken(raw).toRight(s"'$raw' is not an emphasis"),
      _.token,
    )

  given JsonCodec[Style] = JsonCodec.derived
  given JsonCodec[Theme] = JsonCodec.derived
end ThemeJson
