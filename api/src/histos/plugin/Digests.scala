package histos.plugin

/** SHA-256 hex. Shared by the manifest identity and the textual digest ids.
  *
  * Pure so an API native test does not link `libcrypto`. The JVM and Native results match.
  */
private[histos] object Digests:
  def sha256(bytes: Array[Byte]): String =
    digest(bytes).map(byte => f"${byte & 0xff}%02x").mkString

  def content(bytes: Array[Byte]): ContentHash =
    ContentHash.trusted(sha256(bytes))

  private val K: Array[Int] = Array(
    0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5, 0xd807aa98,
    0x12835b01, 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174, 0xe49b69c1, 0xefbe4786,
    0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da, 0x983e5152, 0xa831c66d, 0xb00327c8,
    0xbf597fc7, 0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967, 0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13,
    0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85, 0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3, 0xd192e819,
    0xd6990624, 0xf40e3585, 0x106aa070, 0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a,
    0x5b9cca4f, 0x682e6ff3, 0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7,
    0xc67178f2,
  )

  private def digest(message: Array[Byte]): Array[Byte] =
    val bitLen  = message.length.toLong * 8L
    val withOne = message.length + 1
    val zeros   = (56 - withOne % 64 + 64) % 64
    val padded  = new Array[Byte](withOne + zeros + 8)
    System.arraycopy(message, 0, padded, 0, message.length)
    padded(message.length) = 0x80.toByte
    var shift = 56
    var tail  = padded.length - 8
    while shift >= 0 do
      padded(tail) = ((bitLen >>> shift) & 0xffL).toByte
      shift -= 8
      tail += 1
    var h0     = 0x6a09e667
    var h1     = 0xbb67ae85
    var h2     = 0x3c6ef372
    var h3     = 0xa54ff53a
    var h4     = 0x510e527f
    var h5     = 0x9b05688c
    var h6     = 0x1f83d9ab
    var h7     = 0x5be0cd19
    val w      = new Array[Int](64)
    var offset = 0
    while offset < padded.length do
      var i = 0
      while i < 16 do
        val base = offset + i * 4
        w(i) = ((padded(base) & 0xff) << 24) |
          ((padded(base + 1) & 0xff) << 16) |
          ((padded(base + 2) & 0xff) << 8) |
          (padded(base + 3) & 0xff)
        i += 1
      while i < 64 do
        val s0 = rotr(w(i - 15), 7) ^ rotr(w(i - 15), 18) ^ (w(i - 15) >>> 3)
        val s1 = rotr(w(i - 2), 17) ^ rotr(w(i - 2), 19) ^ (w(i - 2) >>> 10)
        w(i) = w(i - 16) + s0 + w(i - 7) + s1
        i += 1
      var a = h0
      var b = h1
      var c = h2
      var d = h3
      var e = h4
      var f = h5
      var g = h6
      var h = h7
      i = 0
      while i < 64 do
        val s1  = rotr(e, 6) ^ rotr(e, 11) ^ rotr(e, 25)
        val ch  = (e & f) ^ (~e & g)
        val t1  = h + s1 + ch + K(i) + w(i)
        val s0  = rotr(a, 2) ^ rotr(a, 13) ^ rotr(a, 22)
        val maj = (a & b) ^ (a & c) ^ (b & c)
        val t2  = s0 + maj
        h = g
        g = f
        f = e
        e = d + t1
        d = c
        c = b
        b = a
        a = t1 + t2
        i += 1
      h0 += a
      h1 += b
      h2 += c
      h3 += d
      h4 += e
      h5 += f
      h6 += g
      h7 += h
      offset += 64
    val out = new Array[Byte](32)
    write(out, 0, h0)
    write(out, 4, h1)
    write(out, 8, h2)
    write(out, 12, h3)
    write(out, 16, h4)
    write(out, 20, h5)
    write(out, 24, h6)
    write(out, 28, h7)
    out

  private def rotr(value: Int, bits: Int): Int =
    (value >>> bits) | (value << (32 - bits))

  private def write(out: Array[Byte], at: Int, value: Int): Unit =
    out(at) = (value >>> 24).toByte
    out(at + 1) = (value >>> 16).toByte
    out(at + 2) = (value >>> 8).toByte
    out(at + 3) = value.toByte
end Digests
