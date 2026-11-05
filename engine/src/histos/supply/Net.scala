package histos.supply

import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.nio.charset.StandardCharsets
import java.time.Duration

import zio.{IO, ZIO}

/** One HTTP exchange. Tests supply a [[Net]] that never opens a socket. */
final case class Exchange(method: String, url: String, body: String)

final case class Reply(status: Int, body: String)

trait Net:
  def send(
      method: String,
      url: String,
      body: String,
      headers: List[(String, String)],
  ): IO[SupplyFault, Reply]

object Net:
  def live: Net = Live

  private object Live extends Net:
    private val client = HttpClient.newBuilder().connectTimeout(Duration.ofSeconds(10)).build()

    def send(
        method: String,
        url: String,
        body: String,
        headers: List[(String, String)],
    ): IO[SupplyFault, Reply] =
      ZIO
        .attempt:
          val builder = HttpRequest.newBuilder(URI.create(url)).timeout(Duration.ofSeconds(20))
          headers.foreach: (name, value) =>
            builder.header(name, value)
          val publisher =
            if body.isEmpty && (method == "GET" || method == "DELETE") then HttpRequest.BodyPublishers.noBody()
            else HttpRequest.BodyPublishers.ofString(body, StandardCharsets.UTF_8)
          builder.method(method, publisher)
          val response = client.send(builder.build(), HttpResponse.BodyHandlers.ofString(StandardCharsets.UTF_8))
          Reply(response.statusCode(), response.body())
        .mapError: _ =>
          SupplyFault.ServiceDown(SourceName.trusted("net"), OutageStrategy.FailClosed)
  end Live
end Net

/** Pull quoted JSON strings and integers out of a document. The engine does not depend on a JSON library. */
object Scan:
  def strings(body: String, key: String): List[String] =
    val mark = s""""$key":""""
    Iterator
      .unfold(0): at =>
        val next = body.indexOf(mark, at)
        if next < 0 then None
        else
          val start = next + mark.length
          val end   = body.indexOf('"', start)
          if end < 0 then None else Some((body.substring(start, end), end + 1))
      .toList

  def longs(body: String, key: String): List[Long] =
    val mark = s""""$key":"""
    Iterator
      .unfold(0): at =>
        val next = body.indexOf(mark, at)
        if next < 0 then None
        else
          val start = skipSpace(body, next + mark.length)
          val end   = start + body.drop(start).takeWhile(_.isDigit).length
          body.substring(start, end).toLongOption match
            case Some(value) => Some((value, end))
            case None        => None
      .toList

  def bool(body: String, key: String): Option[Boolean] =
    val mark = s""""$key":"""
    val next = body.indexOf(mark)
    if next < 0 then None
    else
      val start = skipSpace(body, next + mark.length)
      if body.startsWith("true", start) then Some(true)
      else if body.startsWith("false", start) then Some(false)
      else None

  def array(body: String, key: String): List[String] =
    val mark = s""""$key":["""
    val next = body.indexOf(mark)
    if next < 0 then Nil
    else
      val close = body.indexOf(']', next)
      if close < 0 then Nil
      else
        body
          .substring(next + mark.length, close)
          .split(",")
          .toList
          .map(_.trim.stripPrefix("\"").stripSuffix("\""))
          .filter(_.nonEmpty)

  private def skipSpace(body: String, at: Int): Int =
    at + body.drop(at).takeWhile(_.isWhitespace).length
end Scan
