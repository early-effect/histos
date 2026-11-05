package histos.doctor

/** Names within edit distance `limit`. Zipx uses 3. Histos uses 2. An exact match is not a suggestion. */
object Suggest:
  def near(typed: String, known: Iterable[String], limit: Int = 2): List[String] =
    val needle = fold(typed)
    if needle.isEmpty then Nil
    else
      known.toList
        .flatMap: name =>
          val folded = fold(name)
          if folded.isEmpty || folded == needle then None
          else distance(needle, folded, limit).map(score => (score, name))
        .sortBy((score, name) => (score, name))
        .map(_._2)
        .distinct
        .take(3)

  def closest(typed: String, known: Iterable[String], limit: Int = 2): Option[String] =
    near(typed, known, limit).headOption

  /** Module names plus the built-in run names. */
  def runs(typed: String, projects: Iterable[String]): List[String] =
    near(typed, projects ++ List("inspect", "versions", "doc"))

  /** A coordinate found with one colon may have been meant as a Scala binary. */
  def binary(organization: String, name: String, revision: String): Option[String] =
    if name.contains("_") then None
    else
      Some(
        s"If this is a Scala library, write $organization::$name:$revision. ':' does not add the Scala suffix."
      )

  private def fold(raw: String): String =
    raw.toLowerCase(java.util.Locale.ROOT)

  private def distance(left: String, right: String, limit: Int): Option[Int] =
    if (left.length - right.length).abs > limit then None
    else
      val score = levenshtein(left, right)
      if score == 0 || score > limit then None else Some(score)

  private def levenshtein(left: String, right: String): Int =
    val start = (0 to right.length).toArray
    left.indices
      .foldLeft(start): (prev, i) =>
        val next = Array.ofDim[Int](right.length + 1)
        next(0) = i + 1
        right.indices.foreach: j =>
          val cost = if left.charAt(i) == right.charAt(j) then 0 else 1
          next(j + 1) = (prev(j + 1) + 1).min(next(j) + 1).min(prev(j) + cost)
        next
      .apply(right.length)
end Suggest
