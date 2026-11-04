package org.interscity.htc
package warping

import data.Models.*

/**
 * Pure DTW computation — no actors, no I/O.
 *
 * Ported directly from the Python `warping_paths` + `best_path` functions
 * in `simedape/python/src/clustering/clustering.py`.
 *
 * Algorithm
 * ─────────
 * 1. Build the (r+1)×(c+1) accumulated-cost DTW matrix.
 * 2. Backtrack from (r,c) to (1,1) choosing the min-cost predecessor.
 * 3. Each step records (i, j, accumulatedCost, isDescending) where
 *    isDescending = centroid[i] > target[j].
 *
 * Constraints
 * ───────────
 * - Sakoe-Chiba band: window = floor(len(s2) * windowFraction)
 * - psi = 2 (relaxed endpoints — first/last 2 cells may be free)
 */
object DTWComputer:

  private val Inf = Double.PositiveInfinity
  private val Psi = 2

  /**
   * Compute the optimal warping path between two normalised time series.
   *
   * @param s1             centroid / reference series
   * @param s2             target series
   * @param windowFraction Sakoe-Chiba band as fraction of s2.length (default 0.10)
   * @return (WarpingPath, dtw_distance)
   */
  def warpingPath(
      s1: Array[Double],
      s2: Array[Double],
      windowFraction: Double = 0.10,
  ): (WarpingPath, Double) =
    val r      = s1.length
    val c      = s2.length
    require(r > 0 && c > 0, "DTW requires nonempty series")
    require(windowFraction >= 0.0 && windowFraction.isFinite, "Invalid DTW window fraction")
    val requested = math.floor(c * windowFraction).toInt
    val window = if requested == 0 then math.max(r, c) else requested

    val matrix = buildMatrix(s1, s2, r, c, window)
    // Mark the relaxed suffix before backtracking, just like dtaidistance.
    val dist = extractPsiDistance(matrix, r, c, window)
    val path = bestPath(matrix, s1, s2, r, c)

    (path, dist)

  // ── DTW matrix construction ───────────────────────────────────────────────

  private def buildMatrix(
      s1: Array[Double],
      s2: Array[Double],
      r: Int,
      c: Int,
      window: Int,
  ): Array[Array[Double]] =
    val dtw = Array.fill(r + 1, c + 1)(Inf)

    // psi initialisation: first `psi` rows and columns start at 0
    for k <- 0 to Psi do
      if k <= c then dtw(0)(k) = 0.0
      if k <= r then dtw(k)(0) = 0.0

    var lastUnderMax = 0
    for i <- 0 until r do
      val prevLastUnder = if lastUnderMax == -1 then Inf.toInt else lastUnderMax
      lastUnderMax = -1
      val jMin = math.max(0, i - math.max(0, r - c) - window + 1)
      val jMax = math.min(c, i + math.max(0, c - r) + window)
      for j <- jMin until jMax do
        val d = (s1(i) - s2(j)) * (s1(i) - s2(j))
        dtw(i + 1)(j + 1) = d + math.min(dtw(i)(j), math.min(dtw(i)(j + 1), dtw(i + 1)(j)))
        if dtw(i + 1)(j + 1) < Inf then lastUnderMax = j

    // Take the sqrt of all finite values (matches Python's sqrt pass)
    for i <- 0 to r do
      for j <- 0 to c do
        if dtw(i)(j) != Inf && dtw(i)(j) != -1.0 then
          dtw(i)(j) = math.sqrt(dtw(i)(j))
    dtw

  // ── Best-path backtracking ─────────────────────────────────────────────────

  private def bestPath(
      dtw: Array[Array[Double]],
      s1: Array[Double],
      s2: Array[Double],
      r: Int,
      c: Int,
  ): WarpingPath =
    val steps = scala.collection.mutable.ArrayBuffer.empty[WarpingStep]
    var i = r
    var j = c

    while i > 0 && j > 0 do
      if dtw(i)(j) != -1.0 then
        steps += WarpingStep(i - 1, j - 1, dtw(i)(j), s1(i - 1) > s2(j - 1))
      val diag = dtw(i - 1)(j - 1)
      val up   = dtw(i - 1)(j)
      val left = dtw(i)(j - 1)
      val minVal = math.min(diag, math.min(up, left))
      if minVal == diag then
        i -= 1; j -= 1
      else if minVal == up then i -= 1
      else j -= 1
    steps.reverseIterator.toList

  // ── psi distance extraction ────────────────────────────────────────────────

  private def extractPsiDistance(
      dtw: Array[Array[Double]],
      r: Int,
      c: Int,
      window: Int,
  ): Double =
    val rowStart = math.max(0, r - Psi)
    val colStart = math.max(0, c - Psi)
    val rowBest = (rowStart to r).minBy(k => dtw(k)(c))
    val colBest = (colStart to c).minBy(k => dtw(r)(k))
    if dtw(rowBest)(c) < dtw(r)(colBest) then
      val distance = dtw(rowBest)(c)
      for i <- rowBest + 1 to r do dtw(i)(c) = -1.0
      distance
    else
      val distance = dtw(r)(colBest)
      for j <- colBest + 1 to c do dtw(r)(j) = -1.0
      distance
