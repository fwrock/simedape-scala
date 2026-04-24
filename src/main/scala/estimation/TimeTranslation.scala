package org.interscity.htc
package estimation

import data.Models.*
import preprocessing.TimeSeriesPreprocessor

/**
 * Core time-translation algorithm.
 *
 * Direct port of the Python functions `time_translation` and `estimate`
 * from `simedape/python/src/util/estimation.py`.
 *
 * Idea
 * ────
 * The DTW warping path encodes, for each position in the target series,
 * which position in the centroid (reference) it aligns with, and the
 * accumulated cost difference between consecutive steps.
 *
 * Given the simulation point's series for the NEW scenario (s_new),
 * we apply the same deformation encoded in the warping path
 * (computed on the BASE scenario) to produce an estimate for the
 * non-simulated series.
 *
 *   estimate_j = s_new[path[pos].i] ± sqrt(cost² − prev_cost²)
 *
 * The ± depends on whether the centroid was above or below the target
 * at that alignment point (the `descending` flag in each WarpingStep).
 */
object TimeTranslation:

  /**
   * Apply the warping path to produce an estimated series.
   *
   * @param path  warping path (centroid → target in base scenario)
   * @param sNew  simulation point series from the NEW scenario (normalised)
   * @return estimated series for the target (normalised space)
   */
  def translate(path: WarpingPath, sNew: Array[Double]): Array[Double] =
    if path.isEmpty then return Array.fill(sNew.length)(0.0)

    val result = new Array[Double](sNew.length)

    // First step
    val t0 = path.head
    result(t0.j) =
      if t0.descending then sNew(t0.i) - t0.cost
      else                   sNew(t0.i) + t0.cost

    // Remaining steps — use incremental cost (Pythagorean difference)
    for pos <- 1 until path.length do
      val t    = path(pos)
      val prev = path(pos - 1)
      val diff = math.sqrt(math.max(0.0, t.cost * t.cost - prev.cost * prev.cost))
      result(t.j) =
        if t.descending then sNew(t.i) - diff
        else                  sNew(t.i) + diff

    result

  /**
   * Translate and de-normalise in one step.
   *
   * @param path  warping path
   * @param sNew  normalised simulation point series (new scenario)
   * @param mu    target series mean (from which dataset? — caller decides)
   * @param sigma target series std
   * @return estimated series in original (count) space, pruned ≥ 0
   */
  def estimate(
      path: WarpingPath,
      sNew: Array[Double],
      mu: Double,
      sigma: Double,
  ): Array[Double] =
    val translated = translate(path, sNew)
    TimeSeriesPreprocessor.prune(TimeSeriesPreprocessor.denormalize(translated, mu, sigma))

  // ── Blending strategies for μ and σ ─────────────────────────────────────

  /** base only: use the target's statistics from the base scenario */
  def muSigmaBase(baseMu: Double, baseSigma: Double): (Double, Double) =
    (baseMu, baseSigma)

  /** avg: arithmetic average of base and partial */
  def muSigmaAvg(baseMu: Double, baseSigma: Double, partialMu: Double, partialSigma: Double): (Double, Double) =
    ((baseMu + partialMu) / 2.0, (baseSigma + partialSigma) / 2.0)

  /** wavg: weighted average, weight = percentSample / 100 */
  def muSigmaWAvg(
      baseMu: Double, baseSigma: Double,
      partialMu: Double, partialSigma: Double,
      pct: Double,
  ): (Double, Double) =
    val alpha = pct / 100.0
    (
      (baseMu    + partialMu    * alpha) / (1.0 + alpha),
      (baseSigma + partialSigma * alpha) / (1.0 + alpha),
    )

  /** sample: use only partial statistics (baseline — no estimation) */
  def muSigmaSample(partialMu: Double, partialSigma: Double): (Double, Double) =
    (partialMu, partialSigma)
