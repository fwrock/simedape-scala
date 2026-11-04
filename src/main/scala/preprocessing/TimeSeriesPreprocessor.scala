package org.interscity.htc
package preprocessing

import org.apache.pekko.actor.typed.*
import org.apache.pekko.actor.typed.scaladsl.*

import core.{SimEDaPEConfig}
import core.Protocol.*
import data.Models.*

/**
 * Normalises each time series with z-score (mean=0, std=1) per series,
 * preserving the original mean and std for later de-normalisation.
 *
 * This mirrors Python's `TimeSeriesScalerSimEDaPE`:
 *   z_i = (x_i - μ_i) / σ_i
 *
 * Constant series use a scale of 1, matching the Python implementation.
 *
 * The actor is stateless — it can handle multiple sequential requests.
 */
object TimeSeriesPreprocessor:

  def apply(cfg: SimEDaPEConfig): Behavior[PreprocessorCommand] =
    Behaviors.receiveMessage {
      case PreprocessData(dataset, replyTo) =>
        val n   = dataset.size
        val len = cfg.seriesLength

        val normalised = new Array[Array[Double]](n)
        val means      = new Array[Double](n)
        val stds       = new Array[Double](n)
        val keys       = dataset.series.map(_.key)

        for i <- 0 until n do
          val values = dataset.series(i).values
          val mu     = mean(values)
          val rawStd = std(values, mu)
          val sigma = if rawStd == 0.0 then 1.0 else rawStd
          means(i)   = mu
          stds(i)    = sigma
          normalised(i) = values.map(v => (v - mu) / sigma)

        val result = PreprocessedDataset(
          simulationId = dataset.simulationId,
          keys         = keys,
          normalised   = normalised,
          means        = means,
          stds         = stds,
        )
        replyTo ! DataPreprocessed(result)
        Behaviors.same
    }

  // ── pure math helpers ─────────────────────────────────────────────────────

  def mean(xs: Array[Double]): Double =
    if xs.isEmpty then 0.0 else xs.sum / xs.length

  def std(xs: Array[Double], mu: Double): Double =
    if xs.length < 2 then 0.0
    else math.sqrt(xs.map(x => (x - mu) * (x - mu)).sum / xs.length)

  /** Inverse z-score transform */
  def denormalize(z: Array[Double], mu: Double, sigma: Double): Array[Double] =
    z.map(_ * sigma + mu)

  /** Pruning: clip negative counts to 0 */
  def prune(xs: Array[Double]): Array[Double] =
    xs.map(v => if v < 0.0 then 0.0 else v)
