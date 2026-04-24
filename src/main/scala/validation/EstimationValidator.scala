package org.interscity.htc
package validation

import org.apache.pekko.actor.typed.*
import org.apache.pekko.actor.typed.scaladsl.*

import scala.math.*
import scala.util.Try

import core.SimEDaPEConfig
import core.Protocol.*
import data.Models.*
import warping.DTWComputer

/**
 * Stateless actor that computes error metrics between estimated and reference series.
 *
 * Receives a single RunValidation message, computes all metrics synchronously
 * (CPU-bound, runs on compute-dispatcher), then replies and stays alive for the
 * next pipeline run.
 *
 * Metrics per link:
 *  - MAE   : Mean Absolute Error
 *  - RMSE  : Root Mean Squared Error
 *  - MAPE  : Mean Absolute Percentage Error (%)
 *  - DTW   : DTW distance (Sakoe-Chiba, same window as estimation)
 *  - r     : Pearson correlation coefficient  (±1)
 */
object EstimationValidator:

  def apply(cfg: SimEDaPEConfig): Behavior[ValidatorCommand] =
    Behaviors
      .receive[ValidatorCommand] { (ctx, msg) =>
        msg match
          case RunValidation(estimated, referenceValues, referenceSimId, mode, replyTo) =>
            ctx.log.info(
              "Validating {} links against '{}' (mode={})",
              estimated.size, referenceSimId, mode,
            )
            val result = Try(
              compute(estimated, referenceValues, referenceSimId, mode, cfg.dtwWindowFraction)
            )
            result.fold(
              err => replyTo ! ValidationFailed(err),
              r   => replyTo ! ValidationDone(r),
            )
            Behaviors.same
      }

  // ── Core computation (pure) ────────────────────────────────────────────────

  private def compute(
      estimated:      Map[String, Array[Double]],
      reference:      Map[String, Array[Double]],
      refSimId:       String,
      mode:           String,
      windowFraction: Double,
  ): ValidationResult =
    val commonKeys = estimated.keySet.intersect(reference.keySet).toSeq.sorted
    val perLink = commonKeys.map { key =>
      key -> linkMetrics(estimated(key), reference(key), key, windowFraction)
    }.toMap
    ValidationResult(mode, refSimId, perLink, aggregate(perLink.values.toSeq))

  private def linkMetrics(
      est:            Array[Double],
      ref:            Array[Double],
      key:            String,
      windowFraction: Double,
  ): LinkValidationMetrics =
    val n   = min(est.length, ref.length)
    val e   = est.take(n)
    val r   = ref.take(n)
    val eps = 1e-8

    val mae  = e.zip(r).map { case (a, b) => abs(a - b) }.sum / n
    val rmse = sqrt(e.zip(r).map { case (a, b) => (a - b) * (a - b) }.sum / n)
    val mape = e.zip(r).map { case (a, b) => abs(a - b) / max(abs(b), eps) }.sum / n * 100.0
    val (_, dtwCost) = DTWComputer.warpingPath(e, r, windowFraction)
    val pr   = pearsonR(e, r)

    LinkValidationMetrics(key, mae, rmse, mape, dtwCost, pr)

  private def pearsonR(xs: Array[Double], ys: Array[Double]): Double =
    val n = xs.length
    if n == 0 then 0.0
    else
      val mx  = xs.sum / n
      val my  = ys.sum / n
      val cov = xs.zip(ys).map { case (x, y) => (x - mx) * (y - my) }.sum
      val sx  = sqrt(xs.map(x => (x - mx) * (x - mx)).sum)
      val sy  = sqrt(ys.map(y => (y - my) * (y - my)).sum)
      if sx < 1e-12 || sy < 1e-12 then 0.0 else cov / (sx * sy)

  private def aggregate(metrics: Seq[LinkValidationMetrics]): ValidationAggregateMetrics =
    if metrics.isEmpty then
      ValidationAggregateMetrics(0, 0, 0, 0, 0, 0)
    else
      val n = metrics.size
      ValidationAggregateMetrics(
        meanMAE      = metrics.map(_.mae).sum / n,
        meanRMSE     = metrics.map(_.rmse).sum / n,
        meanMAPE     = metrics.map(_.mape).sum / n,
        meanDTW      = metrics.map(_.dtwDistance).sum / n,
        meanPearsonR = metrics.map(_.pearsonR).sum / n,
        nLinks       = n,
      )
