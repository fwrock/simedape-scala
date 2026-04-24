package org.interscity.htc
package metrics

import data.Models.*

/**
 * Traffic metrics calculation.
 *
 * Mirrors Python's `src/util/metrics.py` and `estimation.py::calc_metrics`.
 *
 * All metrics are aggregated across roads using the **geometric mean**,
 * consistent with the original Python implementation.
 *
 * Speed model: Greenshields
 * ─────────────────────────
 *   v = v_free × (1 − (n / capacity)^β)^α
 *
 * where:
 *   n        = average number of vehicles on the link
 *   capacity = link capacity
 *   v_free   = free-flow speed (m/s)
 *   α = 1.0, β = 1.0  (linear Greenshields — adjustable)
 */
object TrafficMetrics:

  private val Alpha    = 1.0
  private val Beta     = 1.0
  private val DefaultCapacity        = 1800   // vehicles/hour fallback
  private val DefaultFreeFlowSpeedMs = 13.89  // ≈ 50 km/h in m/s

  // ── Per-link metrics ──────────────────────────────────────────────────────

  /** Average number of vehicles on the link across all ticks. */
  def avgVehicles(series: Array[Double]): Double =
    if series.isEmpty then 0.0 else series.sum / series.length

  /** Road occupancy: (avg_vehicles / capacity) × 100 (%). */
  def occupancy(series: Array[Double], capacity: Int = DefaultCapacity): Double =
    val avg = avgVehicles(series)
    if capacity <= 0 then 0.0 else (avg / capacity) * 100.0

  /**
   * Average speed using the Greenshields model.
   * Returns 0.0 when occupancy ≥ 100%.
   */
  def avgSpeed(
      series: Array[Double],
      capacity: Int = DefaultCapacity,
      vFree: Double = DefaultFreeFlowSpeedMs,
  ): Double =
    val n = avgVehicles(series)
    if n <= 0 then vFree
    else
      val densityRatio = (n / capacity).min(1.0)
      val factor       = math.pow(1.0 - math.pow(densityRatio, Beta), Alpha)
      (vFree * factor).max(0.0)

  /**
   * Kilometers traveled: sum of vehicle-ticks converted to km.
   * km = Σ(n_t) × tickDurationH × linkLengthKm
   * where tickDurationH is assumed 1/3600 (one-second ticks) unless overridden.
   */
  def kmTraveled(
      series: Array[Double],
      linkLengthKm: Double,
      tickDurationH: Double = 1.0 / 3600.0,
  ): Double =
    series.sum * tickDurationH * linkLengthKm

  // ── Aggregation ───────────────────────────────────────────────────────────

  /**
   * Compute aggregate metrics over all estimated series.
   * Link metadata is not available here, so defaults are used.
   * For production use, supply a `LinkInfo` map to `aggregateWithInfo`.
   */
  def aggregate(estimatedSeries: Map[String, Array[Double]]): data.Models.TrafficMetrics =
    if estimatedSeries.isEmpty then
      return data.Models.TrafficMetrics(0, 0, 0, 0)

    val speeds    = estimatedSeries.values.map(s => avgSpeed(s)).toArray
    val occs      = estimatedSeries.values.map(s => occupancy(s)).toArray
    val vehs      = estimatedSeries.values.map(s => avgVehicles(s)).toArray
    val kms       = estimatedSeries.values.map(s => kmTraveled(s, 0.1)).toArray  // 100m default

    data.Models.TrafficMetrics(
      avgSpeed    = geometricMean(speeds.filter(_ > 0)),
      occupancy   = geometricMean(occs.filter(_ > 0)),
      avgVehicles = geometricMean(vehs.filter(_ > 0)),
      kmTraveled  = geometricMean(kms.filter(_ > 0)),
    )

  def aggregateWithInfo(
      estimatedSeries: Map[String, Array[Double]],
      linkInfo: Map[String, LinkInfo],
  ): data.Models.TrafficMetrics =
    if estimatedSeries.isEmpty then return data.Models.TrafficMetrics(0, 0, 0, 0)

    val results = estimatedSeries.flatMap { case (key, series) =>
      val linkId = key.split("-").headOption.getOrElse(key)
      val info   = linkInfo.getOrElse(linkId, LinkInfo(linkId, 0.1, DefaultCapacity, DefaultFreeFlowSpeedMs))
      val sp = avgSpeed(series, info.capacity, info.freeFlowSpeedMs)
      val oc = occupancy(series, info.capacity)
      val av = avgVehicles(series)
      val km = kmTraveled(series, info.lengthKm)
      if sp > 0 && oc > 0 && av > 0 && km > 0 then
        Some((sp, oc, av, km))
      else None
    }.toSeq.unzip4

    data.Models.TrafficMetrics(
      avgSpeed    = geometricMean(results._1.toArray),
      occupancy   = geometricMean(results._2.toArray),
      avgVehicles = geometricMean(results._3.toArray),
      kmTraveled  = geometricMean(results._4.toArray),
    )

  // ── Math helpers ──────────────────────────────────────────────────────────

  def geometricMean(xs: Array[Double]): Double =
    if xs.isEmpty then 0.0
    else math.exp(xs.map(x => math.log(x.max(1e-10))).sum / xs.length)

  extension [A, B, C, D](xs: Seq[(A, B, C, D)])
    private def unzip4: (Seq[A], Seq[B], Seq[C], Seq[D]) =
      (xs.map(_._1), xs.map(_._2), xs.map(_._3), xs.map(_._4))
