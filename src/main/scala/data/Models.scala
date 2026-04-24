package org.interscity.htc
package data

/** All domain data models for the SimEDaPE pipeline. */
object Models:

  /**
   * A single univariate time series for one road segment.
   *
   * @param linkId   road segment identifier
   * @param interval which time interval this series covers (e.g. hour index)
   * @param values   raw vehicle count per tick bucket
   */
  final case class TimeSeries(
      linkId: String,
      interval: Int,
      values: Array[Double],
  ):
    /** Unique key used for lookup across datasets */
    def key: String = s"$linkId-$interval"

  /**
   * A collection of time series for one simulation scenario.
   *
   * @param simulationId source simulation identifier
   * @param series       ordered list of time series
   * @param index        key → position in `series`
   */
  final case class TimeSeriesDataset(
      simulationId: String,
      series: IndexedSeq[TimeSeries],
      index: Map[String, Int],
  ):
    def get(key: String): Option[TimeSeries]  = index.get(key).map(series)
    def getByIdx(i: Int): TimeSeries          = series(i)
    def size: Int                             = series.size

  object TimeSeriesDataset:
    def build(simulationId: String, raw: Seq[TimeSeries]): TimeSeriesDataset =
      val vec = raw.toIndexedSeq
      val idx = vec.zipWithIndex.map { case (ts, i) => ts.key -> i }.toMap
      TimeSeriesDataset(simulationId, vec, idx)

  /**
   * Output of the preprocessing step.
   * Each series is z-score normalised; mean/std are stored for de-normalisation.
   */
  final case class PreprocessedDataset(
      simulationId: String,
      keys: IndexedSeq[String],
      normalised: Array[Array[Double]],  // shape [nSeries, seriesLength]
      means: Array[Double],              // per-series mean (before normalisation)
      stds: Array[Double],               // per-series std  (before normalisation)
  ):
    def size: Int = keys.size
    def seriesAt(i: Int): Array[Double] = normalised(i)

  /**
   * A single DTW warping path step.
   *
   * @param i         index in the centroid (reference) series
   * @param j         index in the target series
   * @param cost      accumulated DTW cost at this cell (square-rooted)
   * @param descending true when centroid[i] > target[j]
   */
  final case class WarpingStep(i: Int, j: Int, cost: Double, descending: Boolean)

  /** Ordered sequence of steps from (0,0) to (r-1,c-1) */
  type WarpingPath = List[WarpingStep]

  /**
   * All warping paths for all clusters.
   * Outer key: cluster id. Inner key: series index in the dataset.
   */
  final case class WarpingPathsResult(
      paths: Map[Int, Map[Int, WarpingPath]]   // clusterId -> seriesIdx -> path
  ):
    def forCluster(k: Int): Map[Int, WarpingPath] = paths.getOrElse(k, Map.empty)

  /**
   * K-Shape clustering result.
   *
   * @param nClusters        K
   * @param labels           cluster assignment per series (length = nSeries)
   * @param centroids        one normalised centroid per cluster
   * @param simPointIndices  index of the simulation point (representative) for each cluster
   */
  final case class ClusteringResult(
      nClusters: Int,
      labels: Array[Int],
      centroids: Array[Array[Double]],
      simPointIndices: Array[Int],
  ):
    def membersOf(k: Int): Array[Int] =
      labels.zipWithIndex.collect { case (label, idx) if label == k => idx }

  /**
   * Final estimation result.
   *
   * @param estimatedSeries  seriesKey -> estimated raw counts array
   * @param metrics          aggregated traffic metrics
   */
  final case class EstimationResult(
      simulationId: String,
      estimatedSeries: Map[String, Array[Double]],
      metrics: TrafficMetrics,
  )

  /**
   * Aggregated traffic metrics (geometric mean across all roads).
   */
  final case class TrafficMetrics(
      avgSpeed: Double,        // m/s
      occupancy: Double,       // %
      avgVehicles: Double,
      kmTraveled: Double,
  )

  /**
   * Per-link road metadata (capacity, length, free-flow speed).
   * Loaded once from configuration or a lookup table.
   */
  final case class LinkInfo(
      linkId: String,
      lengthKm: Double,
      capacity: Int,
      freeFlowSpeedMs: Double,
  )

  // ─── Validation ─────────────────────────────────────────────────────────────

  /** Per-link error metrics comparing an estimated series against a reference. */
  final case class LinkValidationMetrics(
      key: String,
      mae: Double,         // Mean Absolute Error
      rmse: Double,        // Root Mean Squared Error
      mape: Double,        // Mean Absolute Percentage Error (%)
      dtwDistance: Double, // DTW distance between estimated and reference
      pearsonR: Double,    // Pearson correlation coefficient
  )

  /** Aggregate (mean) metrics across all validated non-simulated links. */
  final case class ValidationAggregateMetrics(
      meanMAE:      Double,
      meanRMSE:     Double,
      meanMAPE:     Double,
      meanDTW:      Double,
      meanPearsonR: Double,
      nLinks:       Int,
  )

  /**
   * Full validation result for one pipeline run.
   *
   * @param mode                  "AgainstBase" or "AgainstReference"
   * @param referenceSimulationId simulation used as ground truth
   * @param perLink               per-link metrics keyed by series key
   * @param aggregate             mean metrics across all validated links
   */
  final case class ValidationResult(
      mode:                  String,
      referenceSimulationId: String,
      perLink:               Map[String, LinkValidationMetrics],
      aggregate:             ValidationAggregateMetrics,
  )
