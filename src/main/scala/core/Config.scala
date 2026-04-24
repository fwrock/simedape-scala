package org.interscity.htc
package core

import com.typesafe.config.{Config as TypesafeConfig, ConfigFactory}

/** How to source/sink time series data */
enum DataSource:
  case File(directory: String)
  case ClickHouse

/** Which estimation strategy to use */
enum EstimationMethod:
  case Base   // use base scenario normalization only
  case Avg    // arithmetic average of base + partial normalizations
  case WAvg   // weighted average (weight = % of simulated links)
  case Sample // use only partial scenario normalization (no estimation)
  case PCA    // per-cluster PCA reconstruction

/**
 * Controls whether (and how) the estimated output is compared against
 * a known ground-truth simulation to measure error.
 *
 *  - Disabled          : no validation (default)
 *  - AgainstBase       : compare estimated non-simulated links vs the BASE simulation
 *                        (validates estimation quality under an unmodified scenario)
 *  - AgainstReference  : compare against an independently-run full simulation;
 *                        use validationReferenceId to specify which one
 *                        (validates estimation under a modified scenario)
 */
enum ValidationMode:
  case Disabled
  case AgainstBase
  case AgainstReference

/**
 * Pipeline phase identifiers for selective execution.
 * Pass as startFromPhase to skip earlier phases (loading from checkpoint)
 * or as stopAfterPhase to halt and save after a specific phase.
 */
enum PipelinePhase:
  case LoadBase, PreprocessBase, Cluster, WarpingPaths, LoadPartial, PreprocessPartial, Estimation, Write

/**
 * Optional time-of-day window filter applied to time series by interval index.
 * Only series whose interval falls within [startHour, endHour) are retained.
 *
 * Example: TimeWindow(6, 10) keeps morning rush hours 06:00–10:00
 * when using hourly intervals (intervalSize = 3600).
 */
final case class TimeWindow(startHour: Int, endHour: Int):
  def contains(interval: Int): Boolean = interval >= startHour && interval < endHour
  def label: String                    = s"${startHour}h-${endHour}h"

/** Top-level configuration for the full SimEDaPE pipeline */
final case class SimEDaPEConfig(
    // Identifiers
    baseSimulationId: String,
    partialSimulationId: String,
    outputSimulationId: String,

    // Clustering
    nClusters: Int,
    kshapeMaxIter: Int,
    kshapeNInit: Int,

    // Time series shape
    seriesLength: Int,   // number of ticks per series after resampling
    intervalSize: Long,  // tick granularity for aggregation

    // Estimation
    method: EstimationMethod,
    percentSample: Double, // used only by WAvg

    // DTW warping window (fraction of series length)
    dtwWindowFraction: Double,

    // PCA components per cluster
    pcaComponents: Int,

    // I/O
    dataSource: DataSource,
    warpingPathsDir: String,   // directory to persist warping paths (JSON)
    checkpointDir: String,     // directory for pipeline phase checkpoints

    // ClickHouse
    clickhouse: ClickHouseConfig,

    // Parallelism
    workerParallelism: Int,

    // HTTP API
    http: HttpConfig,

    // Validation
    validation:            ValidationMode,
    validationReferenceId: String,   // simulation ID for AgainstReference mode

    // Per-run overrides (not persisted to application.conf)
    timeWindow:      Option[TimeWindow]    = None,  // filter series to this day interval
    startFromPhase:  Option[PipelinePhase] = None,  // resume from checkpoint at this phase
    stopAfterPhase:  Option[PipelinePhase] = None,  // save checkpoint and stop after this phase
)

final case class ClickHouseConfig(
    host: String,
    port: Int,
    database: String,
    user: String,
    password: String,
    poolSize: Int,
)

final case class HttpConfig(
    host:      String,
    port:      Int,
    autoStart: Boolean, // start pipeline automatically on launch
)

object SimEDaPEConfig:
  def fromTypesafe(cfg: TypesafeConfig = ConfigFactory.load()): SimEDaPEConfig =
    val s = cfg.getConfig("simedape")
    SimEDaPEConfig(
      baseSimulationId    = s.getString("base-simulation-id"),
      partialSimulationId = s.getString("partial-simulation-id"),
      outputSimulationId  = s.getString("output-simulation-id"),
      nClusters           = s.getInt("n-clusters"),
      kshapeMaxIter       = s.getInt("kshape-max-iter"),
      kshapeNInit         = s.getInt("kshape-n-init"),
      seriesLength        = s.getInt("series-length"),
      intervalSize        = s.getLong("interval-size"),
      method              = EstimationMethod.valueOf(s.getString("estimation-method")),
      percentSample       = s.getDouble("percent-sample"),
      dtwWindowFraction   = s.getDouble("dtw-window-fraction"),
      pcaComponents       = s.getInt("pca-components"),
      dataSource          = if s.getString("data-source") == "file"
                            then DataSource.File(s.getString("data-dir"))
                            else DataSource.ClickHouse,
      warpingPathsDir     = s.getString("warping-paths-dir"),
      checkpointDir       = s.getString("checkpoint-dir"),
      clickhouse          = ClickHouseConfig(
        host     = s.getString("clickhouse.host"),
        port     = s.getInt("clickhouse.port"),
        database = s.getString("clickhouse.database"),
        user     = s.getString("clickhouse.user"),
        password = s.getString("clickhouse.password"),
        poolSize = s.getInt("clickhouse.pool-size"),
      ),
      workerParallelism   = s.getInt("worker-parallelism"),
      http                = HttpConfig(
        host      = s.getString("http.host"),
        port      = s.getInt("http.port"),
        autoStart = s.getBoolean("http.auto-start"),
      ),
      validation            =
        scala.util.Try(ValidationMode.valueOf(s.getString("validation.mode")))
          .getOrElse(ValidationMode.Disabled),
      validationReferenceId =
        if s.hasPath("validation.reference-id") then s.getString("validation.reference-id") else "",
    )
