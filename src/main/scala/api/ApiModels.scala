package org.interscity.htc
package api

import spray.json.*
import spray.json.DefaultJsonProtocol.*

// ─── HTTP Request DTOs ─────────────────────────────────────────────────────────

/**
 * Optional per-run overrides sent in POST /api/v1/pipeline/start.
 * Any omitted field falls back to the current default config.
 *
 * Phase control:
 *  - startFromPhase: skip earlier phases, loading from checkpoint
 *    (LoadBase | PreprocessBase | Cluster | WarpingPaths | LoadPartial | PreprocessPartial | Estimation | Write)
 *  - stopAfterPhase: save checkpoint and stop after this phase
 *
 * Time window: restrict processing to a slice of the day.
 *  - timeWindowStart / timeWindowEnd: hour indices [0, 24) bounding the window
 *    e.g. {"timeWindowStart": 6, "timeWindowEnd": 10} = morning rush
 */
final case class StartPipelineRequest(
    baseSimulationId:      Option[String] = None,
    partialSimulationId:   Option[String] = None,
    outputSimulationId:    Option[String] = None,
    nClusters:             Option[Int]    = None,
    estimationMethod:      Option[String] = None,
    dataSource:            Option[String] = None,
    dataDir:               Option[String] = None,
    percentSample:         Option[Double] = None,
    workerParallelism:     Option[Int]    = None,
    validationMode:        Option[String] = None,
    validationReferenceId: Option[String] = None,
    startFromPhase:        Option[String] = None,
    stopAfterPhase:        Option[String] = None,
    timeWindowStart:       Option[Int]    = None,
    timeWindowEnd:         Option[Int]    = None,
)

/**
 * Persistent default-config update sent in PATCH /api/v1/config.
 * Every field is optional — only supplied fields are changed.
 * Takes effect on the next pipeline run.
 */
final case class UpdateConfigRequest(
    baseSimulationId:      Option[String] = None,
    partialSimulationId:   Option[String] = None,
    outputSimulationId:    Option[String] = None,
    nClusters:             Option[Int]    = None,
    kshapeMaxIter:         Option[Int]    = None,
    kshapeNInit:           Option[Int]    = None,
    estimationMethod:      Option[String] = None,
    percentSample:         Option[Double] = None,
    dtwWindowFraction:     Option[Double] = None,
    pcaComponents:         Option[Int]    = None,
    dataSource:            Option[String] = None,
    dataDir:               Option[String] = None,
    warpingPathsDir:       Option[String] = None,
    checkpointDir:         Option[String] = None,
    seriesLength:          Option[Int]    = None,
    intervalSize:          Option[Long]   = None,
    workerParallelism:     Option[Int]    = None,
    validationMode:        Option[String] = None,
    validationReferenceId: Option[String] = None,
    timeWindowStart:       Option[Int]    = None,
    timeWindowEnd:         Option[Int]    = None,
)

// ─── HTTP Response DTOs ───────────────────────────────────────────────────────

final case class PipelineStatusResponse(
    jobId:       Option[String],
    state:       String,
    startedAt:   Option[String],
    completedAt: Option[String],
    error:       Option[String],
)

final case class StartPipelineResponse(
    jobId: String,
    state: String,
)

final case class CancelResponse(
    jobId:    String,
    accepted: Boolean,
)

final case class ErrorResponse(
    error:   String,
    details: Option[String] = None,
)

final case class HealthResponse(
    status:  String,
    version: String,
)

// ─── Progress / results DTOs ──────────────────────────────────────────────────

final case class StageProgressInfo(
    phase:            String,
    status:           String,    // pending | running | completed | failed
    startedAt:        Option[String],
    completedAt:      Option[String],
    durationMs:       Option[Long],
    subProgressDone:  Option[Int],
    subProgressTotal: Option[Int],
    details:          Option[String],
)

final case class PipelineProgressResponse(
    jobId:        Option[String],
    state:        String,
    currentPhase: Option[String],
    stages:       List[StageProgressInfo],
)

final case class ClusterInfoResponse(
    id:          Int,
    size:        Int,
    simPointKey: String,
    centroid:    List[Double],
)

final case class ClusteringResultResponse(
    jobId:               Option[String],
    nClusters:           Int,
    totalSeries:         Int,
    seriesLength:        Int,
    intervalSizeSeconds: Long,
    clusters:            List[ClusterInfoResponse],
)

final case class TrafficMetricsResponse(
    avgSpeedMs:  Double,
    occupancy:   Double,
    avgVehicles: Double,
    kmTraveled:  Double,
)

final case class EstimationResultResponse(
    jobId:                Option[String],
    simulationId:         String,
    totalSeriesEstimated: Int,
    metrics:              TrafficMetricsResponse,
)

final case class ValidationLinkMetrics(
    key:         String,
    mae:         Double,
    rmse:        Double,
    mape:        Double,
    dtwDistance: Double,
    pearsonR:    Double,
)

final case class ValidationResultResponse(
    jobId:                Option[String],
    mode:                 String,
    referenceSimulationId: String,
    nLinks:               Int,
    meanMAE:              Double,
    meanRMSE:             Double,
    meanMAPE:             Double,
    meanDTW:              Double,
    meanPearsonR:         Double,
    perLink:              List[ValidationLinkMetrics],
)

final case class SeriesDefinitionResponse(
    seriesLength:           Int,
    intervalSizeSeconds:    Long,
    totalDurationSeconds:   Long,
    timeWindowStartHour:    Option[Int],
    timeWindowEndHour:      Option[Int],
    description:            String,
)

/**
 * Full config snapshot returned by GET /api/v1/config.
 */
final case class ConfigResponse(
    baseSimulationId:      String,
    partialSimulationId:   String,
    outputSimulationId:    String,
    nClusters:             Int,
    kshapeMaxIter:         Int,
    kshapeNInit:           Int,
    estimationMethod:      String,
    percentSample:         Double,
    dtwWindowFraction:     Double,
    pcaComponents:         Int,
    dataSource:            String,
    dataDir:               Option[String],
    warpingPathsDir:       String,
    checkpointDir:         String,
    seriesLength:          Int,
    intervalSize:          Long,
    workerParallelism:     Int,
    validationMode:        String,
    validationReferenceId: String,
    timeWindowStart:       Option[Int],
    timeWindowEnd:         Option[Int],
)

// ─── Spray-JSON implicit formats ──────────────────────────────────────────────

object JsonProtocol extends DefaultJsonProtocol:
  given RootJsonFormat[StartPipelineRequest]    = jsonFormat15(StartPipelineRequest.apply)
  given RootJsonFormat[UpdateConfigRequest]     = jsonFormat21(UpdateConfigRequest.apply)
  given RootJsonFormat[PipelineStatusResponse]  = jsonFormat5(PipelineStatusResponse.apply)
  given RootJsonFormat[StartPipelineResponse]   = jsonFormat2(StartPipelineResponse.apply)
  given RootJsonFormat[CancelResponse]          = jsonFormat2(CancelResponse.apply)
  given RootJsonFormat[ErrorResponse]           = jsonFormat2(ErrorResponse.apply)
  given RootJsonFormat[HealthResponse]          = jsonFormat2(HealthResponse.apply)
  given RootJsonFormat[ConfigResponse]          = jsonFormat21(ConfigResponse.apply)
  given RootJsonFormat[StageProgressInfo]       = jsonFormat8(StageProgressInfo.apply)
  given RootJsonFormat[PipelineProgressResponse] = jsonFormat4(PipelineProgressResponse.apply)
  given RootJsonFormat[ClusterInfoResponse]     = jsonFormat4(ClusterInfoResponse.apply)
  given RootJsonFormat[ClusteringResultResponse] = jsonFormat6(ClusteringResultResponse.apply)
  given RootJsonFormat[TrafficMetricsResponse]  = jsonFormat4(TrafficMetricsResponse.apply)
  given RootJsonFormat[EstimationResultResponse] = jsonFormat4(EstimationResultResponse.apply)
  given RootJsonFormat[ValidationLinkMetrics]   = jsonFormat6(ValidationLinkMetrics.apply)
  given RootJsonFormat[ValidationResultResponse] = jsonFormat10(ValidationResultResponse.apply)
  given RootJsonFormat[SeriesDefinitionResponse] = jsonFormat6(SeriesDefinitionResponse.apply)
