package org.interscity.htc
package core

import org.apache.pekko.actor.typed.ActorRef
import data.Models.*
import java.time.Instant

/** All messages exchanged between actors in the SimEDaPE pipeline. */
object Protocol:

  // ─── Pipeline lifecycle ───────────────────────────────────────────────────

  sealed trait PipelineCommand
  case object StartPipeline extends PipelineCommand

  sealed trait PipelineEvent
  /** Sent by PipelineCoordinator when the full pipeline finishes. */
  final case class PipelineCompleted(validation: Option[ValidationResult]) extends PipelineEvent
  final case class PipelineFailed(cause: Throwable)                        extends PipelineEvent

  // ─── Pipeline progress events (forwarded gateway → HTTP) ────────────────────────

  sealed trait PipelineProgressEvent extends PipelineEvent

  /** Emitted when a pipeline stage begins. */
  final case class StageStarted(
      phase: PipelinePhase,
      at:    Instant,
  ) extends PipelineProgressEvent

  /** Emitted when a pipeline stage finishes (successfully). */
  final case class StageFinished(
      phase:   PipelinePhase,
      at:      Instant,
      details: Option[String] = None,
  ) extends PipelineProgressEvent

  /** Sub-progress update for multi-worker stages (DTW, Estimation). */
  final case class SubProgressUpdate(
      phase: PipelinePhase,
      done:  Int,
      total: Int,
  ) extends PipelineProgressEvent

  /** Snapshot of the clustering result — sent after Step 3 completes. */
  final case class ClusteringSnapshot(
      result:       ClusteringResult,
      seriesKeys:   IndexedSeq[String],
      seriesLength: Int,
      intervalSize: Long,
  ) extends PipelineProgressEvent

  /** Snapshot of the estimation result — sent after Step 7 completes. */
  final case class EstimationSnapshot(result: EstimationResult) extends PipelineProgressEvent

  /** Snapshot of the validation result — sent after optional validation step. */
  final case class ValidationSnapshot(result: ValidationResult) extends PipelineProgressEvent

  // ─── Data loading ─────────────────────────────────────────────────────────

  sealed trait DataLoaderCommand
  final case class LoadBaseData(
      replyTo: ActorRef[DataLoaderReply]
  ) extends DataLoaderCommand

  final case class LoadPartialData(
      simulationPointIds: Set[String],
      replyTo: ActorRef[DataLoaderReply]
  ) extends DataLoaderCommand

  /** Load ALL series for an arbitrary simulation ID (used for validation reference). */
  final case class LoadSimulationData(
      simulationId: String,
      replyTo: ActorRef[DataLoaderReply]
  ) extends DataLoaderCommand

  sealed trait DataLoaderReply
  final case class BaseDataLoaded(dataset: TimeSeriesDataset)    extends DataLoaderReply
  final case class PartialDataLoaded(dataset: TimeSeriesDataset) extends DataLoaderReply
  final case class DataLoadFailed(cause: Throwable)              extends DataLoaderReply

  // ─── Preprocessing ────────────────────────────────────────────────────────

  sealed trait PreprocessorCommand
  final case class PreprocessData(
      dataset: TimeSeriesDataset,
      replyTo: ActorRef[PreprocessorReply]
  ) extends PreprocessorCommand

  sealed trait PreprocessorReply
  final case class DataPreprocessed(result: PreprocessedDataset) extends PreprocessorReply
  final case class PreprocessFailed(cause: Throwable)            extends PreprocessorReply

  // ─── Clustering ───────────────────────────────────────────────────────────

  sealed trait ClustererCommand
  final case class RunClustering(
      dataset: PreprocessedDataset,
      replyTo: ActorRef[ClustererReply]
  ) extends ClustererCommand

  sealed trait ClustererReply
  final case class ClusteringDone(result: ClusteringResult) extends ClustererReply
  final case class ClusteringFailed(cause: Throwable)       extends ClustererReply

  // ─── Warping path calculation ─────────────────────────────────────────────

  sealed trait WarpingSupervisorCommand
  final case class ComputeWarpingPaths(
      clustering:  ClusteringResult,
      dataset:     PreprocessedDataset,
      replyTo:     ActorRef[WarpingSupervisorReply],
      progressTo:  ActorRef[PipelineEvent],
  ) extends WarpingSupervisorCommand

  sealed trait WarpingSupervisorReply
  final case class WarpingPathsDone(paths: WarpingPathsResult) extends WarpingSupervisorReply
  final case class WarpingFailed(cause: Throwable)             extends WarpingSupervisorReply

  // Internal: per-cluster worker
  sealed trait WarpingWorkerCommand
  final case class ComputeClusterWarpingPaths(
      clusterId: Int,
      centroid: Array[Double],
      memberIndices: Array[Int],
      memberSeries: Array[Array[Double]],
      windowFraction: Double,
      replyTo: ActorRef[WarpingWorkerReply]
  ) extends WarpingWorkerCommand

  sealed trait WarpingWorkerReply
  final case class ClusterWarpingPathsDone(
      clusterId: Int,
      paths: Map[Int, WarpingPath]   // seriesIndex -> path
  ) extends WarpingWorkerReply
  final case class ClusterWarpingFailed(clusterId: Int, cause: Throwable) extends WarpingWorkerReply

  // ─── Estimation ───────────────────────────────────────────────────────────

  sealed trait EstimationSupervisorCommand
  final case class RunEstimation(
      clustering:     ClusteringResult,
      baseDataset:    PreprocessedDataset,
      partialDataset: PreprocessedDataset,
      warpingPaths:   WarpingPathsResult,
      replyTo:        ActorRef[EstimationSupervisorReply],
      progressTo:     ActorRef[PipelineEvent],
  ) extends EstimationSupervisorCommand

  sealed trait EstimationSupervisorReply
  final case class EstimationDone(result: EstimationResult)  extends EstimationSupervisorReply
  final case class EstimationFailed(cause: Throwable)        extends EstimationSupervisorReply

  // Internal: per-cluster estimation worker
  sealed trait EstimationWorkerCommand
  final case class EstimateCluster(
      clusterId: Int,
      simPointIndex: Int,
      memberIndices: Array[Int],
      baseMeans: Array[Double],
      baseStds: Array[Double],
      partialMeans: Array[Double],
      partialStds: Array[Double],
      simPointSeriesPartial: Array[Double],
      warpingPaths: Map[Int, WarpingPath],
      percentSample: Double,
      replyTo: ActorRef[EstimationWorkerReply],
      method: EstimationMethod = EstimationMethod.WAvg,
      baseSeries: Array[Array[Double]] = Array.empty[Array[Double]],
      pcaComponents: Int = 1
  ) extends EstimationWorkerCommand

  sealed trait EstimationWorkerReply
  final case class ClusterEstimationDone(
      clusterId: Int,
      estimates: Map[Int, Array[Double]]  // seriesIndex -> estimated series (raw counts)
  ) extends EstimationWorkerReply
  final case class ClusterEstimationFailed(clusterId: Int, cause: Throwable) extends EstimationWorkerReply

  // ─── Result writing ───────────────────────────────────────────────────────

  sealed trait WriterCommand
  final case class WriteEstimations(
      result: EstimationResult,
      simulationId: String,
      replyTo: ActorRef[WriterReply]
  ) extends WriterCommand

  sealed trait WriterReply
  case object WriteCompleted                     extends WriterReply
  final case class WriteFailed(cause: Throwable) extends WriterReply

  // ─── Estimation validation ────────────────────────────────────────────────

  /**
   * Ask the EstimationValidator to compare estimated series against a reference.
   *
   * @param estimatedSeries  key → estimated raw vehicle counts (de-normalised)
   * @param referenceValues  key → reference raw vehicle counts (same keys subset)
   * @param referenceSimId   simulation ID used as ground truth (for logging/result)
   * @param mode             "AgainstBase" | "AgainstReference"
   */
  sealed trait ValidatorCommand
  final case class RunValidation(
      estimatedSeries: Map[String, Array[Double]],
      referenceValues: Map[String, Array[Double]],
      referenceSimId:  String,
      mode:            String,
      replyTo:         ActorRef[ValidatorReply],
  ) extends ValidatorCommand

  sealed trait ValidatorReply
  final case class ValidationDone(result: ValidationResult)  extends ValidatorReply
  final case class ValidationFailed(cause: Throwable)        extends ValidatorReply
