package org.interscity.htc
package estimation

import org.apache.pekko.actor.typed.*
import org.apache.pekko.actor.typed.scaladsl.*

import scala.collection.mutable
import scala.util.Try

import core.{SimEDaPEConfig, EstimationMethod, PipelinePhase}
import core.Protocol.*
import data.Models.*
import metrics.TrafficMetrics as TM

/**
 * Supervisor that fans out one `EstimationWorker` per cluster
 * and aggregates the estimated series and traffic metrics.
 */
object EstimationSupervisor:

  def apply(cfg: SimEDaPEConfig): Behavior[EstimationSupervisorCommand] =
    Behaviors.receiveMessage {
      case RunEstimation(clustering, baseDs, partialDs, warpingPaths, replyTo, progressTo) =>
        Behaviors.setup { ctx =>
          val k         = clustering.nClusters
          val allEstimates = mutable.HashMap.empty[String, Array[Double]]
          var completed = 0
          var failed    = false

          for clusterIdx <- 0 until k do
            val members      = clustering.membersOf(clusterIdx)
            val simPointIdx  = clustering.simPointIndices(clusterIdx)
            val simPointKey  = baseDs.keys(simPointIdx)
            val clusterPaths = warpingPaths.forCluster(clusterIdx)

            // Look up the sim point in the partial dataset
            val simPointNormPartial: Array[Double] =
              partialDs.keys.indexOf(simPointKey) match
                case -1  => Array.fill(cfg.seriesLength)(0.0)
                case idx => partialDs.normalised(idx)

            val baseMeans  = members.map(baseDs.means)
            val baseStds   = members.map(baseDs.stds)

            val (partialMeans, partialStds) = members.map { idx =>
              val key = baseDs.keys(idx)
              partialDs.keys.indexOf(key) match
                case -1  => (baseDs.means(idx), baseDs.stds(idx))   // fallback
                case pIdx => (partialDs.means(pIdx), partialDs.stds(pIdx))
            }.unzip match
              case (ms, ss) => (ms.toArray, ss.toArray)

            val worker = ctx.spawn(EstimationWorker(), s"est-worker-$clusterIdx")
            worker ! EstimateCluster(
              clusterId              = clusterIdx,
              simPointIndex          = simPointIdx,
              memberIndices          = members,
              baseMeans              = baseMeans,
              baseStds               = baseStds,
              partialMeans           = partialMeans,
              partialStds            = partialStds,
              simPointSeriesPartial  = simPointNormPartial,
              warpingPaths           = clusterPaths,
              percentSample          = cfg.percentSample,
              replyTo                = ctx.self.asInstanceOf[ActorRef[EstimationWorkerReply]],
              method                 = cfg.method,
              pcaComponents          = cfg.pcaComponents,
              baseSeries             = if cfg.method == EstimationMethod.PCA then members.map(baseDs.normalised) else Array.empty,
            )

          Behaviors.receiveMessage[EstimationSupervisorCommand | EstimationWorkerReply] {
            case ClusterEstimationDone(_, estimates) if !failed =>
              estimates.foreach { case (idx, series) =>
                val key = baseDs.keys(idx)
                allEstimates(key) = series
              }
              completed += 1
              progressTo ! SubProgressUpdate(PipelinePhase.Estimation, completed, k)
              if completed == k then
                val metrics = TM.aggregate(allEstimates.toMap)
                replyTo ! EstimationDone(EstimationResult(
                  simulationId    = cfg.outputSimulationId,
                  estimatedSeries = allEstimates.toMap,
                  metrics         = metrics,
                ))
                Behaviors.stopped
              else Behaviors.same

            case ClusterEstimationFailed(cid, cause) =>
              ctx.log.error(s"Estimation failed for cluster $cid", cause)
              failed = true
              replyTo ! EstimationFailed(cause)
              Behaviors.stopped

            case _ => Behaviors.same
          }.narrow[EstimationSupervisorCommand]
        }
    }

// ─── Per-cluster estimation worker ────────────────────────────────────────────

object EstimationWorker:

  def apply(): Behavior[EstimationWorkerCommand] =
    Behaviors.receiveMessage {
      case EstimateCluster(
            clusterId, simPointIdx, memberIndices,
            baseMeans, baseStds, partialMeans, partialStds,
            simPointSeriesPartial, warpingPaths,
            percentSample, replyTo, method, baseSeries, pcaComponents) =>

        val result = Try {
          if method == EstimationMethod.PCA then
            val model = PCAEstimator.fitCluster(baseSeries, pcaComponents)
            val rows = PCAEstimator.estimateCluster(model, simPointSeriesPartial, memberIndices.length, baseMeans, baseStds)
            memberIndices.zip(rows).toMap
          else
            memberIndices.zipWithIndex.flatMap { case (seriesIdx, pos) =>
              warpingPaths.get(seriesIdx) match
                case None =>
                  None  // no warping path — skip (missing sim point)

                case Some(path) =>
                  val (mu, sigma) = blendNormParams(
                    baseMeans(pos), baseStds(pos),
                    partialMeans(pos), partialStds(pos),
                    percentSample, method,
                  )
                  val estimated = TimeTranslation.estimate(path, simPointSeriesPartial, mu, sigma)
                  Some(seriesIdx -> estimated)
            }.toMap

        }

        result.fold(
          err => replyTo ! ClusterEstimationFailed(clusterId, err),
          est => replyTo ! ClusterEstimationDone(clusterId, est),
        )
        Behaviors.stopped
    }

  /** Weighted average of base and partial normalization parameters. */
  private def blendNormParams(
      baseMu: Double, baseSigma: Double,
      partialMu: Double, partialSigma: Double,
      percentSample: Double, method: EstimationMethod,
  ): (Double, Double) =
    method match
      case EstimationMethod.Base => TimeTranslation.muSigmaBase(baseMu, baseSigma)
      case EstimationMethod.Avg => TimeTranslation.muSigmaAvg(baseMu, baseSigma, partialMu, partialSigma)
      case EstimationMethod.Sample => TimeTranslation.muSigmaSample(partialMu, partialSigma)
      case _ => TimeTranslation.muSigmaWAvg(baseMu, baseSigma, partialMu, partialSigma, percentSample)
