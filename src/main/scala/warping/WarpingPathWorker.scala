package org.interscity.htc
package warping

import org.apache.pekko.actor.typed.*
import org.apache.pekko.actor.typed.scaladsl.*

import scala.collection.mutable
import scala.util.Try

import core.{SimEDaPEConfig, PipelinePhase}
import core.Protocol.*
import data.Models.*

/**
 * Supervisor that spawns one `WarpingPathWorker` per cluster and
 * collects results, providing cluster-level parallelism.
 *
 * Parallelism is bounded by `cfg.workerParallelism` — at most that many
 * workers run concurrently.
 */
object WarpingPathSupervisor:

  def apply(cfg: SimEDaPEConfig): Behavior[WarpingSupervisorCommand] =
    Behaviors.receiveMessage {
      case ComputeWarpingPaths(clustering, dataset, replyTo, progressTo) =>
        Behaviors.setup { ctx =>
          val k = clustering.nClusters
          ctx.log.info("Launching {} warping-path workers (parallelism={})", k, cfg.workerParallelism)

          val pending    = mutable.HashMap.empty[Int, Map[Int, WarpingPath]]
          var completed  = 0
          var failed     = false

          require(cfg.workerParallelism > 0, "workerParallelism must be positive")
          var nextCluster = 0
          def launchNext(): Unit =
            val clusterIdx = nextCluster
            nextCluster += 1
            val members  = clustering.membersOf(clusterIdx)
            val centroid = clustering.centroids(clusterIdx)
            val memberSeries = members.map(dataset.normalised)

            val worker = ctx.spawn(
              WarpingPathWorker(),
              s"wp-worker-$clusterIdx",
              DispatcherSelector.fromConfig("pekko.actor.compute-dispatcher")
            )
            worker ! ComputeClusterWarpingPaths(
              clusterId     = clusterIdx,
              centroid      = centroid,
              memberIndices = members,
              memberSeries  = memberSeries,
              windowFraction = cfg.dtwWindowFraction,
              replyTo       = ctx.self.asInstanceOf[ActorRef[WarpingWorkerReply]],
            )

          for _ <- 0 until math.min(k, cfg.workerParallelism) do launchNext()

          // Collect replies
          Behaviors
            .receiveMessage[WarpingSupervisorCommand | WarpingWorkerReply] {
              case ClusterWarpingPathsDone(clusterId, paths) if !failed =>
                pending(clusterId) = paths
                completed += 1
                if nextCluster < k then launchNext()
                progressTo ! SubProgressUpdate(PipelinePhase.WarpingPaths, completed, k)
                ctx.log.debug("Warping paths done for cluster {}/{}", completed, k)
                if completed == k then
                  replyTo ! WarpingPathsDone(WarpingPathsResult(pending.toMap))
                  Behaviors.stopped
                else
                  Behaviors.same

              case ClusterWarpingFailed(clusterId, cause) =>
                ctx.log.error(s"Warping computation failed for cluster $clusterId", cause)
                failed = true
                replyTo ! WarpingFailed(cause)
                Behaviors.stopped

              case _ => Behaviors.same
            }.narrow[WarpingSupervisorCommand]
        }
    }

// ─── Per-cluster worker ────────────────────────────────────────────────────────

/**
 * Computes DTW warping paths for all members of one cluster against the centroid.
 * Runs in its own actor (thread in the dispatcher) for parallelism.
 */
object WarpingPathWorker:

  def apply(): Behavior[WarpingWorkerCommand] =
    Behaviors.receiveMessage {
      case ComputeClusterWarpingPaths(clusterId, centroid, memberIndices, memberSeries, windowFraction, replyTo) =>
        val result = Try {
          val paths = memberIndices.zip(memberSeries).map { case (idx, series) =>
            val (path, _) = DTWComputer.warpingPath(centroid, series, windowFraction)
            idx -> path
          }.toMap
          paths
        }
        result.fold(
          err => replyTo ! ClusterWarpingFailed(clusterId, err),
          ps  => replyTo ! ClusterWarpingPathsDone(clusterId, ps),
        )
        Behaviors.stopped   // single-use worker
    }
