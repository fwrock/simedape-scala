package org.interscity.htc
package api

import org.apache.pekko.actor.typed.*
import org.apache.pekko.actor.typed.scaladsl.*
import java.time.Instant
import java.util.UUID

import core.{SimEDaPEConfig, DataSource, EstimationMethod, ValidationMode}
import core.Protocol.*
import api.ApiProtocol.*

/**
 * Single actor that manages the SimEDaPE pipeline lifecycle on behalf of HTTP clients.
 *
 * State machine:
 *   Idle ──start──▶ Running ──done──▶ Completed
 *                           ──fail──▶ Failed
 *                           ──cancel▶ Failed(cancelled)
 *
 * A new start is always accepted from Completed/Failed (returns to Running).
 * Config updates (ApiUpdateConfig) are accepted in any state and take effect
 * on the next pipeline run.
 */
object PipelineApiGateway:

  def apply(defaultConfig: SimEDaPEConfig): Behavior[GatewayCommand] =
    idle(defaultConfig, lastRun = None)

  // ─── state: idle ──────────────────────────────────────────────────────────

  private def idle(
      defaultConfig: SimEDaPEConfig,
      lastRun:       Option[PipelineRunInfo],
  ): Behavior[GatewayCommand] =
    Behaviors.receive { (ctx, msg) =>
      msg match
        case ApiStart(req, replyTo) =>
          val jobId = UUID.randomUUID().toString.take(8)
          val cfg   = applyOverrides(defaultConfig, req)
          val info  = PipelineRunInfo(Some(jobId), RunState.Running, Some(Instant.now()), None, None, None)
          ctx.spawn(PipelineJobSupervisor(cfg, jobId, ctx.self), s"pipeline-job-$jobId")
          ctx.log.info("API: started pipeline job {}", jobId)
          replyTo ! Started(jobId)
          running(defaultConfig, info)

        case ApiGetStatus(replyTo) =>
          val info = lastRun.getOrElse(PipelineRunInfo(None, RunState.Idle, None, None, None, None))
          replyTo ! StatusReply(info)
          Behaviors.same

        case ApiCancel(replyTo) =>
          replyTo ! NotRunning
          Behaviors.same

        case ApiGetConfig(replyTo) =>
          replyTo ! ConfigReply(defaultConfig)
          Behaviors.same

        case ApiUpdateConfig(req, replyTo) =>
          val newConfig = applyConfigUpdate(defaultConfig, req)
          ctx.log.info("API: default config updated")
          replyTo ! ConfigUpdated
          idle(newConfig, lastRun)

        case _ => Behaviors.same
    }

  // ─── state: running ───────────────────────────────────────────────────────

  private def running(
      defaultConfig: SimEDaPEConfig,
      runInfo:       PipelineRunInfo,
  ): Behavior[GatewayCommand] =
    Behaviors.receive { (ctx, msg) =>
      msg match
        case ApiStart(_, replyTo) =>
          replyTo ! AlreadyRunning(runInfo.jobId.get)
          Behaviors.same

        case ApiGetStatus(replyTo) =>
          replyTo ! StatusReply(runInfo)
          Behaviors.same

        case ApiCancel(replyTo) =>
          ctx.children
            .find(_.path.name.startsWith("pipeline-job-"))
            .foreach(child => ctx.stop(child.asInstanceOf[ActorRef[Nothing]]))
          val cancelled = runInfo.copy(
            state       = RunState.Failed,
            completedAt = Some(Instant.now()),
            error       = Some("Cancelled by user"),
          )
          ctx.log.info("API: cancelled pipeline job {}", runInfo.jobId.get)
          replyTo ! Cancelled(runInfo.jobId.get)
          idle(defaultConfig, Some(cancelled))

        case ApiGetConfig(replyTo) =>
          replyTo ! ConfigReply(defaultConfig)
          Behaviors.same

        case ApiUpdateConfig(req, replyTo) =>
          // Takes effect on the NEXT run; current run is unaffected
          val newConfig = applyConfigUpdate(defaultConfig, req)
          ctx.log.info("API: default config updated (takes effect on next pipeline run)")
          replyTo ! ConfigUpdated
          running(newConfig, runInfo)

        case PipelineDone(jobId, _) =>
          ctx.log.info("API: pipeline job {} completed", jobId)
          val done = runInfo.copy(state = RunState.Completed, completedAt = Some(Instant.now()))
          idle(defaultConfig, Some(done))

        case PipelineJobError(jobId, cause) =>
          ctx.log.error("API: pipeline job {} failed: {}", jobId, cause.getMessage)
          val failed = runInfo.copy(
            state       = RunState.Failed,
            completedAt = Some(Instant.now()),
            error       = Some(Option(cause.getMessage).getOrElse(cause.getClass.getSimpleName)),
          )
          idle(defaultConfig, Some(failed))

        case _ => Behaviors.same
    }

  // ─── per-run override helper ──────────────────────────────────────────────

  private def applyOverrides(cfg: SimEDaPEConfig, req: StartPipelineRequest): SimEDaPEConfig =
    cfg.copy(
      baseSimulationId      = req.baseSimulationId.getOrElse(cfg.baseSimulationId),
      partialSimulationId   = req.partialSimulationId.getOrElse(cfg.partialSimulationId),
      outputSimulationId    = req.outputSimulationId.getOrElse(cfg.outputSimulationId),
      nClusters             = req.nClusters.getOrElse(cfg.nClusters),
      kshapeMaxIter         = cfg.kshapeMaxIter,
      kshapeNInit           = cfg.kshapeNInit,
      seriesLength          = cfg.seriesLength,
      intervalSize          = cfg.intervalSize,
      method                = req.estimationMethod.map(EstimationMethod.valueOf).getOrElse(cfg.method),
      percentSample         = req.percentSample.getOrElse(cfg.percentSample),
      dtwWindowFraction     = cfg.dtwWindowFraction,
      pcaComponents         = cfg.pcaComponents,
      warpingPathsDir       = cfg.warpingPathsDir,
      clickhouse            = cfg.clickhouse,
      workerParallelism     = req.workerParallelism.getOrElse(cfg.workerParallelism),
      http                  = cfg.http,
      validation            = req.validationMode.map(ValidationMode.valueOf).getOrElse(cfg.validation),
      validationReferenceId = req.validationReferenceId.getOrElse(cfg.validationReferenceId),
      dataSource =
        req.dataDir
          .map(DataSource.File.apply)
          .orElse(req.dataSource.map {
            case "file"       =>
              DataSource.File(cfg.dataSource match
                case DataSource.File(d) => d
                case _                  => "/data/simulations"
              )
            case "clickhouse" => DataSource.ClickHouse
            case other        => throw IllegalArgumentException(s"Unknown data-source: $other")
          })
          .getOrElse(cfg.dataSource),
    )

  // ─── persistent config update helper ─────────────────────────────────────

  private def applyConfigUpdate(cfg: SimEDaPEConfig, req: UpdateConfigRequest): SimEDaPEConfig =
    val updatedDataSource =
      req.dataSource.map {
        case "file"       =>
          DataSource.File(req.dataDir.getOrElse(cfg.dataSource match
            case DataSource.File(d) => d
            case _                  => "/data/simulations"
          ))
        case "clickhouse" => DataSource.ClickHouse
        case other        => throw IllegalArgumentException(s"Unknown data-source: $other")
      }.orElse(req.dataDir.map(DataSource.File.apply))
       .getOrElse(cfg.dataSource)

    cfg.copy(
      baseSimulationId      = req.baseSimulationId.getOrElse(cfg.baseSimulationId),
      partialSimulationId   = req.partialSimulationId.getOrElse(cfg.partialSimulationId),
      outputSimulationId    = req.outputSimulationId.getOrElse(cfg.outputSimulationId),
      nClusters             = req.nClusters.getOrElse(cfg.nClusters),
      kshapeMaxIter         = req.kshapeMaxIter.getOrElse(cfg.kshapeMaxIter),
      kshapeNInit           = req.kshapeNInit.getOrElse(cfg.kshapeNInit),
      method                = req.estimationMethod.map(EstimationMethod.valueOf).getOrElse(cfg.method),
      percentSample         = req.percentSample.getOrElse(cfg.percentSample),
      dtwWindowFraction     = req.dtwWindowFraction.getOrElse(cfg.dtwWindowFraction),
      pcaComponents         = req.pcaComponents.getOrElse(cfg.pcaComponents),
      dataSource            = updatedDataSource,
      warpingPathsDir       = req.warpingPathsDir.getOrElse(cfg.warpingPathsDir),
      seriesLength          = req.seriesLength.getOrElse(cfg.seriesLength),
      intervalSize          = req.intervalSize.getOrElse(cfg.intervalSize),
      workerParallelism     = req.workerParallelism.getOrElse(cfg.workerParallelism),
      validation            = req.validationMode.map(ValidationMode.valueOf).getOrElse(cfg.validation),
      validationReferenceId = req.validationReferenceId.getOrElse(cfg.validationReferenceId),
      clickhouse            = cfg.clickhouse,
      http                  = cfg.http,
    )

// ─── Per-job supervisor ────────────────────────────────────────────────────────

/**
 * Single-use actor that wraps one pipeline run.
 * Spawns PipelineCoordinator, waits for its completion event, then
 * notifies the PipelineApiGateway.
 */
private[api] object PipelineJobSupervisor:

  def apply(
      config:  SimEDaPEConfig,
      jobId:   String,
      gateway: ActorRef[GatewayCommand],
  ): Behavior[PipelineEvent] =
    Behaviors.setup { ctx =>
      val coordinator = ctx.spawn(
        core.PipelineCoordinator(config, ctx.self),
        "coordinator",
      )
      coordinator ! StartPipeline

      Behaviors.receiveMessage {
        case PipelineCompleted(validation) =>
          gateway ! PipelineDone(jobId, validation)
          Behaviors.stopped

        case PipelineFailed(cause) =>
          gateway ! PipelineJobError(jobId, cause)
          Behaviors.stopped
      }
    }
