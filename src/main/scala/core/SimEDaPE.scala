package org.interscity.htc
package core

import org.apache.pekko.actor.typed.*
import org.apache.pekko.actor.typed.scaladsl.*
import org.slf4j.LoggerFactory

import data.Models.*
import core.Protocol.*
import java.time.Instant

/** Entry point — creates the ActorSystem with HTTP API + optional auto-start. */
object SimEDaPE:

  private val log = LoggerFactory.getLogger(getClass)

  def start(): Unit =
    val config = SimEDaPEConfig.fromTypesafe()
    log.info(
      "Starting SimEDaPE — base={} partial={} api={}:{} validation={}",
      config.baseSimulationId, config.partialSimulationId,
      config.http.host, config.http.port,
      config.validation,
    )
    val system = ActorSystem(AppGuardian(config), "simedape")
    sys.addShutdownHook {
      log.info("Shutting down SimEDaPE")
      system.terminate()
    }

// ─── Application Guardian ──────────────────────────────────────────────────────

object AppGuardian:

  def apply(config: SimEDaPEConfig): Behavior[Nothing] =
    Behaviors.setup[Nothing] { ctx =>
      import api.{PipelineApiGateway, HttpServer, Routes}
      import api.ApiProtocol.*
      import api.{StartPipelineRequest}

      val gateway = ctx.spawn(PipelineApiGateway(config), "api-gateway")
      val nativeApi = new org.interscity.htc.parity.Api(java.nio.file.Path.of(sys.env.getOrElse("SIMEDAPE_STATE_DIR", "state")))(using ctx.system)
      import org.apache.pekko.http.scaladsl.server.Directives.concat
      val route = concat(Routes(gateway)(using ctx.system), nativeApi.route)
      ctx.system.whenTerminated.onComplete(_ => nativeApi.close())(ctx.executionContext)
      HttpServer.bind(route, config.http)(using ctx.system)

      if config.http.autoStart then
        ctx.log.info(
          "Auto-starting pipeline (base={}, partial={})",
          config.baseSimulationId, config.partialSimulationId,
        )
        val noopRef = ctx.system.deadLetters.asInstanceOf[ActorRef[GatewayReply]]
        gateway ! ApiStart(StartPipelineRequest(), noopRef)

      Behaviors.empty
    }

// ─── PipelineCoordinator ──────────────────────────────────────────────────────

/**
 * Orchestrates the full SimEDaPE pipeline as a sequential state machine.
 *
 * Normal flow (validation disabled):
 *   LoadBase → PreprocessBase → Cluster → WarpingPaths
 *     → LoadPartial → PreprocessPartial → Estimate → Write → Done
 *
 * With AgainstBase validation:
 *   ... → Estimate → Validate(vsBase) → Write → Done
 *
 * With AgainstReference validation:
 *   ... → Estimate → LoadReference → Validate(vsRef) → Write → Done
 *
 * On completion (success or failure) sends PipelineCompleted/PipelineFailed
 * to notifyRef before stopping.
 */
object PipelineCoordinator:

  import clustering.KShapeClusterer
  import preprocessing.TimeSeriesPreprocessor
  import warping.WarpingPathSupervisor
  import estimation.EstimationSupervisor
  import validation.EstimationValidator
  import data.{ClickHouseLoader, FileDataLoader, ClickHouseWriter, CheckpointManager}

  type CoordMsg = PipelineCommand | DataLoaderReply | PreprocessorReply |
                  ClustererReply | WarpingSupervisorReply |
                  EstimationSupervisorReply | ValidatorReply | WriterReply

  def apply(
      config:    SimEDaPEConfig,
      notifyRef: ActorRef[PipelineEvent],
  ): Behavior[CoordMsg] =
    Behaviors.setup { ctx =>
      val loader    = spawnLoader(ctx, config)
      val preproc   = ctx.spawn(TimeSeriesPreprocessor(config), "preprocessor")
      val clusterer = ctx.spawn(KShapeClusterer(config), "clusterer")
      val warpSup   = ctx.spawn(WarpingPathSupervisor(config), "warping-supervisor")
      val estimSup  = ctx.spawn(EstimationSupervisor(config), "estimation-supervisor")
      val validator = ctx.spawn(EstimationValidator(config), "validator")
      val writer    = ctx.spawn(ClickHouseWriter(config), "clickhouse-writer")
      idle(ctx, config, notifyRef, loader, preproc, clusterer, warpSup, estimSup, validator, writer)
    }

  // ── state: idle ─────────────────────────────────────────────────────────

  private def idle(
      ctx:       ActorContext[?],
      cfg:       SimEDaPEConfig,
      notify:    ActorRef[PipelineEvent],
      loader:    ActorRef[DataLoaderCommand],
      preproc:   ActorRef[PreprocessorCommand],
      clusterer: ActorRef[ClustererCommand],
      warpSup:   ActorRef[WarpingSupervisorCommand],
      estimSup:  ActorRef[EstimationSupervisorCommand],
      validator: ActorRef[ValidatorCommand],
      writer:    ActorRef[WriterCommand],
  ): Behavior[CoordMsg] =
    Behaviors.receiveMessage {
      case StartPipeline =>
        cfg.startFromPhase match

          case Some(PipelinePhase.Cluster) =>
            CheckpointManager.loadPreprocessed(cfg.checkpointDir, cfg.baseSimulationId, cfg.timeWindow) match
              case Some(preprocessed) =>
                ctx.log.info("[resume] Loaded preprocessed checkpoint → clustering (k={})", cfg.nClusters)
                clusterer ! RunClustering(preprocessed, ctx.self.asInstanceOf[ActorRef[ClustererReply]])
                waitingForClustering(ctx, cfg, notify, loader, preproc, clusterer, warpSup, estimSup, validator, writer, preprocessed)
              case None =>
                ctx.log.warn("[resume] No preprocessed checkpoint found — starting from scratch")
                loader ! LoadBaseData(ctx.self.asInstanceOf[ActorRef[DataLoaderReply]])
                waitingForBaseData(ctx, cfg, notify, loader, preproc, clusterer, warpSup, estimSup, validator, writer)

          case Some(PipelinePhase.WarpingPaths) =>
            (CheckpointManager.loadPreprocessed(cfg.checkpointDir, cfg.baseSimulationId, cfg.timeWindow),
             CheckpointManager.loadClustering(cfg.checkpointDir, cfg.baseSimulationId, cfg.timeWindow)) match
              case (Some(preprocessed), Some(clustering)) =>
                ctx.log.info("[resume] Loaded clustering checkpoint → DTW ({} clusters)", clustering.nClusters)
                warpSup ! ComputeWarpingPaths(clustering, preprocessed, ctx.self.asInstanceOf[ActorRef[WarpingSupervisorReply]], notify)
                waitingForWarpingPaths(ctx, cfg, notify, loader, preproc, clusterer, warpSup, estimSup, validator, writer, preprocessed, clustering)
              case _ =>
                ctx.log.warn("[resume] Missing checkpoints for WarpingPaths — starting from scratch")
                loader ! LoadBaseData(ctx.self.asInstanceOf[ActorRef[DataLoaderReply]])
                waitingForBaseData(ctx, cfg, notify, loader, preproc, clusterer, warpSup, estimSup, validator, writer)

          case Some(PipelinePhase.LoadPartial) | Some(PipelinePhase.PreprocessPartial) | Some(PipelinePhase.Estimation) =>
            (CheckpointManager.loadPreprocessed(cfg.checkpointDir, cfg.baseSimulationId, cfg.timeWindow),
             CheckpointManager.loadClustering(cfg.checkpointDir, cfg.baseSimulationId, cfg.timeWindow),
             CheckpointManager.loadWarpingPaths(cfg.checkpointDir, cfg.baseSimulationId, cfg.timeWindow)) match
              case (Some(preprocessed), Some(clustering), Some(warpingPaths)) =>
                val simPointKeys = clustering.simPointIndices.map(preprocessed.keys).toSet
                ctx.log.info("[resume] Loaded warping paths checkpoint → loading partial ({} sim-points)", simPointKeys.size)
                loader ! LoadPartialData(simPointKeys, ctx.self.asInstanceOf[ActorRef[DataLoaderReply]])
                waitingForPartialData(ctx, cfg, notify, loader, preproc, clusterer, warpSup, estimSup, validator, writer, preprocessed, clustering, warpingPaths)
              case _ =>
                ctx.log.warn("[resume] Missing checkpoints for Estimation — starting from scratch")
                loader ! LoadBaseData(ctx.self.asInstanceOf[ActorRef[DataLoaderReply]])
                waitingForBaseData(ctx, cfg, notify, loader, preproc, clusterer, warpSup, estimSup, validator, writer)

          case _ =>
            ctx.log.info("Step 1 — loading base simulation '{}'", cfg.baseSimulationId)
            notify ! StageStarted(PipelinePhase.LoadBase, Instant.now())
            loader ! LoadBaseData(ctx.self.asInstanceOf[ActorRef[DataLoaderReply]])
            waitingForBaseData(ctx, cfg, notify, loader, preproc, clusterer, warpSup, estimSup, validator, writer)

      case other =>
        ctx.log.warn("Unexpected message in idle: {}", other)
        Behaviors.same
    }

  // ── state: waiting for base data ────────────────────────────────────────

  private def waitingForBaseData(
      ctx:       ActorContext[?],
      cfg:       SimEDaPEConfig,
      notify:    ActorRef[PipelineEvent],
      loader:    ActorRef[DataLoaderCommand],
      preproc:   ActorRef[PreprocessorCommand],
      clusterer: ActorRef[ClustererCommand],
      warpSup:   ActorRef[WarpingSupervisorCommand],
      estimSup:  ActorRef[EstimationSupervisorCommand],
      validator: ActorRef[ValidatorCommand],
      writer:    ActorRef[WriterCommand],
  ): Behavior[CoordMsg] =
    Behaviors.receiveMessage {
      case BaseDataLoaded(dataset) =>
        notify ! StageFinished(PipelinePhase.LoadBase, Instant.now(), Some(s"${dataset.size} series"))
        val ds = filterByWindow(dataset, cfg.timeWindow)
        ctx.log.info("Step 2 — preprocessing {} base series", ds.size)
        notify ! StageStarted(PipelinePhase.PreprocessBase, Instant.now())
        preproc ! PreprocessData(ds, ctx.self.asInstanceOf[ActorRef[PreprocessorReply]])
        waitingForPreprocessedBase(ctx, cfg, notify, loader, preproc, clusterer, warpSup, estimSup, validator, writer)
      case DataLoadFailed(cause) =>
        ctx.log.error("Failed to load base data", cause)
        fail(notify, cause)
      case _ => Behaviors.same
    }

  // ── state: waiting for preprocessed base ────────────────────────────────

  private def waitingForPreprocessedBase(
      ctx:       ActorContext[?],
      cfg:       SimEDaPEConfig,
      notify:    ActorRef[PipelineEvent],
      loader:    ActorRef[DataLoaderCommand],
      preproc:   ActorRef[PreprocessorCommand],
      clusterer: ActorRef[ClustererCommand],
      warpSup:   ActorRef[WarpingSupervisorCommand],
      estimSup:  ActorRef[EstimationSupervisorCommand],
      validator: ActorRef[ValidatorCommand],
      writer:    ActorRef[WriterCommand],
  ): Behavior[CoordMsg] =
    Behaviors.receiveMessage {
      case DataPreprocessed(preprocessed) =>
        CheckpointManager.savePreprocessed(cfg.checkpointDir, cfg.baseSimulationId, cfg.timeWindow, preprocessed)
        notify ! StageFinished(PipelinePhase.PreprocessBase, Instant.now(), Some(s"${preprocessed.keys.length} series"))
        ctx.log.info("Step 3 — running KShape clustering (k={})", cfg.nClusters)
        notify ! StageStarted(PipelinePhase.Cluster, Instant.now())
        clusterer ! RunClustering(preprocessed, ctx.self.asInstanceOf[ActorRef[ClustererReply]])
        waitingForClustering(ctx, cfg, notify, loader, preproc, clusterer, warpSup, estimSup, validator, writer, preprocessed)
      case PreprocessFailed(cause) =>
        ctx.log.error("Preprocessing of base data failed", cause)
        fail(notify, cause)
      case _ => Behaviors.same
    }

  // ── state: waiting for clustering ───────────────────────────────────────

  private def waitingForClustering(
      ctx:         ActorContext[?],
      cfg:         SimEDaPEConfig,
      notify:      ActorRef[PipelineEvent],
      loader:      ActorRef[DataLoaderCommand],
      preproc:     ActorRef[PreprocessorCommand],
      clusterer:   ActorRef[ClustererCommand],
      warpSup:     ActorRef[WarpingSupervisorCommand],
      estimSup:    ActorRef[EstimationSupervisorCommand],
      validator:   ActorRef[ValidatorCommand],
      writer:      ActorRef[WriterCommand],
      baseDataset: PreprocessedDataset,
  ): Behavior[CoordMsg] =
    Behaviors.receiveMessage {
      case ClusteringDone(clustering) =>
        CheckpointManager.saveClustering(cfg.checkpointDir, cfg.baseSimulationId, cfg.timeWindow, clustering)
        notify ! StageFinished(PipelinePhase.Cluster, Instant.now(), Some(s"${clustering.nClusters} clusters"))
        notify ! ClusteringSnapshot(clustering, baseDataset.keys, cfg.seriesLength, cfg.intervalSize)
        if cfg.stopAfterPhase.contains(PipelinePhase.Cluster) then
          ctx.log.info("[checkpoint] Clustering saved. Stopping after phase: Cluster")
          notify ! PipelineCompleted(None)
          Behaviors.stopped
        else
          ctx.log.info("Step 4 — computing DTW warping paths ({} clusters)", clustering.nClusters)
          notify ! StageStarted(PipelinePhase.WarpingPaths, Instant.now())
          warpSup ! ComputeWarpingPaths(
            clustering, baseDataset,
            ctx.self.asInstanceOf[ActorRef[WarpingSupervisorReply]],
            notify,
          )
          waitingForWarpingPaths(ctx, cfg, notify, loader, preproc, clusterer, warpSup, estimSup, validator, writer,
            baseDataset, clustering)
      case ClusteringFailed(cause) =>
        ctx.log.error("Clustering failed", cause)
        fail(notify, cause)
      case _ => Behaviors.same
    }

  // ── state: waiting for warping paths ────────────────────────────────────

  private def waitingForWarpingPaths(
      ctx:         ActorContext[?],
      cfg:         SimEDaPEConfig,
      notify:      ActorRef[PipelineEvent],
      loader:      ActorRef[DataLoaderCommand],
      preproc:     ActorRef[PreprocessorCommand],
      clusterer:   ActorRef[ClustererCommand],
      warpSup:     ActorRef[WarpingSupervisorCommand],
      estimSup:    ActorRef[EstimationSupervisorCommand],
      validator:   ActorRef[ValidatorCommand],
      writer:      ActorRef[WriterCommand],
      baseDataset: PreprocessedDataset,
      clustering:  ClusteringResult,
  ): Behavior[CoordMsg] =
    Behaviors.receiveMessage {
      case WarpingPathsDone(paths) =>
        CheckpointManager.saveWarpingPaths(cfg.checkpointDir, cfg.baseSimulationId, cfg.timeWindow, paths)
        notify ! StageFinished(PipelinePhase.WarpingPaths, Instant.now(), Some(s"${paths.paths.size} cluster paths"))
        if cfg.stopAfterPhase.contains(PipelinePhase.WarpingPaths) then
          ctx.log.info("[checkpoint] Warping paths saved. Stopping after phase: WarpingPaths")
          notify ! PipelineCompleted(None)
          Behaviors.stopped
        else
          val simPointKeys = clustering.simPointIndices.map(baseDataset.keys).toSet
          ctx.log.info("Step 5 — loading partial simulation '{}' ({} sim-points)",
            cfg.partialSimulationId, simPointKeys.size)
          notify ! StageStarted(PipelinePhase.LoadPartial, Instant.now())
          loader ! LoadPartialData(simPointKeys, ctx.self.asInstanceOf[ActorRef[DataLoaderReply]])
          waitingForPartialData(ctx, cfg, notify, loader, preproc, clusterer, warpSup, estimSup, validator, writer,
            baseDataset, clustering, paths)
      case WarpingFailed(cause) =>
        ctx.log.error("Warping path computation failed", cause)
        fail(notify, cause)
      case _ => Behaviors.same
    }

  // ── state: waiting for partial data ─────────────────────────────────────

  private def waitingForPartialData(
      ctx:          ActorContext[?],
      cfg:          SimEDaPEConfig,
      notify:       ActorRef[PipelineEvent],
      loader:       ActorRef[DataLoaderCommand],
      preproc:      ActorRef[PreprocessorCommand],
      clusterer:    ActorRef[ClustererCommand],
      warpSup:      ActorRef[WarpingSupervisorCommand],
      estimSup:     ActorRef[EstimationSupervisorCommand],
      validator:    ActorRef[ValidatorCommand],
      writer:       ActorRef[WriterCommand],
      baseDataset:  PreprocessedDataset,
      clustering:   ClusteringResult,
      warpingPaths: WarpingPathsResult,
  ): Behavior[CoordMsg] =
    Behaviors.receiveMessage {
      case PartialDataLoaded(partialRaw) =>
        notify ! StageFinished(PipelinePhase.LoadPartial, Instant.now(), Some(s"${partialRaw.size} series"))
        val ds = filterByWindow(partialRaw, cfg.timeWindow)
        ctx.log.info("Step 6 — preprocessing partial simulation ({} series)", ds.size)
        notify ! StageStarted(PipelinePhase.PreprocessPartial, Instant.now())
        preproc ! PreprocessData(ds, ctx.self.asInstanceOf[ActorRef[PreprocessorReply]])
        waitingForPreprocessedPartial(ctx, cfg, notify, loader, preproc, clusterer, warpSup, estimSup, validator, writer,
          baseDataset, clustering, warpingPaths)
      case DataLoadFailed(cause) =>
        ctx.log.error("Failed to load partial data", cause)
        fail(notify, cause)
      case _ => Behaviors.same
    }

  // ── state: waiting for preprocessed partial ─────────────────────────────

  private def waitingForPreprocessedPartial(
      ctx:          ActorContext[?],
      cfg:          SimEDaPEConfig,
      notify:       ActorRef[PipelineEvent],
      loader:       ActorRef[DataLoaderCommand],
      preproc:      ActorRef[PreprocessorCommand],
      clusterer:    ActorRef[ClustererCommand],
      warpSup:      ActorRef[WarpingSupervisorCommand],
      estimSup:     ActorRef[EstimationSupervisorCommand],
      validator:    ActorRef[ValidatorCommand],
      writer:       ActorRef[WriterCommand],
      baseDataset:  PreprocessedDataset,
      clustering:   ClusteringResult,
      warpingPaths: WarpingPathsResult,
  ): Behavior[CoordMsg] =
    Behaviors.receiveMessage {
      case DataPreprocessed(partialDataset) =>
        notify ! StageFinished(PipelinePhase.PreprocessPartial, Instant.now(), Some(s"${partialDataset.keys.length} series"))
        ctx.log.info("Step 7/8 — estimating {} non-simulated series",
          baseDataset.size - partialDataset.size)
        notify ! StageStarted(PipelinePhase.Estimation, Instant.now())
        estimSup ! RunEstimation(
          clustering, baseDataset, partialDataset, warpingPaths,
          ctx.self.asInstanceOf[ActorRef[EstimationSupervisorReply]],
          notify,
        )
        waitingForEstimation(ctx, cfg, notify, loader, validator, writer, baseDataset)
      case PreprocessFailed(cause) =>
        ctx.log.error("Preprocessing of partial data failed", cause)
        fail(notify, cause)
      case _ => Behaviors.same
    }

  // ── state: waiting for estimation result ────────────────────────────────

  private def waitingForEstimation(
      ctx:         ActorContext[?],
      cfg:         SimEDaPEConfig,
      notify:      ActorRef[PipelineEvent],
      loader:      ActorRef[DataLoaderCommand],
      validator:   ActorRef[ValidatorCommand],
      writer:      ActorRef[WriterCommand],
      baseDataset: PreprocessedDataset,
  ): Behavior[CoordMsg] =
    Behaviors.receiveMessage {
      case EstimationDone(result) =>
        logMetrics(ctx, result)
        notify ! StageFinished(PipelinePhase.Estimation, Instant.now(), Some(s"${result.estimatedSeries.size} series estimated"))
        notify ! EstimationSnapshot(result)
        cfg.validation match

          case ValidationMode.Disabled =>
            ctx.log.info("Step 8/8 — writing {} estimated series (validation disabled)",
              result.estimatedSeries.size)
            notify ! StageStarted(PipelinePhase.Write, Instant.now())
            writer ! WriteEstimations(result, cfg.outputSimulationId,
              ctx.self.asInstanceOf[ActorRef[WriterReply]])
            waitingForWrite(ctx, notify, None)

          case ValidationMode.AgainstBase =>
            ctx.log.info("Step 8/9 — validating {} links against base simulation",
              result.estimatedSeries.size)
            notify ! StageStarted(PipelinePhase.Estimation, Instant.now())
            val refValues = denormalizeKeys(result.estimatedSeries.keySet, baseDataset)
            validator ! RunValidation(
              result.estimatedSeries, refValues, cfg.baseSimulationId, "AgainstBase",
              ctx.self.asInstanceOf[ActorRef[ValidatorReply]],
            )
            waitingForValidation(ctx, cfg, notify, writer, result)

          case ValidationMode.AgainstReference =>
            ctx.log.info("Step 8/10 — loading reference simulation '{}' for validation",
              cfg.validationReferenceId)
            loader ! LoadSimulationData(
              cfg.validationReferenceId,
              ctx.self.asInstanceOf[ActorRef[DataLoaderReply]],
            )
            waitingForRefData(ctx, cfg, notify, validator, writer, result)

      case EstimationFailed(cause) =>
        ctx.log.error("Estimation failed", cause)
        fail(notify, cause)

      case _ => Behaviors.same
    }

  // ── state: waiting for reference simulation (AgainstReference only) ─────

  private def waitingForRefData(
      ctx:        ActorContext[?],
      cfg:        SimEDaPEConfig,
      notify:     ActorRef[PipelineEvent],
      validator:  ActorRef[ValidatorCommand],
      writer:     ActorRef[WriterCommand],
      estimation: EstimationResult,
  ): Behavior[CoordMsg] =
    Behaviors.receiveMessage {
      case BaseDataLoaded(refDataset) =>
        val refValues = estimation.estimatedSeries.keys.flatMap { key =>
          refDataset.get(key).map(ts => key -> ts.values)
        }.toMap
        ctx.log.info("Step 9/10 — validating {} links against reference '{}'",
          refValues.size, cfg.validationReferenceId)
        validator ! RunValidation(
          estimation.estimatedSeries, refValues, cfg.validationReferenceId, "AgainstReference",
          ctx.self.asInstanceOf[ActorRef[ValidatorReply]],
        )
        waitingForValidation(ctx, cfg, notify, writer, estimation)

      case DataLoadFailed(cause) =>
        // Reference load failed — log, skip validation, continue to write
        ctx.log.warn("Failed to load reference simulation for validation (writing without validation): {}",
          cause.getMessage)
        writer ! WriteEstimations(estimation, cfg.outputSimulationId,
          ctx.self.asInstanceOf[ActorRef[WriterReply]])
        waitingForWrite(ctx, notify, None)

      case _ => Behaviors.same
    }

  // ── state: waiting for validation result ────────────────────────────────

  private def waitingForValidation(
      ctx:        ActorContext[?],
      cfg:        SimEDaPEConfig,
      notify:     ActorRef[PipelineEvent],
      writer:     ActorRef[WriterCommand],
      estimation: EstimationResult,
  ): Behavior[CoordMsg] =
    Behaviors.receiveMessage {
      case ValidationDone(result) =>
        val agg = result.aggregate
        ctx.log.info(
          "Validation OK — MAE={:.4f} RMSE={:.4f} MAPE={:.2f}% DTW={:.4f} r={:.4f} (n={})",
          agg.meanMAE, agg.meanRMSE, agg.meanMAPE, agg.meanDTW, agg.meanPearsonR, agg.nLinks,
        )
        notify ! StageFinished(PipelinePhase.Estimation, Instant.now(), Some(s"MAE=${agg.meanMAE} RMSE=${agg.meanRMSE}"))
        notify ! ValidationSnapshot(result)
        notify ! StageStarted(PipelinePhase.Write, Instant.now())
        writer ! WriteEstimations(estimation, cfg.outputSimulationId,
          ctx.self.asInstanceOf[ActorRef[WriterReply]])
        waitingForWrite(ctx, notify, Some(result))

      case ValidationFailed(cause) =>
        ctx.log.warn("Validation computation failed (writing without validation metrics): {}",
          cause.getMessage)
        writer ! WriteEstimations(estimation, cfg.outputSimulationId,
          ctx.self.asInstanceOf[ActorRef[WriterReply]])
        waitingForWrite(ctx, notify, None)

      case _ => Behaviors.same
    }

  // ── state: waiting for write ─────────────────────────────────────────────

  private def waitingForWrite(
      ctx:        ActorContext[?],
      notify:     ActorRef[PipelineEvent],
      validation: Option[ValidationResult],
  ): Behavior[CoordMsg] =
    Behaviors.receiveMessage {
      case WriteCompleted =>
        ctx.log.info("SimEDaPE pipeline finished successfully.")
        notify ! StageFinished(PipelinePhase.Write, Instant.now())
        notify ! PipelineCompleted(validation)
        Behaviors.stopped

      case WriteFailed(cause) =>
        ctx.log.error("Writing results failed", cause)
        notify ! PipelineFailed(cause)
        Behaviors.stopped

      case _ => Behaviors.same
    }

  // ── helpers ──────────────────────────────────────────────────────────────

  private def fail(notify: ActorRef[PipelineEvent], cause: Throwable): Behavior[CoordMsg] =
    notify ! PipelineFailed(cause)
    Behaviors.stopped

  private def logMetrics(ctx: ActorContext[?], result: EstimationResult): Unit =
    val m = result.metrics
    ctx.log.info(
      "Step 8 — estimation done: {} series | speed={:.3f} m/s | occupancy={:.2f}% | vehicles={:.1f} | km={:.1f}",
      result.estimatedSeries.size, m.avgSpeed, m.occupancy, m.avgVehicles, m.kmTraveled,
    )

  /** Denormalize a subset of keys from a PreprocessedDataset back to raw counts. */
  private def denormalizeKeys(
      keys:    Iterable[String],
      dataset: PreprocessedDataset,
  ): Map[String, Array[Double]] =
    val keyToIdx = dataset.keys.zipWithIndex.toMap
    keys.flatMap { key =>
      keyToIdx.get(key).map { idx =>
        val raw = dataset.normalised(idx).map(v => v * dataset.stds(idx) + dataset.means(idx))
        key -> raw
      }
    }.toMap

  private def filterByWindow(
      dataset: TimeSeriesDataset,
      window:  Option[TimeWindow],
  ): TimeSeriesDataset =
    window match
      case None     => dataset
      case Some(tw) =>
        val filtered = dataset.series.filter(ts => tw.contains(ts.interval))
        TimeSeriesDataset.build(dataset.simulationId, filtered)

  private def spawnLoader(
      ctx: ActorContext[?],
      cfg: SimEDaPEConfig,
  ): ActorRef[DataLoaderCommand] =
    cfg.dataSource match
      case DataSource.File(_)    => ctx.spawn(FileDataLoader(cfg), "file-loader")
      case DataSource.ClickHouse => ctx.spawn(ClickHouseLoader(cfg), "clickhouse-loader")
