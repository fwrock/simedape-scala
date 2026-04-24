package org.interscity.htc
package api

import org.apache.pekko.actor.typed.{ActorRef, ActorSystem, Scheduler}
import org.apache.pekko.actor.typed.scaladsl.AskPattern.*
import org.apache.pekko.http.scaladsl.model.{HttpMethods, HttpResponse, StatusCodes}
import org.apache.pekko.http.scaladsl.model.HttpMethods.*
import org.apache.pekko.http.scaladsl.model.headers.{
  `Access-Control-Allow-Headers`,
  `Access-Control-Allow-Methods`,
  `Access-Control-Allow-Origin`,
}
import org.apache.pekko.http.scaladsl.server.Directives.*
import org.apache.pekko.http.scaladsl.server.Route
import org.apache.pekko.http.scaladsl.marshallers.sprayjson.SprayJsonSupport.*
import org.apache.pekko.util.Timeout

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration.*
import scala.util.{Failure, Success}

import core.{SimEDaPEConfig, DataSource}
import api.ApiProtocol.*
import JsonProtocol.given

/**
 * Pekko HTTP route definitions for the SimEDaPE REST API.
 *
 *  GET   /health
 *  GET   /api/v1/pipeline/status
 *  POST  /api/v1/pipeline/start     — body: StartPipelineRequest (can be {})
 *  POST  /api/v1/pipeline/cancel
 *  GET   /api/v1/config             — read current default config (password masked)
 *  PATCH /api/v1/config             — partial update of default config
 */
object Routes:

  private val corsHeaders = List(
    `Access-Control-Allow-Origin`.*,
    `Access-Control-Allow-Methods`(GET, POST, PATCH, OPTIONS, DELETE),
    `Access-Control-Allow-Headers`("Content-Type", "Authorization", "Accept"),
  )

  def apply(
      gateway: ActorRef[GatewayCommand],
  )(using system: ActorSystem[?]): Route =

    given Timeout         = Timeout(10.seconds)
    given Scheduler       = system.scheduler
    given ExecutionContext = system.executionContext

    options {
      complete(HttpResponse(StatusCodes.OK, headers = corsHeaders))
    } ~
    respondWithHeaders(corsHeaders) {
      concat(

      // ── GET /health ──────────────────────────────────────────────────────
      path("health") {
        get {
          complete(HealthResponse("ok", "1.0.0"))
        }
      },

      pathPrefix("api" / "v1") {
        concat(

          // ── GET /api/v1/pipeline/status ─────────────────────────────────
          path("pipeline" / "status") {
            get {
              onComplete(gateway.ask(ApiGetStatus(_))) {
                case Success(StatusReply(info)) =>
                  complete(toStatusResponse(info))
                case Failure(ex) =>
                  complete(StatusCodes.ServiceUnavailable, ErrorResponse("Gateway unavailable", Some(ex.getMessage)))
                case _ =>
                  complete(StatusCodes.InternalServerError, ErrorResponse("Unexpected reply"))
              }
            }
          },

          // ── POST /api/v1/pipeline/start ─────────────────────────────────
          path("pipeline" / "start") {
            post {
              entity(as[StartPipelineRequest]) { req =>
                onComplete(gateway.ask(ApiStart(req, _))) {
                  case Success(Started(jobId)) =>
                    complete(StatusCodes.Accepted, StartPipelineResponse(jobId, "running"))
                  case Success(AlreadyRunning(jobId)) =>
                    complete(StatusCodes.Conflict, ErrorResponse("Pipeline already running", Some(s"job=$jobId")))
                  case Failure(ex) =>
                    complete(StatusCodes.ServiceUnavailable, ErrorResponse("Gateway unavailable", Some(ex.getMessage)))
                  case _ =>
                    complete(StatusCodes.InternalServerError, ErrorResponse("Unexpected reply"))
                }
              }
            }
          },

          // ── POST /api/v1/pipeline/cancel ────────────────────────────────
          path("pipeline" / "cancel") {
            post {
              onComplete(gateway.ask(ApiCancel(_))) {
                case Success(Cancelled(jobId)) =>
                  complete(CancelResponse(jobId, accepted = true))
                case Success(NotRunning) =>
                  complete(StatusCodes.NotFound, ErrorResponse("No pipeline is currently running"))
                case Failure(ex) =>
                  complete(StatusCodes.ServiceUnavailable, ErrorResponse("Gateway unavailable", Some(ex.getMessage)))
                case _ =>
                  complete(StatusCodes.InternalServerError, ErrorResponse("Unexpected reply"))
              }
            }
          },

          // ── GET  /api/v1/config ─────────────────────────────────────────
          // ── PATCH /api/v1/config ────────────────────────────────────────
          path("config") {
            get {
              onComplete(gateway.ask(ApiGetConfig(_))) {
                case Success(ConfigReply(cfg)) =>
                  complete(toConfigResponse(cfg))
                case Failure(ex) =>
                  complete(StatusCodes.ServiceUnavailable, ErrorResponse("Gateway unavailable", Some(ex.getMessage)))
                case _ =>
                  complete(StatusCodes.InternalServerError, ErrorResponse("Unexpected reply"))
              }
            } ~
            patch {
              entity(as[UpdateConfigRequest]) { req =>
                val result: Future[GatewayReply] =
                  gateway.ask[GatewayReply](ApiUpdateConfig(req, _)).flatMap { _ =>
                    gateway.ask[GatewayReply](ApiGetConfig(_))
                  }
                onComplete(result) {
                  case Success(ConfigReply(cfg)) =>
                    complete(toConfigResponse(cfg))
                  case Failure(ex) =>
                    complete(StatusCodes.ServiceUnavailable, ErrorResponse("Gateway unavailable", Some(ex.getMessage)))
                  case _ =>
                    complete(StatusCodes.InternalServerError, ErrorResponse("Unexpected reply"))
                }
              }
            }
          },

        )
      }
    ) // closes outer concat
    } // end respondWithHeaders

  // ─── DTO conversion helpers ───────────────────────────────────────────────

  private def toStatusResponse(info: PipelineRunInfo): PipelineStatusResponse =
    PipelineStatusResponse(
      jobId       = info.jobId,
      state       = info.state.toString.toLowerCase,
      startedAt   = info.startedAt.map(_.toString),
      completedAt = info.completedAt.map(_.toString),
      error       = info.error,
    )

  private def toConfigResponse(cfg: SimEDaPEConfig): ConfigResponse =
    val (src, dir) = cfg.dataSource match
      case DataSource.File(d) => ("file", Some(d))
      case DataSource.ClickHouse => ("clickhouse", None)
    ConfigResponse(
      baseSimulationId      = cfg.baseSimulationId,
      partialSimulationId   = cfg.partialSimulationId,
      outputSimulationId    = cfg.outputSimulationId,
      nClusters             = cfg.nClusters,
      kshapeMaxIter         = cfg.kshapeMaxIter,
      kshapeNInit           = cfg.kshapeNInit,
      estimationMethod      = cfg.method.toString,
      percentSample         = cfg.percentSample,
      dtwWindowFraction     = cfg.dtwWindowFraction,
      pcaComponents         = cfg.pcaComponents,
      dataSource            = src,
      dataDir               = dir,
      warpingPathsDir       = cfg.warpingPathsDir,
      checkpointDir         = cfg.checkpointDir,
      seriesLength          = cfg.seriesLength,
      intervalSize          = cfg.intervalSize,
      workerParallelism     = cfg.workerParallelism,
      validationMode        = cfg.validation.toString,
      validationReferenceId = cfg.validationReferenceId,
      timeWindowStart       = cfg.timeWindow.map(_.startHour),
      timeWindowEnd         = cfg.timeWindow.map(_.endHour),
    )
