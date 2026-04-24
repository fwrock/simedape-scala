package org.interscity.htc
package api

import org.apache.pekko.actor.typed.ActorRef
import java.time.Instant
import data.Models.ValidationResult
import core.SimEDaPEConfig

/** Actor messages for the HTTP API ↔ PipelineApiGateway bridge. */
object ApiProtocol:

  // ─── Commands received by PipelineApiGateway ─────────────────────────────

  sealed trait GatewayCommand

  final case class ApiStart(
      request: StartPipelineRequest,
      replyTo: ActorRef[GatewayReply],
  ) extends GatewayCommand

  final case class ApiGetStatus(
      replyTo: ActorRef[GatewayReply],
  ) extends GatewayCommand

  final case class ApiCancel(
      replyTo: ActorRef[GatewayReply],
  ) extends GatewayCommand

  /** Retrieve the validation result from the last completed pipeline run. */
  final case class ApiGetValidation(
      replyTo: ActorRef[GatewayReply],
  ) extends GatewayCommand

  final case class ApiGetConfig(
      replyTo: ActorRef[GatewayReply],
  ) extends GatewayCommand

  final case class ApiUpdateConfig(
      request: UpdateConfigRequest,
      replyTo: ActorRef[GatewayReply],
  ) extends GatewayCommand

  // Internal: forwarded from PipelineJobSupervisor → gateway
  private[api] final case class PipelineDone(
      jobId:      String,
      validation: Option[ValidationResult],
  ) extends GatewayCommand
  private[api] final case class PipelineJobError(jobId: String, cause: Throwable) extends GatewayCommand

  // ─── Replies sent back to HTTP route handlers ─────────────────────────────

  sealed trait GatewayReply

  final case class Started(jobId: String)                              extends GatewayReply
  final case class AlreadyRunning(jobId: String)                       extends GatewayReply
  final case class StatusReply(run: PipelineRunInfo)                   extends GatewayReply
  final case class Cancelled(jobId: String)                            extends GatewayReply
  case object NotRunning                                               extends GatewayReply
  final case class ValidationReply(result: Option[ValidationResult])  extends GatewayReply
  final case class ConfigReply(config: SimEDaPEConfig)                extends GatewayReply
  case object ConfigUpdated                                           extends GatewayReply

  // ─── Pipeline run state (immutable snapshot) ──────────────────────────────

  enum RunState:
    case Idle, Running, Completed, Failed

  final case class PipelineRunInfo(
      jobId:       Option[String],
      state:       RunState,
      startedAt:   Option[Instant],
      completedAt: Option[Instant],
      error:       Option[String],
      validation:  Option[ValidationResult],
  )
