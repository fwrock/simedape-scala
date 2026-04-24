package org.interscity.htc
package data

import org.apache.pekko.actor.typed.*
import org.apache.pekko.actor.typed.scaladsl.*
import com.zaxxer.hikari.{HikariConfig, HikariDataSource}

import java.sql.{Connection, PreparedStatement, Timestamp}
import scala.collection.mutable
import scala.util.{Try, Using}

import core.{SimEDaPEConfig, ClickHouseConfig}
import core.Protocol.*
import data.Models.*

/**
 * Writes estimated time series back to ClickHouse `link_vehicle_counts`
 * under a new `simulation_id` (the `outputSimulationId` from config).
 *
 * Uses batch inserts for efficiency — ClickHouse performs best with
 * large, columnar batches rather than individual row inserts.
 */
object ClickHouseWriter:

  private val BatchSize = 50_000

  def apply(cfg: SimEDaPEConfig): Behavior[WriterCommand] =
    Behaviors.setup { ctx =>
      val pool = buildPool(cfg.clickhouse)
      ctx.log.info("ClickHouseWriter ready")

      Behaviors
        .receiveMessage[WriterCommand] {
          case WriteEstimations(result, simulationId, replyTo) =>
            ctx.log.info("Writing {} estimated series for simulation '{}'",
              result.estimatedSeries.size, simulationId)
            val attempt = Try(writeAll(pool, simulationId, result, cfg))
            attempt.fold(
              err => replyTo ! WriteFailed(err),
              _   => replyTo ! WriteCompleted,
            )
            Behaviors.same
        }
        .receiveSignal { case (_, PostStop) =>
          pool.close()
          Behaviors.same
        }
    }

  // ─── SQL ──────────────────────────────────────────────────────────────────

  private val InsertSQL =
    """
      |INSERT INTO link_vehicle_counts
      |  (simulation_id, link_id, tick, enters, leaves, peak_count)
      |VALUES (?, ?, ?, 0, 0, ?)
      |""".stripMargin

  private def writeAll(
      pool: HikariDataSource,
      simulationId: String,
      result: EstimationResult,
      cfg: SimEDaPEConfig,
  ): Unit =
    Using.resource(pool.getConnection()) { conn =>
      conn.setAutoCommit(false)
      Using.resource(conn.prepareStatement(InsertSQL)) { ps =>
        var count = 0
        for (key, values) <- result.estimatedSeries do
          // key format: "linkId-interval"
          val parts    = key.split("-", 2)
          val linkId   = parts(0)
          val interval = if parts.length > 1 then parts(1).toIntOption.getOrElse(0) else 0
          val tickBase = interval.toLong * cfg.seriesLength * cfg.intervalSize
          values.zipWithIndex.foreach { case (v, i) =>
            val rawCount = v.max(0.0).round.toInt
            ps.setString(1, simulationId)
            ps.setString(2, linkId)
            ps.setLong(3, tickBase + i * cfg.intervalSize)
            ps.setInt(4, rawCount)
            ps.addBatch()
            count += 1
            if count % BatchSize == 0 then
              ps.executeBatch()
              conn.commit()
          }
        if count % BatchSize != 0 then
          ps.executeBatch()
          conn.commit()
      }
    }

  private def buildPool(cfg: ClickHouseConfig): HikariDataSource =
    val hc = new HikariConfig()
    hc.setJdbcUrl(s"jdbc:ch://${cfg.host}:${cfg.port}/${cfg.database}?socket_timeout=3600000")
    hc.setUsername(cfg.user)
    hc.setPassword(cfg.password)
    hc.setMaximumPoolSize(cfg.poolSize)
    hc.setDriverClassName("com.clickhouse.jdbc.ClickHouseDriver")
    new HikariDataSource(hc)
