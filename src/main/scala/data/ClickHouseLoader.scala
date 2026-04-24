package org.interscity.htc
package data

import org.apache.pekko.actor.typed.*
import org.apache.pekko.actor.typed.scaladsl.*
import com.clickhouse.jdbc.ClickHouseDataSource
import com.zaxxer.hikari.{HikariConfig, HikariDataSource}

import java.util.Properties
import java.sql.{Connection, PreparedStatement, ResultSet}
import scala.collection.mutable
import scala.util.{Try, Using}

import core.{SimEDaPEConfig, ClickHouseConfig}
import core.Protocol.*
import data.Models.*

/**
 * Loads time series from ClickHouse `link_vehicle_counts`.
 *
 * Query strategy
 * ──────────────
 * Base load  : SELECT all link_ids for simulation_id = baseSimulationId,
 *              aggregate ticks into series of length cfg.seriesLength.
 * Partial load: Same but restricted to the given simulationPointIds.
 *
 * The table is populated by the materialized view from vehicle_link_events,
 * so `peak_count` is used as the vehicle-count value per tick bucket.
 */
object ClickHouseLoader:

  def apply(cfg: SimEDaPEConfig): Behavior[DataLoaderCommand] =
    Behaviors.setup { ctx =>
      val pool = buildPool(cfg.clickhouse)
      ctx.log.info("ClickHouseLoader ready — {}:{}/{}", cfg.clickhouse.host, cfg.clickhouse.port, cfg.clickhouse.database)

      Behaviors
        .receiveMessage[DataLoaderCommand] {
          case LoadBaseData(replyTo) =>
            ctx.log.info("Loading base simulation '{}'", cfg.baseSimulationId)
            val result = Try(loadSeries(pool, cfg.baseSimulationId, None, cfg))
            result.fold(
              err => { logJdbcError(ctx, err); replyTo ! DataLoadFailed(err) },
              ds  => replyTo ! BaseDataLoaded(ds),
            )
            Behaviors.same

          case LoadPartialData(simPointIds, replyTo) =>
            ctx.log.info("Loading partial simulation '{}' ({} sim-points)",
              cfg.partialSimulationId, simPointIds.size)
            val result = Try(loadSeries(pool, cfg.partialSimulationId, Some(simPointIds), cfg))
            result.fold(
              err => { logJdbcError(ctx, err); replyTo ! DataLoadFailed(err) },
              ds  => replyTo ! PartialDataLoaded(ds),
            )
            Behaviors.same

          case LoadSimulationData(simulationId, replyTo) =>
            ctx.log.info("Loading full simulation '{}' (validation reference)", simulationId)
            val result = Try(loadSeries(pool, simulationId, None, cfg))
            result.fold(
              err => { logJdbcError(ctx, err); replyTo ! DataLoadFailed(err) },
              ds  => replyTo ! BaseDataLoaded(ds),  // reuse BaseDataLoaded for full loads
            )
            Behaviors.same
        }
        .receiveSignal { case (_, PostStop) =>
          pool.close()
          Behaviors.same
        }
    }

  // ── JDBC helpers ──────────────────────────────────────────────────────────

  private val LoadSeriesSQL =
    """
      |SELECT link_id, tick, peak_count
      |FROM link_vehicle_counts
      |WHERE simulation_id = ?
      |ORDER BY link_id, tick
      |""".stripMargin

  private val LoadSeriesForLinksSQL =
    """
      |SELECT link_id, tick, peak_count
      |FROM link_vehicle_counts
      |WHERE simulation_id = ?
      |  AND link_id IN (%s)
      |ORDER BY link_id, tick
      |""".stripMargin

  private def loadSeries(
      pool: HikariDataSource,
      simulationId: String,
      filterLinks: Option[Set[String]],
      cfg: SimEDaPEConfig,
  ): TimeSeriesDataset =
    Using.resource(pool.getConnection()) { conn =>
      val rawRows = executeQuery(conn, simulationId, filterLinks)
      buildDataset(simulationId, rawRows, cfg)
    }

  private def executeQuery(
      conn: Connection,
      simulationId: String,
      filterLinks: Option[Set[String]],
  ): Seq[(String, Long, Int)] =
    val (sql, ps) = filterLinks match
      case None =>
        val s = conn.prepareStatement(LoadSeriesSQL)
        s.setString(1, simulationId)
        (LoadSeriesSQL, s)
      case Some(ids) =>
        val placeholders = ids.map(_ => "?").mkString(",")
        val s = conn.prepareStatement(LoadSeriesForLinksSQL.format(placeholders))
        s.setString(1, simulationId)
        ids.zipWithIndex.foreach { case (id, i) => s.setString(i + 2, id) }
        (LoadSeriesForLinksSQL, s)

    val rows = mutable.ArrayBuffer.empty[(String, Long, Int)]
    Using.resource(ps.executeQuery()) { rs =>
      while rs.next() do
        rows += ((rs.getString("link_id"), rs.getLong("tick"), rs.getInt("peak_count")))
    }
    rows.toSeq

  /**
   * Groups rows by link_id, buckets ticks into intervals of `intervalSize`,
   * and resamples each bucket to `seriesLength` via linear interpolation.
   */
  private def buildDataset(
      simulationId: String,
      rows: Seq[(String, Long, Int)],
      cfg: SimEDaPEConfig,
  ): TimeSeriesDataset =
    // Group by link_id
    val byLink = rows.groupBy(_._1)
    val seriesList = byLink.flatMap { case (linkId, linkRows) =>
      // Group ticks into intervals
      val byInterval = linkRows.groupBy { case (_, tick, _) =>
        (tick / (cfg.intervalSize * cfg.seriesLength)).toInt
      }
      byInterval.map { case (interval, intervalRows) =>
        val sorted = intervalRows.sortBy(_._2)
        // One value per tick within the interval, resampled to seriesLength
        val values = resampleToLength(sorted.map(_._3.toDouble).toArray, cfg.seriesLength)
        TimeSeries(linkId, interval, values)
      }
    }.toSeq
    TimeSeriesDataset.build(simulationId, seriesList)

  /** Linear interpolation / downsampling to target length. */
  private def resampleToLength(src: Array[Double], targetLen: Int): Array[Double] =
    if src.length == targetLen then src
    else
      val out = new Array[Double](targetLen)
      val scale = (src.length - 1).toDouble / (targetLen - 1).max(1)
      for i <- 0 until targetLen do
        val pos = i * scale
        val lo  = pos.toInt
        val hi  = (lo + 1).min(src.length - 1)
        val frac = pos - lo
        out(i) = src(lo) * (1.0 - frac) + src(hi) * frac
      out

  private def logJdbcError(ctx: ActorContext[?], err: Throwable): Unit =
    val sb = new StringBuilder(s"JDBC error: ${err.getClass.getSimpleName}: ${err.getMessage}")
    var cause = err.getCause
    while cause != null do
      sb.append(s"\n  caused by: ${cause.getClass.getSimpleName}: ${cause.getMessage}")
      cause = cause.getCause
    ctx.log.error(sb.toString)

  private def buildPool(cfg: ClickHouseConfig): HikariDataSource =
    val hc = new HikariConfig()
    // jdbc:ch:// is the canonical URL prefix for clickhouse-jdbc 0.6+
    hc.setJdbcUrl(s"jdbc:ch://${cfg.host}:${cfg.port}/${cfg.database}?socket_timeout=3600000&compress=1")
    hc.setUsername(cfg.user)
    hc.setPassword(cfg.password)
    hc.setMaximumPoolSize(cfg.poolSize)
    hc.setConnectionTimeout(30_000)
    hc.setIdleTimeout(600_000)
    hc.setMaxLifetime(1_800_000)
    hc.setDriverClassName("com.clickhouse.jdbc.ClickHouseDriver")
    new HikariDataSource(hc)
