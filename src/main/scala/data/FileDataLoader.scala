package org.interscity.htc
package data

import org.apache.pekko.actor.typed.*
import org.apache.pekko.actor.typed.scaladsl.*

import com.github.tototoshi.csv.CSVReader
import java.io.File
import scala.util.Try

import core.{SimEDaPEConfig, DataSource}
import core.Protocol.*
import data.Models.*

/**
 * Loads time series from CSV files produced by InterSCSimulator.
 *
 * Expected layout: one CSV file per link, named `<linkId>.csv`,
 * under `cfg.dataSource.asInstanceOf[DataSource.File].directory`.
 *
 * CSV columns: time, vehicle_id, vehicle_count_on_link
 *
 * Each file is grouped by time interval and resampled to `seriesLength`.
 */
object FileDataLoader:

  def apply(cfg: SimEDaPEConfig): Behavior[DataLoaderCommand] =
    Behaviors.setup { ctx =>
      val dir = cfg.dataSource.asInstanceOf[DataSource.File].directory
      ctx.log.info("FileDataLoader ready — directory={}", dir)

      Behaviors.receiveMessage {
        case LoadBaseData(replyTo) =>
          ctx.log.info("Loading base simulation from files in '{}'", dir)
          val result = Try(loadFromDirectory(dir, cfg.baseSimulationId, None, cfg))
          result.fold(
            err => replyTo ! DataLoadFailed(err),
            ds  => replyTo ! BaseDataLoaded(ds),
          )
          Behaviors.same

        case LoadPartialData(simPointIds, replyTo) =>
          ctx.log.info("Loading partial simulation from '{}' ({} sim-points)", dir, simPointIds.size)
          val result = Try(loadFromDirectory(dir, cfg.partialSimulationId, Some(simPointIds), cfg))
          result.fold(
            err => replyTo ! DataLoadFailed(err),
            ds  => replyTo ! PartialDataLoaded(ds),
          )
          Behaviors.same

        case LoadSimulationData(simulationId, replyTo) =>
          ctx.log.info("Loading full simulation '{}' from files (validation reference)", simulationId)
          val result = Try(loadFromDirectory(dir, simulationId, None, cfg))
          result.fold(
            err => replyTo ! DataLoadFailed(err),
            ds  => replyTo ! BaseDataLoaded(ds),  // reuse BaseDataLoaded for full loads
          )
          Behaviors.same
      }
    }

  // ── file I/O ──────────────────────────────────────────────────────────────

  private def loadFromDirectory(
      directory: String,
      simulationId: String,
      filterLinkIds: Option[Set[String]],
      cfg: SimEDaPEConfig,
  ): TimeSeriesDataset =
    val root = new File(directory)
    require(root.isDirectory, s"'$directory' is not a directory")
    val csvFiles = root.listFiles()
      .filter(_.getName.endsWith(".csv"))
      .filter { f =>
        filterLinkIds.forall { ids =>
          val linkId = f.getName.stripSuffix(".csv")
          ids.contains(linkId) || ids.exists(_.startsWith(linkId + "-"))
        }
      }

    val allSeries = csvFiles.flatMap { f =>
      val linkId = f.getName.stripSuffix(".csv")
      Try(parseLinkFile(f, linkId, cfg)).getOrElse(Seq.empty)
    }
    TimeSeriesDataset.build(simulationId, allSeries.toSeq)

  /**
   * Parses one CSV file into one or more TimeSeries (one per time interval).
   * Columns: time (Long), vehicle_id (ignored), count (Int)
   */
  private def parseLinkFile(
      file: File,
      linkId: String,
      cfg: SimEDaPEConfig,
  ): Seq[TimeSeries] =
    val reader = CSVReader.open(file)
    try
      // skip header
      val rows = reader.all().drop(1)
      // group by interval index
      val byInterval = rows.groupBy { row =>
        val tick = row.headOption.flatMap(_.toLongOption).getOrElse(0L)
        (tick / (cfg.intervalSize * cfg.seriesLength)).toInt
      }
      byInterval.map { case (interval, intervalRows) =>
        val sorted = intervalRows
          .flatMap(r => for tick <- r.headOption.flatMap(_.toLongOption);
                            cnt  <- r.lastOption.flatMap(_.toIntOption)
                        yield (tick, cnt.toDouble))
          .sortBy(_._1)
        val values = resampleToLength(sorted.map(_._2).toArray, cfg.seriesLength)
        TimeSeries(linkId, interval, values)
      }.toSeq
    finally
      reader.close()

  private def resampleToLength(src: Array[Double], targetLen: Int): Array[Double] =
    if src.isEmpty then Array.fill(targetLen)(0.0)
    else if src.length == targetLen then src
    else
      val out   = new Array[Double](targetLen)
      val scale = (src.length - 1).toDouble / (targetLen - 1).max(1)
      for i <- 0 until targetLen do
        val pos  = i * scale
        val lo   = pos.toInt
        val hi   = (lo + 1).min(src.length - 1)
        val frac = pos - lo
        out(i) = src(lo) * (1.0 - frac) + src(hi) * frac
      out
