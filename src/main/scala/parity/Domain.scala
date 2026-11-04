package org.interscity.htc.parity

import spray.json.*
import Json.*
import java.nio.file.Path

final case class Window(start: Long = 0, end: Long = 86400, width: Long = 3600, period: Int = 0):
  require(start >= 0 && end > start && width > 0 && period >= 0, "Invalid time interval")
  if period > 0 then require(start == 0 && width % period == 0 && (end-start) % width == 0, "Regular occupancy needs start=0 and a sample period dividing the window and horizon")
  def json: JsObject = obj("start_time" -> JsNumber(start), "end_time" -> JsNumber(end), "interval_size" -> JsNumber(width), "sample_period" -> JsNumber(period))

final case class Road(id: String, length: Double, capacity: Double, freeSpeed: Double):
  require(length > 0 && freeSpeed >= 0 && length.isFinite && freeSpeed.isFinite, "Invalid road metadata")
  def json: JsValue = arr(Seq(length,capacity,freeSpeed))
final case class Series(link: String, left: Long, right: Long, values: Array[Double], vehicles: Vector[String]):
  def label: String = s"$link-$left-$right"
final case class Dataset(series: Vector[Series], roads: Map[String,Road], streetVehicles: Map[String,Vector[Vector[String]]], source: String):
  lazy val index: Map[String,Int] = series.iterator.zipWithIndex.map((s,i) => s.label -> i).toMap
  def maxSize: Int = series.iterator.map(_.values.length).maxOption.getOrElse(0)
final case class Prepared(dataset: Dataset, normalized: Array[Array[Double]], means: Array[Double], stds: Array[Double]):
  def raw(i: Int): Array[Double] = normalized(i).map(_ * stds(i) + means(i))
final case class Clusters(labels: Array[Int], centroids: Array[Array[Double]], simulationPoints: Array[Int]):
  def count: Int = centroids.length
  def members(k: Int): Array[Int] = labels.indices.filter(labels(_) == k).toArray

final case class RunConfig(json: JsObject):
  val source: String = json.field("data_source", JsString("csv")).text()
  require(Set("csv","clickhouse").contains(source), "data_source must be csv or clickhouse")
  private val iv = json.field("intervals", empty)
  val window: Window = Window(iv.field("start_time").number(0).toLong,iv.field("end_time").number(86400).toLong,iv.field("interval_size").number(3600).toLong,iv.field("sample_period").int())
  val clustering: JsValue = json.field("clustering",empty)
  val clusters: Int = clustering.field("n_clusters").int(36)
  val iterations: Int = clustering.field("max_iter").int(8)
  val seed: Long = clustering.field("random_state").number(10).toLong
  val clusteringSize: Int = clustering.field("clustering_size").int()
  val name: String = json.field("scenario_name",JsString("default")).text()
  val scenario: String = json.field("scenario_dir").text()
  val streetInfo: String = json.field("street_info_file").text()
  private val csvSeparator=json.field("csv_sep",JsString(";")).text()
  require(csvSeparator.length==1,"csv_sep must contain exactly one character")
  val separator: Char = csvSeparator.head
  val clickhouse: JsValue = json.field("clickhouse",empty)
  def ch(name: String, default: String): String = clickhouse.field(name).text(sys.env.getOrElse(s"HTC_CLICKHOUSE_${name.toUpperCase}",default))
  val simulation: String = clickhouse.field("simulation_id").text()
  val table: String = clickhouse.field("table",JsString("vehicle_link_events")).text()
  val precision: String = json.field("model_precision",JsString("float64")).text()
  require(Set("float32","float64").contains(precision), "Invalid model precision")
  require(seed>=0 && seed<=0xffffffffL && clustering.field("random_state").number(10)==seed.toDouble,"random_state must be an unsigned 32-bit integer")
  require(clusters > 0 && iterations > 0 && clusteringSize >= 0, "Invalid clustering parameters")
  def sourceDescription: JsObject = if source == "csv" then obj("type"->JsString(source),"scenario_dir"->JsString(scenario)) else
    obj("type"->JsString(source),"host"->JsString(ch("host","localhost")),"port"->JsNumber(ch("port","8123").toInt),"database"->JsString(ch("database","htc")),"table"->JsString(table),"simulation_id"->JsString(simulation))
