package org.interscity.htc.parity

import spray.json.*
import Json.*
import java.nio.file.{Files,Path}
import java.io.FileNotFoundException
import java.util.zip.ZipFile
import scala.util.Using
import scala.jdk.CollectionConverters.*

object Exports:
  val labels=Map("average_speed"->"Velocidade Média (m/s)","percentage_of_track"->"Ocupação (%)","average_vehicles"->"Veículos Médios","km_traveled"->"Km Trafegados",
    "density_veh_km"->"Densidade (veíc./km)","speed_index"->"Índice de velocidade (v/v livre)","delay_index"->"Índice de atraso","peak_occupancy"->"Ocupação de pico (%)","congested_time_pct"->"Tempo congestionado (%)")
  val robust=Vector("wape","smape","median_ape","bias_pct","rmse","mae","pearson","spearman")
  def valid(name: String): String = {require(name.matches("[A-Za-z0-9._-]+") && !name.contains(".."),"Invalid artifact name");name}
  def read(root: Path,stem: String): JsValue =
    val file=Json.safe(root,valid(stem)+".json")
    if !Files.isRegularFile(file) then throw new FileNotFoundException(s"Artifact not found: $stem")
    Selection.read(file)
  def methods(root: Path,prefix: String,exclude: Set[String]=Set.empty): Vector[String] =
    valid(prefix)
    if !Files.isDirectory(root) then Vector.empty else Using.resource(Files.list(root)) { paths=>
      paths.iterator().asScala.map(_.getFileName.toString).filter(n=>n.startsWith(prefix+"_") && n.endsWith(".json")).map(_.stripPrefix(prefix+"_").stripSuffix(".json")).filterNot(exclude).toVector.sorted
    }
  def classify(name: String,clustering: String): String =
    if name.endsWith(".pdf") then "report"
    else if name.stripSuffix(".json")==clustering then "clustering"
    else Vector("new_simulation"->"real_metrics","simulation_points"->"simulation_points","dataset_cluster_"->"cluster_dataset","selective_instances"->"selective_instantiation","time_series_labels"->"labels","compare_"->"comparison","sweep_"->"sweep","result_"->"estimation").find((p,_)=>name.startsWith(p)).map(_._2).getOrElse("other")
  def artifacts(root: Path,clustering: String="clustering"): Vector[JsValue] =
    Vector("","plots").flatMap { sub=>
      val folder=root.resolve(sub)
      if !Files.isDirectory(folder) then Vector.empty else Using.resource(Files.list(folder)) { paths=>
        paths.iterator().asScala.filter(p=>Files.isRegularFile(p) && (p.toString.endsWith(".json") || p.toString.endsWith(".pdf"))).toVector.sortBy(_.toString).map { file=>
          obj("name"->JsString(if sub.isEmpty then file.getFileName.toString else s"$sub/${file.getFileName}"),"kind"->JsString(classify(file.getFileName.toString,clustering)),"size_bytes"->JsNumber(Files.size(file)),"modified"->num(Files.getLastModifiedTime(file).toMillis/1000.0))
        }
      }
    }
  def stats(values: Vector[JsValue]): JsObject =
    val numbers=values.collect {case JsNumber(n)=>n.toDouble}
    obj("count"->JsNumber(values.size),"min"->numbers.minOption.map(num).getOrElse(JsNull),"max"->numbers.maxOption.map(num).getOrElse(JsNull),"mean"->(if numbers.isEmpty then JsNull else num(numbers.sum/numbers.size)))
  def estimation(root: Path,prefix: String): JsObject = obj("prefix"->JsString(prefix),"methods"->JsObject(methods(root,prefix).map { method=>
    val data=read(root,s"${prefix}_$method").asJsObject.fields
    method->obj("file"->JsString(s"${prefix}_$method.json"),"metrics"->JsObject(data.collect {case (k,JsArray(v)) if k!="labels"=>k->stats(v)}),
      "scalars"->JsObject(data.filterNot((_,v)=>v.isInstanceOf[JsArray] || v.isInstanceOf[JsObject])))
  }.toMap))
  def detail(root: Path,prefix: String,method: String,metric: Option[String],offset: Int,limit: Option[Int]): JsObject =
    valid(method)
    val data=read(root,s"${prefix}_$method").asJsObject.fields
    val kept=data.filterNot((k,v)=>metric.nonEmpty && v.isInstanceOf[JsArray] && k!=metric.get)
    val total=kept.values.collect {case JsArray(v)=>v.size}.maxOption
    obj("method"->JsString(method),"prefix"->JsString(prefix),"offset"->JsNumber(offset),"limit"->limit.map(JsNumber(_)).getOrElse(JsNull),"total_series"->total.map(JsNumber(_)).getOrElse(JsNull),
      "data"->JsObject(kept.map((k,v)=>k->(v match
        case JsArray(xs)=>JsArray(xs.slice(offset,limit.map(l=>math.min(Int.MaxValue.toLong,offset.toLong+l).toInt).getOrElse(xs.size)))
        case other=>other))))
  def comparison(root: Path,prefix: String): JsObject =
    val ms=methods(root,prefix,Set("summary"));val results=ms.map(m=>m->read(root,s"${prefix}_$m")).toMap
    val keys=results.values.flatMap(_.field("per_series_error",empty).asJsObject.fields.keys).toSet
    val metrics=Metrics.keys.filter(keys)++(keys--Metrics.keys).toVector.sorted
    def matrix(f: (JsValue,String)=>JsValue): JsObject = JsObject(metrics.map(k=>k->JsObject(ms.map(m=>m->f(results(m),k)).toMap)).toMap)
    obj("prefix"->JsString(prefix),"methods"->strings(ms),"metrics"->JsArray(metrics.map(k=>obj("key"->JsString(k),"label"->JsString(labels.getOrElse(k,k))))),
      "mape"->matrix((r,k)=>r.field("per_series_error").field(k).field("mape")),"aggregate_error_pct"->matrix((r,k)=>r.field("aggregate").field("error_pct").field(k)),
      "robust"->JsObject(metrics.map(k=>k->JsObject(robust.map(s=>s->JsObject(ms.map(m=>m->results(m).field("robust").field(k).field(s)).toMap)).toMap)).toMap),
      "pairing"->JsObject(ms.map(m=>m->results(m).field("pairing")).toMap),"coverage_pct"->JsObject(ms.map(m=>m->results(m).field("coverage_pct")).toMap),
      "n_real"->JsObject(ms.map(m=>m->results(m).field("n_real")).toMap),"n_estimated"->JsObject(ms.map(m=>m->results(m).field("n_estimated")).toMap))
  def modelFile(root: Path): Path =
    if !Files.isDirectory(root) then throw new FileNotFoundException("No model")
    Using.resource(Files.list(root))(_.iterator().asScala.filter(_.toString.endsWith(".simedape")).toVector.sortBy(p=>(if p.getFileName.toString=="model.simedape" then 0 else 1,p.toString)).headOption.getOrElse(throw new FileNotFoundException("No model")))
  def manifest(file: Path): JsValue = Using.resource(new ZipFile(file.toFile))(zip=>Using.resource(zip.getInputStream(zip.getEntry("manifest.json")))(s=>new String(s.readAllBytes(),java.nio.charset.StandardCharsets.UTF_8).parseJson))
  def model(root: Path): JsObject =
    val file=modelFile(root);val m=manifest(file)
    val groups=m.field("members").asJsObject.fields.toVector.groupBy(_._1.takeWhile(_!='/')).map { (group,members)=>
      group->obj("members"->JsNumber(members.size),"raw_bytes"->num(members.map(_._2.field("raw_bytes").number()).sum),"stored_bytes"->num(members.map(_._2.field("stored_bytes").number()).sum))
    }
    JsObject(m.asJsObject.fields.filter((k,_)=>Set("format","created","meta","summary","total_raw_bytes","total_stored_bytes")(k))++Map("file"->JsString(file.getFileName.toString),"file_bytes"->JsNumber(Files.size(file)),"components"->JsObject(groups)))
