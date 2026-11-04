package org.interscity.htc.parity

import spray.json.*
import Json.*
import java.nio.file.{Files,Path}
import java.util.concurrent.{Executors,Callable,ExecutionException}
import scala.collection.mutable
import scala.util.{Using,control}
import scala.jdk.CollectionConverters.*

final case class StageRequest(name: String,params: JsObject=Json.empty)

/** Native pipeline state. Large series stay in primitive buffers; variant outputs are streamed to disk. */
final class Pipeline(val config: RunConfig,val output: Path,val input: Option[Path]=None,
  log: String=>Unit=_=>(),check: ()=>Unit=()=>()):
  Files.createDirectories(output)
  private var dataset: Option[Dataset]=None
  private var prepared: Option[Prepared]=None
  private var trained: Option[TrainedModel]=None
  private var experiment: Option[JsObject]=None
  private val estimates=mutable.LinkedHashMap.empty[String,Path]
  private def path(name: String): Path = Json.safe(output,if name.endsWith(".json") then name else name+".json")
  private def save(name: String,value: JsValue): Unit = Json.write(path(name),value)
  private def read(name: String): JsValue = Selection.read(path(name))
  private def getData: Dataset = dataset.getOrElse { check();val data=Sources.load(config,progress=message=>{check();log(message)});dataset=Some(data);data }
  private def getPrepared: Prepared = prepared.getOrElse {
    val data=getData;val p=Sources.prepare(data,trained.map(_.length).getOrElse(data.maxSize));prepared=Some(p);p
  }
  def model: TrainedModel = trained.getOrElse {
    val base=input.getOrElse(output)
    val file=if Files.isRegularFile(base) then base else
      Using.resource(Files.list(base))(_.iterator().asScala.filter(_.toString.endsWith(".simedape")).toVector.sortBy(p=>(if p.getFileName.toString=="model.simedape" then 0 else 1,p.toString)).headOption.getOrElse(throw IllegalArgumentException(s"No portable model in $base")))
    val m=ModelIO.read(file,Sources.roads(config.streetInfo,config.separator));trained=Some(m);m
  }
  private def checkpoint(): Unit = ModelIO.write(output.resolve("model.simedape"),model)
  private def clusteringJson(m: TrainedModel): JsObject = obj("number_of_clusters"->JsNumber(m.clusters.count),"clusters"->ints(m.clusters.labels.toVector),
    "centroids"->JsObject(m.clusters.centroids.indices.map(k=>k.toString->arr(m.clusters.centroids(k).toVector)).toMap),
    "mean"->JsArray(m.means.toVector.map(n=>JsArray(JsArray(num(n))))),"standard_deviation"->JsArray(m.stds.toVector.map(n=>JsArray(JsArray(num(n))))),
    "time_series_by_cluster"->JsObject((0 until m.clusters.count).map(k=>k.toString->JsNumber(m.clusters.members(k).length)).toMap))

  /** Load the partial only once; one preparation per interpolation/size group, bounded active variants. */
  def variants(keys: Vector[String],location: String,percentage: Double,prefix: String,workers: Int=1)(result: (Variant,Path)=>Unit=(_,_)=>()): JsObject =
    require(workers>=1 && workers<=256,"workers must be in [1,256]")
    val m=model;val statuses=mutable.LinkedHashMap.empty[String,JsValue]
    val entries: Vector[Variant]=keys.distinct.flatMap { key=>
      try Some(Variants.get(key))
      catch case _: IllegalArgumentException=>
        statuses(key)=obj("ok"->JsBoolean(false),"error"->JsString("variação desconhecida"))
        None
    }
    val needsPartial=entries.exists(v=>v.family!="pca_delta" || v.method!="base")
    val empty=Dataset(Vector.empty,m.roads,Map.empty,"")
    val loaded: Either[Throwable,Dataset]=try Right(if !needsPartial then empty else if location.isEmpty || Set("training","100%")(location.trim.toLowerCase) then getData else Sources.load(config,location,log))
      catch case control.NonFatal(e)=>{check();Left(e)}
    val partial=loaded.getOrElse(empty)
    val runnable=entries.filter { v=>
      if loaded.isLeft && !(v.family=="pca_delta" && v.method=="base") then
        statuses(v.key)=obj("ok"->JsBoolean(false),"error"->JsString(loaded.swap.toOption.get.toString));false
      else true
    }
    val groups=runnable.groupBy(v=>if v.family=="pca_delta" && v.method=="base" then (0,"linear") else Estimator.preparationKey(v,m,partial)).toVector.sortBy(_._1)
    val pool=Executors.newFixedThreadPool(workers)
    try
      groups.foreach { (key,group)=>
        try
          check()
          val prep=if key._1==0 then Prepared(partial,Array.empty,Array.empty,Array.empty) else Sources.prepare(partial,key._1,key._2)
          val dtw=group.filter(_.family=="dtw")
          val work=(if dtw.nonEmpty then Vector(dtw) else Vector.empty)++group.filter(_.family!="dtw").map(Vector(_))
          // DTW methods share decompression and translation; other variants share immutable preparation buffers.
          work.grouped(workers).foreach { batch=>
            val jobs=batch.map { vs=>vs->pool.submit(new Callable[JsObject]:
              def call(): JsObject =
                check();val started=System.nanoTime()
                val sinks=vs.map(v=>v.key->new MetricSink(path(s"${prefix}_${v.key}"),m.roads)).toMap
                try
                  val missing=if vs.head.family=="dtw" then Estimator.dtwBatch(m,prep,vs,percentage)((v,label,values)=>{check();sinks(v.key).append(label,values)})
                  else Estimator.estimate(m,prep,vs.head,percentage)((label,values)=>{check();sinks(vs.head.key).append(label,values)})
                  vs.foreach(v=>sinks(v.key).finish(missing,v.family=="dtw"))
                finally sinks.values.foreach(_.close())
                obj("ok"->JsBoolean(true),"seconds"->num(math.rint((System.nanoTime()-started)/1e7)/100))
            ) }
            jobs.foreach { (vs,future)=>
              try
                val status=future.get()
                vs.foreach { v=>
                  statuses(v.key)=status;val file=path(s"${prefix}_${v.key}");estimates(v.key)=file;result(v,file);log(s"${v.key}: completed")
                }
              catch case e: ExecutionException=>
                check();vs.foreach { v=>statuses(v.key)=obj("ok"->JsBoolean(false),"error"->JsString(Option(e.getCause).getOrElse(e).toString));log(s"${v.key}: ${e.getCause}") }
            }
          }
        catch case control.NonFatal(e)=>
          check()
          group.foreach { v=>statuses(v.key)=obj("ok"->JsBoolean(false),"error"->JsString(e.toString));log(s"${v.key}: $e") }

      }
    finally pool.shutdownNow()
    val status=JsObject(statuses.toMap);save("variants_status",status)
    require(statuses.isEmpty || statuses.values.exists(_.field("ok").bool()),"No estimation variant completed; see variants_status.json")
    status

  def execute(request: StageRequest): Unit =
    val p=request.params
    def text(key: String,default: String): String = p.field(key,JsString(default)).text()
    def names(key: String,default: Vector[String]): Vector[String] = p.field(key,strings(default)).names
    def dest(default: String): String = text("output_file",default)
    request.name match
      case "data_load"=>dataset=Some(Sources.load(config,progress=message=>{check();log(message)}));prepared=None;log(s"Loaded ${getData.series.size} series")
      case "process_dataset"=>prepared=Some(Sources.prepare(getData,trained.map(_.length).getOrElse(getData.maxSize)));log(s"Prepared ${getPrepared.normalized.length} series")
      case "clustering"=>
        trained=Some(Training.cluster(getPrepared,config,log));checkpoint()
        save(config.clustering.field("output_file",JsString("clustering")).text(),clusteringJson(model));save("time_series_labels",strings(model.labels))
        if !config.json.field("compact_model").bool(true) then
          save("street_series_vehicles",JsArray(model.vehicles.map(strings)))
          for k<-0 until model.clusters.count do
            Using.resource(Files.newBufferedWriter(path(s"dataset_cluster_$k"))) { writer=>
              writer.write('[')
              model.clusters.members(k).zipWithIndex.foreach { (i,index)=>
                if index>0 then writer.write(',')
                writer.write('[')
                getPrepared.normalized(i).zipWithIndex.foreach { (value,j)=>if j>0 then writer.write(',');writer.write("["+num(value).compactPrint+"]") }
                writer.write(']')
              };writer.write(']')
            }
      case "simulation_points"=>
        val m=model;val points=m.clusters.simulationPoints.toVector
        save("simulation_points_indexs",ints(points));save("simulation_points_labels",strings(points.map(m.labels)))
        save("time_series_labels_indexs",ints(m.labels.indices))
        if !config.json.field("compact_model").bool(true) then
          save("simulation_points_vehicles",m.simulationPointVehicles)
          save("clustering_vehicles",JsObject((0 until m.clusters.count).map(k=>k.toString->strings(m.clusters.members(k).flatMap(m.vehicles).distinct.sorted.toVector)).toMap))
      case "warping_paths"=>
        if p.field("load_from_disk").bool() then require(model.dtw.nonEmpty,"No trained DTW paths")
        else
          trained=Some(Training.warping(model,getPrepared,output.resolve("warping_paths"),p.field("n_jobs").int(1),config.precision,log,check));checkpoint()
          if !config.json.field("compact_model").bool(true) then
            for k<-0 until model.clusters.count do
              val paths=ModelIO.paths(model.dtw.get,k)
              Using.resource(Files.newBufferedWriter(output.resolve("warping_paths").resolve(s"cluster_$k.json"))) { writer=>
                writer.write('{')
                for i<-0 until paths.size do
                  if i>0 then writer.write(',')
                  writer.write(JsString(i.toString).compactPrint+":")
                  writer.write(JsArray(paths.path(i).toVector.map(s=>JsArray(JsNumber(s.i),JsNumber(s.j),num(s.cost),JsNumber(if s.descending then 1 else 0)))).compactPrint)
                writer.write('}')
              }
      case "pca_train"=>
        if p.field("load_from_disk").bool() then
          val file=input.filter(Files.isDirectory(_)).getOrElse(output).resolve(text("models_file","pca_models")+".simedape")
          if Files.isRegularFile(file) then trained=Some(ModelIO.read(file))
          require(model.pca.nonEmpty,"No trained PCA models")
        else
          trained=Some(Training.pca(model,getPrepared,p.field("n_components").int(5),log));checkpoint()
          ModelIO.write(Json.safe(output,text("models_file","pca_models")+".simedape"),model)
      case "model_export"=>
        val destination=Json.safe(output,dest("model.simedape"))
        ModelIO.write(destination,model,p.field("include_vehicles").bool(true))
        if !p.field("keep_parts").bool() then
          model.dtw.filter(Files.isDirectory(_)).filter(_.toAbsolutePath.normalize()==output.resolve("warping_paths").toAbsolutePath.normalize()).foreach { folder=>
            Using.resource(Files.list(folder))(_.iterator().asScala.filter(_.getFileName.toString.matches("cluster_[0-9]+\\.wpz")).foreach(Files.delete(_)))
            trained=Some(model.copy(dtw=Some(destination)))
          }
      case "scenario_metrics"=>
        val base=getPrepared;val reference=text("reference_dir","")
        val data=if reference.isEmpty then base else Sources.prepare(Sources.load(config,reference,log),base.normalized(0).length)
        val labels=if reference.isEmpty then data.dataset.series.map(_.label) else getData.series.map(_.label).filter(data.dataset.index.contains)
        require(labels.nonEmpty,"Reference has no series in common with the training scenario")
        Using.resource(new MetricSink(path(dest("new_simulation")),getData.roads)) { sink=>
          labels.foreach { label=>check();val link=label.takeWhile(_!='-');if getData.roads.contains(link) then sink.append(label,data.raw(data.dataset.index(label))) }
          val modes=config.json.field("transport_modes",strings(Vector("car"))).names
          val counts=labels.filter(l=>getData.roads.contains(l.takeWhile(_!='-'))).map(l=>data.raw(data.dataset.index(l)).sum/data.normalized(0).length)
          val total=counts.sum
          val scalars=if modes.size<=1 then Json.empty else JsObject(modes.zipWithIndex.flatMap { (mode,i)=>
            val count=if i==0 then total else 0.0
            Vector(s"avg_travel_time_$mode"->num(if i==0 && counts.nonEmpty then total/counts.size else 0),s"throughput_$mode"->num(count),s"modal_split_$mode"->num(if total>0 then math.rint(count/total*1e6)/1e6 else 0))
          }.toMap)
          sink.finish(Missing(),false,includeSeries=false,scalars=scalars)
        }
      case "estimation"=>variants(names("methods",Vector("avg")),text("second_scenario_dir",""),p.field("percentage_sample").number(100),text("output_prefix","result"))()
      case "estimation_variants"=>
        val selected=names("variants",Vector("avg","base","sample","wavg"))
        variants(if selected==Vector("all") then Variants.catalog.map(_.key) else selected,text("second_scenario_dir",""),p.field("percentage_sample").number(100),text("output_prefix","result"),p.field("workers").int(1))()
      case "pca_estimation"=>
        if model.pca.isEmpty then
          val file=input.filter(Files.isDirectory(_)).getOrElse(output).resolve(text("models_file","pca_models")+".simedape")
          val loaded=ModelIO.read(file);require(loaded.labels==model.labels,"PCA models and clustering do not align")
          trained=Some(model.copy(pca=loaded.pca))
        val mode=if p.field("use_sp")!=JsNull then (if p.field("use_sp").bool() then "sp" else "ind") else text("mode","sp")
        require(Set("sp","ind","stretched")(mode),"Invalid PCA mode")
        val location=text("second_scenario_dir","")
        val m=model
        val data=if location.isEmpty then getData else Sources.load(config,location,log)
        val prepared=Sources.prepare(data,m.length)
        Using.resource(new MetricSink(path(dest("result_pca")),m.roads)) { sink=>
          val missing=Estimator.standalonePCA(m,prepared,mode)((label,values)=>{check();sink.append(label,values)})
          sink.finish(missing,false)
        }
      case "compare"=>
        val real=read(text("real_file","new_simulation"));val prefix=text("output_prefix","compare")
        val results=names("estimated_files",Vector("result_avg")).filter(f=>Files.isRegularFile(path(f))).map { file=>
          val key=file.stripPrefix("result_");val comparison=Metrics.compare(real,read(file),key,trained.map(_.context).getOrElse(empty))
          save(s"${prefix}_$key",comparison);key->JsObject(comparison.fields-"specific")
        }.toMap
        if results.nonEmpty then
          save(s"${prefix}_summary",JsObject(results))
          if p.field("generate_plots").bool(true) then Reports.savePlots(output,prefix,title=config.name)
      case "sweep"=>
        val prefix=text("output_prefix","sweep");val real=read(text("real_file","new_simulation"))
        val requested=names("variants",Vector.empty)
        val keys=if requested.nonEmpty then (if requested==Vector("all") then Variants.catalog.map(_.key) else requested) else
          Variants.catalog.filter(v=>(v.family=="dtw" && names("methods",Vector("avg","base","sample","wavg")).contains(v.method)) || (v.family!="dtw" && p.field("include_pca").bool())).map(_.key)
        val summaries=p.field("scenarios").values.zipWithIndex.map { (scenario,index)=>
          val label=scenario.field("label",JsString(s"scenario_${index+1}")).text();val key=label.replace("%","p").replace(" ","_")
          val methods=mutable.LinkedHashMap.empty[String,JsValue]
          variants(keys,scenario.field("dir").text(),scenario.field("percentage").number(100),s"${prefix}_$key") { (v,file)=>
            methods(v.key)=Metrics.compare(real,Selection.read(file),s"${v.key}@$label")
          }
          save(s"${prefix}_compare_$key",JsObject(methods.toMap.map((k,v)=>k->JsObject(v.asJsObject.fields-"label"))))
          if p.field("generate_plots").bool(true) && methods.nonEmpty then
            val comparisonPrefix=s"${prefix}_compare_$key"
            methods.foreach((method,value)=>save(s"${comparisonPrefix}_$method",value))
            save(s"${comparisonPrefix}_summary",JsObject(methods.toMap.map((k,v)=>k->JsObject(v.asJsObject.fields-"specific"))))
          val stats=methods.toMap.map { (k,v)=>
            val per=v.field("per_series_error").asJsObject.fields
            k->obj("coverage_pct"->v.field("coverage_pct"),"mape"->JsObject(per.map((metric,s)=>metric->s.field("mape"))),
              "wape"->JsObject(per.keys.map(m=>m->v.field("robust").field(m).field("wape")).toMap),"error_pct"->v.field("aggregate").field("error_pct"))
          }
          obj("label"->JsString(label),"percentage"->scenario.field("percentage",JsNumber(100)),"methods"->JsObject(stats))
        }
        save(s"${prefix}_summary",obj("scenarios"->JsArray(summaries)))
        if p.field("generate_plots").bool(true) then summaries.foreach { scenario=>
          val key=scenario.field("label").text().replace("%","p").replace(" ","_")
          if scenario.field("methods").asJsObject.fields.nonEmpty then Reports.savePlots(output,s"${prefix}_compare_$key",sweep=prefix,title=config.name)
        }
      case "selection"=>
        val data=getData;val m=try Some(model) catch case _: IllegalArgumentException=>None
        val strategy=text("strategy","random");val unitFile=text("unit_map_file","")
        val mapping=if unitFile.isEmpty then None else Some(Selection.unitMap(Path.of(unitFile)))
        val u=Selection.universe(data.series.map(_.vehicles),data.series.map(_.label),m.map(_.clusters.labels.toVector).getOrElse(Vector.fill(data.series.size)(-1)),mapping,text("unit_kind",if mapping.nonEmpty then "person" else "vehicle"))
        val sp=if Selection.spStrategies(strategy) then m.getOrElse(throw IllegalArgumentException("Simulation points required")).clusters.simulationPoints.toVector else Vector.empty
        val distances=if strategy!="centroid_nearest" then Vector.empty else getPrepared.normalized.indices.map(i=>getPrepared.normalized(i).zip(m.get.clusters.centroids(m.get.clusters.labels(i))).map((a,b)=>math.abs(a-b)).sum).toVector
        val topology=if !strategy.startsWith("topology_") then Map.empty[String,Set[String]] else Selection.topology(Path.of(text("topology_file","")))
        val decisions=if text("decisions_file","").isEmpty then Map.empty[String,Vector[JsValue]] else Selection.decisions(Path.of(text("decisions_file","")))
        save(dest("selection"),Selection.manifest(u,config.name,p,sp,distances,topology,decisions))
      case "selective_instantiation"=>save(dest("selective_instances"),Selection.selective(model,config.name,p.field("include_buffer_ratio").number(.1)))
      case "multivariable_experiment"=>
        experiment=Some(experiments.ExperimentRunner.run(p.field("experiment_config").asJsObject,Path.of(text("base_dir",".")).toAbsolutePath,output,log,check))
      case other=>throw IllegalArgumentException(s"Unknown stage: $other")

  def run(stages: Vector[StageRequest],progress: JsObject=>Unit=_=>()): JsObject =
    Pipeline.validate(stages)
    def emit(event: JsObject): Unit = try progress(event) catch case control.NonFatal(_)=>()
    emit(obj("event"->JsString("pipeline_start"),"stages"->strings(stages.map(_.name))))
    val start=System.nanoTime()
    stages.zipWithIndex.foreach { (stage,index)=>
      check();val started=System.nanoTime()
      emit(obj("event"->JsString("stage_start"),"index"->JsNumber(index+1),"name"->JsString(stage.name)))
      try
        execute(stage);check()
        emit(obj("event"->JsString("stage_end"),"index"->JsNumber(index+1),"name"->JsString(stage.name),"elapsed"->num((System.nanoTime()-started)/1e9)))
      catch case control.NonFatal(e)=>
        emit(obj("event"->JsString("stage_failed"),"index"->JsNumber(index+1),"name"->JsString(stage.name),"elapsed"->num((System.nanoTime()-started)/1e9),"error"->JsString(e.toString)));throw e
    }
    emit(obj("event"->JsString("pipeline_end"),"elapsed"->num((System.nanoTime()-start)/1e9)))
    val summary=mutable.Map[String,JsValue]("scenario_name"->JsString(config.name),"output_dir"->JsString(output.toString))
    dataset.foreach(d=>{summary("n_series")=JsNumber(d.series.size);summary("max_size")=JsNumber(d.maxSize)})
    if estimates.nonEmpty then summary("estimation_files")=JsObject(estimates.toMap.map((k,p)=>k->JsString(p.getFileName.toString)))
    experiment.foreach(value=>summary("experimental_results")=value)
    JsObject(summary.toMap)

object Pipeline:
  val stages: Vector[JsValue] = Using.resource(getClass.getResourceAsStream("/parity/stages.json"))(s=>new String(s.readAllBytes(),java.nio.charset.StandardCharsets.UTF_8).parseJson.values)
  val implemented: Set[String] = stages.map(_.field("name").text()).toSet
  def validate(requests: Vector[StageRequest]): Unit =
    require(requests.nonEmpty,"stages must be a nonempty list")
    requests.foreach { r=>
      require(implemented(r.name),s"Stage not yet implemented natively: ${r.name}")
      val allowed=stages.find(_.field("name").text()==r.name).get.field("parameters").names.toSet
      require(r.params.fields.keySet.subsetOf(allowed),s"Unknown parameters for ${r.name}: ${(r.params.fields.keySet--allowed).mkString(", ")}")
    }
