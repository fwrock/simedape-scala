package org.interscity.htc.parity

import spray.json.*
import Json.*
import java.nio.file.{Files,Path}
import java.net.{URI,URLEncoder}
import java.net.http.{HttpClient,HttpRequest,HttpResponse}
import java.time.Duration
import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.util.Using
import org.tslearn.preprocessing.TimeSeriesResampler

final case class FlowEvent(link: String,tick: Double,vehicle: String,count: Double)

object Sources:
  private val http = HttpClient.newBuilder().connectTimeout(Duration.ofSeconds(30)).build()
  def csv(line: String, separator: Char): Vector[String] =
    val out=Vector.newBuilder[String];val cell=new StringBuilder;var quote: Char=0;var i=0
    while i<line.length do
      val c=line(i)
      if quote!=0 then
        if c==quote && i+1<line.length && line(i+1)==quote then {cell.append(c);i+=1}
        else if c==quote then quote=0
        else cell.append(c)
      else if (c=='\'' || c=='"') && cell.isEmpty then quote=c
      else if c==separator then {out+=cell.toString;cell.clear()}
      else cell.append(c)
      i+=1
    require(quote==0,"Unterminated CSV field")
    out+=cell.toString
    out.result()

  def roads(path: String,separator: Char): Map[String,Road] =
    if path.isEmpty then Map.empty
    else Using.resource(Files.newBufferedReader(Path.of(path))) { reader =>
      val header=Option(reader.readLine()).getOrElse(throw IllegalArgumentException("Empty street metadata"))
      val names=csv(header.stripPrefix("\uFEFF"),separator);val idx=Vector("link","length","capacity","freespeed").map(names.indexOf)
      require(idx.forall(_>=0),"Street metadata needs link,length,capacity,freespeed")
      reader.lines().iterator().asScala.filter(_.nonEmpty).map { line =>
        val row=csv(line,separator);val id=row(idx(0));id->Road(id,row(idx(1)).toDouble,row(idx(2)).toDouble,row(idx(3)).toDouble)
      }.toMap
    }

  def query(config: RunConfig,sql: String,params: Map[String,String]=Map.empty)(consume: Iterator[JsValue] => Unit): Unit =
    val secure=config.clickhouse.field("secure").bool()
    val parameters=Map("database"->config.ch("database","htc"),"query"->sql)++params.map((k,v)=>s"param_$k"->v)
    val encoded=parameters.iterator.map((k,v)=>s"${URLEncoder.encode(k,"UTF-8")}=${URLEncoder.encode(v,"UTF-8")}").mkString("&")
    val uri=URI.create(s"${if secure then "https" else "http"}://${config.ch("host","localhost")}:${config.ch("port","8123")}/?$encoded")
    val request=HttpRequest.newBuilder(uri).timeout(Duration.ofSeconds(config.clickhouse.field("timeout").int(3600)))
      .header("X-ClickHouse-User",config.ch("user","default")).header("X-ClickHouse-Key",config.ch("password",""))
      .POST(HttpRequest.BodyPublishers.noBody()).build()
    val response=http.send(request,HttpResponse.BodyHandlers.ofInputStream())
    Using.resource(response.body()) { body =>
      if response.statusCode()!=200 then throw IllegalStateException(s"ClickHouse HTTP ${response.statusCode()}: ${new String(body.readNBytes(65536),java.nio.charset.StandardCharsets.UTF_8)}")
      val reader=new java.io.BufferedReader(new java.io.InputStreamReader(body,java.nio.charset.StandardCharsets.UTF_8))
      consume(reader.lines().iterator().asScala.filter(_.nonEmpty).map(_.parseJson))
    }

  def simulations(config: RunConfig): JsArray =
    require(config.table.matches("[A-Za-z0-9_.]+"),"Invalid ClickHouse table")
    var names=Set.empty[String]
    query(config,s"DESCRIBE TABLE ${config.table} FORMAT JSONEachRow")(rows=>names=rows.map(_.field("name").text()).toSet)
    val extra=if names("scenario_id") then ",any(scenario_id) AS scenario_id" else ""
    var result=Vector.empty[JsValue]
    query(config,s"SELECT simulation_id,uniqExact(link_id) AS link_count,count() AS events$extra FROM ${config.table} GROUP BY simulation_id ORDER BY simulation_id FORMAT JSONEachRow") { rows=>
      result=rows.map { row=>JsObject(Map("id"->row.field("simulation_id"),"link_count"->num(row.field("link_count").number()),"events"->num(row.field("events").number()))++
        (if row.field("scenario_id")==JsNull then Map.empty else Map("scenario_id"->row.field("scenario_id")))) }.toVector
    }
    JsArray(result)

  def load(config: RunConfig,location: String="",progress: String=>Unit=_=>()): Dataset =
    val metadata=roads(config.streetInfo,config.separator)
    if config.source=="csv" then
      require(config.window.period==0,"Regular occupancy requires ClickHouse events")
      val dir=Path.of(if location.nonEmpty then location else config.scenario)
      require(Files.isDirectory(dir),s"Scenario directory does not exist: $dir")
      val all=Vector.newBuilder[Series];val vehicles=mutable.LinkedHashMap.empty[String,Vector[Vector[String]]]
      Using.resource(Files.list(dir)) { entries =>
        entries.iterator().asScala.filter(_.getFileName.toString.endsWith(".csv")).toVector.sortBy(_.toString).foreach { file =>
          val link=file.getFileName.toString.stripSuffix(".csv").takeWhile(_!='-')
          val events=Using.resource(Files.newBufferedReader(file)) { reader =>
            val header=csv(Option(reader.readLine()).getOrElse("").stripPrefix("\uFEFF"),config.separator)
            val names=Vector("time","vehicle","count");val ix=names.map(header.indexOf)
            require(ix.forall(_>=0),s"Missing ${names.mkString(",")} in $file")
            reader.lines().iterator().asScala.filter(_.nonEmpty).flatMap { line =>
              val row=csv(line,config.separator)
              row(ix(2)).toDoubleOption.filter(_.isFinite).map(n=>FlowEvent(link,row(ix(0)).toDouble,row(ix(1)),n))
            }.toVector
          }
          val result=legacy(link,events,config.window,includeEmpty=true)
          all++=result._1;vehicles(link)=vehicles.getOrElse(link,Vector.empty)++result._2
        }
      }
      Dataset(all.result(),metadata,vehicles.toMap,dir.toString)
    else
      require(config.table.matches("[A-Za-z0-9_.]+"),"Invalid ClickHouse table")
      val scope=if location.nonEmpty then location else config.simulation
      val filter=if scope.nonEmpty then " AND simulation_id = {simulation_id:String}" else ""
      val sql=s"SELECT link_id,tick,vehicle_id,vehicle_count_on_link FROM ${config.table} WHERE tick >= {start_time:UInt64} AND tick <= {end_time:UInt64}$filter ORDER BY link_id,tick FORMAT JSONEachRow"
      val params=Map("start_time"->config.window.start.toString,"end_time"->config.window.end.toString)++(if scope.nonEmpty then Map("simulation_id"->scope) else Map.empty)
      var result: Option[Dataset]=None
      query(config,sql,params) { rows =>
        val events=rows.map(row=>FlowEvent(row.field("link_id").text(),row.field("tick").number(),row.field("vehicle_id").text(),row.field("vehicle_count_on_link").number(Double.NaN)))
        result=Some(fromEvents(events,config.window,metadata,scope,progress))
      }
      result.get

  /** Events are ordered by link and tick by ClickHouse; memory is bounded to one link. */
  def fromEvents(events: Iterator[FlowEvent],window: Window,metadata: Map[String,Road],source: String="",progress: String=>Unit=_=>()): Dataset =
    val buffered=events.buffered;val series=mutable.Map.empty[String,Vector[Series]]
    val membership=mutable.Map.empty[String,Vector[Vector[String]]];var links=0;var count=0L
    while buffered.hasNext do
      val link=buffered.head.link;val current=Vector.newBuilder[FlowEvent]
      while buffered.hasNext && buffered.head.link==link do
        val event=buffered.next();count+=1
        if count%65536==0 then progress(s"Loaded $links links / $count events")
        if window.period>0 then require(event.count.isFinite && event.count>=0 && event.tick.isFinite && event.tick>=window.start && event.tick<=window.end,"Invalid occupancy measurement")
        if event.count.isFinite && event.tick.isFinite then current+=event
      require(!series.contains(link),"ClickHouse rows must be ordered by link")
      val rows=current.result()
      if window.period>0 then
        require(metadata.contains(link),s"Event link $link absent from street catalogue")
        val result=regular(link,rows,window);series(link)=result;membership(link)=result.map(_.vehicles)
      else
        val result=legacy(link,rows,window,includeEmpty=false);series(link)=result._1;membership(link)=result._2
      links+=1
      if links%1000==0 then progress(s"Loaded $links links / $count events")
    if window.period>0 then
      require(count>0,"Cannot infer an entirely empty run")
      require(metadata.nonEmpty,"Regular occupancy requires street_info_file")
      metadata.keysIterator.filterNot(series.contains).foreach { link =>
        val result=regular(link,Vector.empty,window);series(link)=result;membership(link)=result.map(_.vehicles)
      }
    Dataset(series.toVector.sortBy(_._1).flatMap(_._2),metadata,membership.toMap,source)

  private def regular(link: String,events: Vector[FlowEvent],w: Window): Vector[Series] =
    val values=new Array[Double](((w.end-w.start)/w.period).toInt)
    val windows=((w.end-w.start)/w.width).toInt
    val members=Array.fill(windows)(mutable.Set.empty[String])
    var cursor=0.0;var state=0.0;var previousTick=Double.NaN;var previousCount=Double.NaN
    def integrate(until: Double): Unit =
      while cursor<until do
        val bin=(cursor/w.period).toInt
        val end=math.min(until,(bin+1).toDouble*w.period)
        values(bin)+=state*(end-cursor)/w.period;cursor=end
    events.foreach { event =>
      require(event.tick>=cursor,"Events must be ordered by tick")
      if event.tick==previousTick then require(event.count==previousCount,"Conflicting occupancy at the same tick")
      integrate(event.tick);state=event.count;previousTick=event.tick;previousCount=event.count
      val interval=if event.tick==0 then 0 else math.ceil(event.tick/w.width).toInt-1
      if event.vehicle.nonEmpty then members(interval)+=event.vehicle
    }
    integrate(w.end.toDouble)
    val samples=(w.width/w.period).toInt
    Vector.tabulate(windows)(i=>Series(link,w.start+i*w.width,w.start+(i+1)*w.width,values.slice(i*samples,(i+1)*samples),members(i).toVector.sorted))

  private def legacy(link: String,events: Vector[FlowEvent],w: Window,includeEmpty: Boolean): (Vector[Series],Vector[Vector[String]]) =
    val series=Vector.newBuilder[Series];val members=Vector.newBuilder[Vector[String]]
    val groups=events.filter(e=>e.tick>w.start && e.tick<=w.end+w.width-1).groupBy(e=>math.ceil((e.tick-w.start)/w.width).toLong-1)
    var index=0L
    while w.start+index*w.width<w.end do
      val rows=groups.getOrElse(index,Vector.empty);val vehicles=rows.map(_.vehicle)
      if includeEmpty || rows.nonEmpty then members+=vehicles
      if rows.length>1 then series+=Series(link,w.start+index*w.width,w.start+(index+1)*w.width,rows.map(_.count).toArray,vehicles)
      index+=1
    (series.result(),members.result())

  def prepare(dataset: Dataset,size: Int,interpolation: String="tslearn",workers: Int=sys.env.get("SIMEDAPE_PROCESS_WORKERS").fold(math.min(8,Runtime.getRuntime.availableProcessors()))(_.toInt)): Prepared =
    require(dataset.series.nonEmpty && size>0,"Dataset has no trainable series")
    require(workers>=1 && workers<=256,"SIMEDAPE_PROCESS_WORKERS must be in [1,256]")
    val x=new Array[Array[Double]](dataset.series.size);val means=new Array[Double](x.length);val stds=new Array[Double](x.length)
    def process(i: Int): Unit =
      val raw=TimeSeriesResampler.resample(dataset.series(i).values,size,interpolation)
      val mean=raw.sum/raw.length;val sigma=math.sqrt(raw.iterator.map(v=>(v-mean)*(v-mean)).sum/raw.length)
      means(i)=mean;stds(i)=if sigma==0 then 1.0 else sigma
      var j=0
      while j<raw.length do {raw(j)=(raw(j)-mean)/stds(i);j+=1}
      x(i)=raw
    if workers==1 || x.length<64 then x.indices.foreach(process)
    else
      val executor=java.util.concurrent.Executors.newFixedThreadPool(workers)
      try
        val futures=x.indices.grouped(64).map { ids=>executor.submit(new Runnable:
          def run(): Unit = ids.foreach(process)
        ) }.toVector
        futures.foreach(_.get())
      finally executor.shutdownNow()
    Prepared(dataset,x,means,stds)
