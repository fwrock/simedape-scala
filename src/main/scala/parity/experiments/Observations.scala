package org.interscity.htc.parity.experiments
import org.interscity.htc.parity.*
import Json.*
import spray.json.*
import java.nio.file.{Files,Path}
import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.util.Using

final case class Observation(run: String,vehicle: String,link: String,time: Double,event: String,duration: Double,speed: Double,acceleration: Double,gap: Double,position: Double)
object Observations:
  val units=Map("count"->"vehicles","speed"->"m/s","speed_std"->"m/s","acceleration"->"m/s^2","acceleration_positive"->"m/s^2","deceleration"->"m/s^2","gap"->"m","gap_p10"->"m","queue"->"vehicles","flow"->"vehicles/s","lane_changes"->"events/s","distance"->"vehicle-m","position"->"m")
  private val aliases=Vector("car_id"->"vehicle_id","bus_id"->"vehicle_id","bike_id"->"vehicle_id","motorcycle_id"->"vehicle_id","bicycle_id"->"vehicle_id","velocity"->"speed","leader_vehicle"->"leader_id","gap_to_leader"->"gap","simulation_id"->"run_id")
  private def numeric(value: JsValue): Double = value match
    case JsNull=>Double.NaN
    case JsString(s) if s.trim.isEmpty=>Double.NaN
    case JsString(s) if Set("nan","na","none","null")(s.toLowerCase)=>Double.NaN
    case JsString(s) if Set("inf","+inf","infinity","+infinity")(s.toLowerCase)=>Double.PositiveInfinity
    case JsString(s) if Set("-inf","-infinity")(s.toLowerCase)=>Double.NegativeInfinity
    case _=>value.number()
  def read(file: Path,runOverride: Option[String]=None): Vector[Observation] =
    val rows=Vector.newBuilder[JsValue]
    if file.toString.endsWith(".jsonl") || file.toString.endsWith(".ndjson") then
      Using.resource(Files.lines(file))(_.iterator().asScala.filter(_.trim.nonEmpty).foreach(s=>rows+=s.parseJson))
    else
      val source=if Files.isDirectory(file) || file.toString.endsWith(".parquet") || file.toString.endsWith(".pq") then "read_parquet(?)" else if file.toString.endsWith(".csv") then "read_csv(?,header=true,all_varchar=true,nullstr=['','NaN','nan','NULL','null'])" else throw IllegalArgumentException("Observations must be CSV, Parquet or JSONL")
      Class.forName("org.duckdb.DuckDBDriver")
      val properties=new java.util.Properties();properties.setProperty(org.duckdb.DuckDBDriver.JDBC_STREAM_RESULTS,"true")
      Using.resource(java.sql.DriverManager.getConnection("jdbc:duckdb:",properties)) { connection=>
        val threads=sys.env.get("SIMEDAPE_DUCKDB_THREADS").fold(math.min(8,Runtime.getRuntime.availableProcessors()))(_.toInt)
        val memory=sys.env.getOrElse("SIMEDAPE_DUCKDB_MEMORY","512MB")
        require(threads>=1 && threads<=256 && memory.matches("[0-9]+(?:\\.[0-9]+)?(?:KB|MB|GB|TB)"),"Invalid DuckDB resource settings")
        Using.resource(connection.createStatement()) { statement=>
          statement.execute(s"SET threads=$threads");statement.execute(s"SET memory_limit='$memory'")
        }
        Using.resource(connection.prepareStatement(s"SELECT * FROM $source")) { statement=>
          statement.setString(1,if Files.isDirectory(file) then file.toAbsolutePath.resolve("**/*.parquet").toString else file.toAbsolutePath.toString)
          Using.resource(statement.executeQuery()) { result=>
            val metadata=result.getMetaData;val names=(1 to metadata.getColumnCount).map(i=>i->metadata.getColumnLabel(i)).toVector
            while result.next() do rows+=JsObject(names.map { (i,key)=>
              val value=result.getObject(i)
              key->(value match
                case null=>JsNull
                case n: java.lang.Number if n.toString=="NaN"=>JsNull
                case n: java.lang.Number if !n.doubleValue().isFinite=>JsString(n.toString)
                case n: java.lang.Number=>JsNumber(BigDecimal(n.toString))
                case b: java.lang.Boolean=>JsBoolean(b.booleanValue())
                case v=>JsString(v.toString))
            }.toMap)
          }
        }
      }
    validate(rows.result(),runOverride)
  def validate(rows: Vector[JsValue],runOverride: Option[String]=None): Vector[Observation] =
    val hasPayload=rows.exists(_.asJsObject.fields.contains("data"))
    val decoded=rows.flatMap { original=>
      val merged=if !hasPayload then Some(original.asJsObject) else original.field("data") match
        case payload: JsObject=>Some(JsObject(original.asJsObject.fields++payload.fields))
        case JsString(text)=>text.parseJson match {case p: JsObject=>Some(JsObject(original.asJsObject.fields++p.fields));case _=>None}
        case _=>None
      merged.filter(r=> !hasPayload || Set("sample","enter","exit","lane_change","micro_update","enter_micro_link","leave_micro_link")(r.field("event_type").text()))
    }
    val events=decoded.map { original=>
      var row=original
      aliases.foreach { (from,to)=>if row.field(to)==JsNull && row.field(from)!=JsNull then row=JsObject(row.fields+(to->row.field(from))) }
      runOverride.foreach { run=>require(!row.fields.contains("run_id") || row.field("run_id").text()==run,"run_id override disagrees with observation provenance");row=JsObject(row.fields+("run_id"->JsString(run))) }
      Vector("run_id","vehicle_id","link_id","time_seconds","event_type").foreach(key=>require(row.field(key)!=JsNull,s"Missing $key"))
      val event=row.field("event_type").text() match {case "micro_update"=>"sample";case "enter_micro_link"=>"enter";case "leave_micro_link"=>"exit";case v=>v}
      require(Set("sample","enter","exit","lane_change")(event),"Unsupported observation event")
      val time=numeric(row.field("time_seconds"));require(time.isFinite,"Observation times must be finite physical seconds")
      val duration=numeric(row.field("sample_duration_seconds"));if event=="sample" then require(duration.isFinite && duration>0,"Samples require positive finite sample_duration_seconds")
      val leader=row.field("leader_id").text();val gap=if leader.isEmpty || Set("None","null","-1","-1.0")(leader) then Double.NaN else numeric(row.field("gap"))
      Observation(row.field("run_id").text(),row.field("vehicle_id").text(),row.field("link_id").text(),time,event,duration,numeric(row.field("speed")),numeric(row.field("acceleration")),gap,numeric(row.field("position")))
    }
    val samples=events.filter(_.event=="sample")
    require(samples.map(s=>(s.run,s.vehicle,s.time)).distinct.size==samples.size,"Duplicate vehicle samples")
    samples.groupBy(s=>(s.run,s.vehicle)).values.foreach { group=>
      group.sortBy(_.time).sliding(2).foreach(pair=>if pair.size==2 then require(pair(1).time-pair(1).duration>=pair(0).time-1e-9,"Overlapping observation intervals"))
    }
    events.sortBy(s=>(s.run,s.time,s.vehicle))
  private def labelTime(value: Double): String =
    val raw=String.format(java.util.Locale.ROOT,"%.6g",Double.box(value));val parts=raw.split('e')
    val base=if parts(0).contains('.') then parts(0).reverse.dropWhile(_=='0').dropWhile(_=='.').reverse else parts(0)
    base+(if parts.length>1 then "e"+parts(1) else "")
  def aggregate(frame: Vector[Observation],params: JsValue): SeriesDataset =
    val variables=params.field("variables").names
    require(variables.nonEmpty && variables.forall(units.contains),"Unknown or empty observation variables")
    val start=params.field("start_seconds").number();val end=params.field("end_seconds").number();val bin=params.field("bin_seconds").number();val window=params.field("window_bins").int()
    val representation=params.field("representation",JsString("link")).text();val alignment=params.field("alignment",JsString("clock")).text()
    require(Set("link","vehicle")(representation) && Set("clock","departure")(alignment) && (representation!="link" || alignment=="clock"),"Invalid representation/alignment")
    val snapshots=params.field("complete_snapshots").bool();val events=params.field("complete_events").bool();val countSource=params.field("count_source",JsString("samples")).text()
    require(Set("samples","events")(countSource),"Invalid count_source")
    require(snapshots || !variables.exists(v=>v=="queue" || (v=="count" && countSource=="samples")),"count/queue require complete_snapshots")
    require(events || !variables.exists(v=>v=="flow" || v=="lane_changes" || (v=="count" && countSource=="events")),"Event variables require complete_events")
    val linkIDs=params.field("link_ids").names
    require(!snapshots || (linkIDs.nonEmpty && representation=="link"),"Complete snapshots require a declared link universe")
    require(representation!="vehicle" || !variables.exists(Set("count","queue","flow")),"Vehicle trajectories do not have link count/queue/flow")
    val span=end-start;require(span.isFinite && bin.isFinite && span>0 && bin>0 && window>=1,"Invalid physical time bins")
    val total=math.rint(span/bin).toInt;require(math.abs(total*bin-span)<=1e-8+1e-5*math.abs(span) && total%window==0,"Span must contain whole bins and windows")
    val stopped=params.field("stopped_speed").number(.1);require(stopped.isFinite && stopped>=0,"Invalid stopped_speed")
    val maximumGap=Option(params.field("max_gap_seconds")).filter(_!=JsNull).map(_.number())
    val groups=params.field("groups",Json.empty)
    val rows=Vector.newBuilder[Array[Double]];val labels=Vector.newBuilder[String];val runs=Vector.newBuilder[String];val classes=Vector.newBuilder[String];val contributors=Vector.newBuilder[Vector[String]]
    frame.groupBy(_.run).toVector.sortBy(_._1).foreach { (run,records)=>
      val entities=if representation=="link" then (if params.field("link_ids")!=JsNull then linkIDs else records.map(_.link).distinct.sorted) else records.map(_.vehicle).distinct.sorted
      entities.foreach { entity=>
        var local=records.filter(r=>if representation=="link" then r.link==entity else r.vehicle==entity)
        if alignment=="departure" then
          val entries=local.filter(_.event=="enter");require(entries.nonEmpty,"Departure alignment needs an explicit enter event")
          val first=entries.map(_.time).min;local=local.map(r=>r.copy(time=r.time-first))
        val buckets=Array.fill(total)(mutable.ArrayBuffer.empty[(Observation,Double)])
        val count=Array.fill(total)(0.0);val flows=Array.fill(total)(0.0);val changes=Array.fill(total)(0.0)
        def bins(begin: Double,finish: Double)(add: (Int,Double)=>Unit): Unit =
          val left=math.max(start,begin);val right=math.min(end,finish)
          if right>left then
            val first=math.max(0,math.floor((left-start)/bin).toInt);val last=math.min(total,math.ceil((right-start)/bin).toInt)
            for i<-first until last do
              val weight=math.min(right,start+(i+1)*bin)-math.max(left,start+i*bin)
              if weight>0 then add(i,weight)
        if variables.contains("count") && countSource=="events" then
          local.groupBy(_.vehicle).values.foreach { vehicle=>
            var entered: Option[Double]=None
            vehicle.sortBy(_.time).foreach { r=>
              if r.event=="enter" then {require(entered.isEmpty,"Repeated entry without exit");entered=Some(r.time)}
              else if r.event=="exit" then {require(entered.nonEmpty,"Exit without entry");bins(entered.get,r.time)((i,w)=>count(i)+=w/bin);entered=None}
            }
            entered.foreach(left=>bins(left,end)((i,w)=>count(i)+=w/bin))
          }
        local.foreach { r=>
          if r.event=="sample" then
            require(maximumGap.forall(r.duration<=_),"Observation interval exceeds max_gap_seconds")
            bins(r.time-r.duration,r.time)((i,w)=>buckets(i)+=((r,w)))
          else
            val index=if r.time==end then total-1 else math.floor((r.time-start)/bin).toInt
            if index>=0 && index<total then
              if r.event=="exit" then flows(index)+=1/bin else if r.event=="lane_change" then changes(index)+=1/bin
        }
        val values=Array.fill(total*variables.size)(Double.NaN)
        buckets.indices.foreach { i=>
          val samples=buckets(i)
          def weighted(field: Observation=>Double,transform: Double=>Double=identity): Double =
            val valid=samples.filter((r,_)=>field(r).isFinite)
            if valid.isEmpty then Double.NaN else valid.map((r,w)=>transform(field(r))*w).sum/valid.map(_._2).sum
          def quantile(q: Double): Double =
            val sorted=samples.filter((r,_)=> !r.gap.isNaN).map((r,w)=>(r.gap,w)).sortBy(_._1)
            if sorted.isEmpty then Double.NaN else
              val threshold=q*sorted.map(_._2).sum;var sum=0.0
              sorted.find { (_,w)=>sum+=w;sum>=threshold }.map(_._1).getOrElse(sorted.last._1)
          val speed=weighted(_.speed)
          val calculated=mutable.Map[String,Double]("speed"->speed,"speed_std"->math.sqrt(weighted(_.speed,x=>math.pow(x-speed,2))),"acceleration"->weighted(_.acceleration),
            "acceleration_positive"->weighted(_.acceleration,x=>math.max(x,0)),"deceleration"->weighted(_.acceleration,x=>math.max(-x,0)),"gap"->quantile(.5),"gap_p10"->quantile(.1),"position"->weighted(_.position),
            "distance"->(if speed.isFinite then samples.filter((r,_)=> !r.speed.isNaN).map((r,w)=>r.speed*w).sum else Double.NaN))
          if snapshots then {calculated("count")=samples.map(_._2).sum/bin;calculated("queue")=samples.filter((r,_)=> !r.speed.isNaN && r.speed<=stopped).map(_._2).sum/bin}
          if variables.contains("count") && countSource=="events" then calculated("count")=count(i)
          if events then {calculated("flow")=flows(i);calculated("lane_changes")=changes(i)}
          variables.zipWithIndex.foreach { (variable,p)=>val value=calculated.getOrElse(variable,Double.NaN);values(i*variables.size+p)=if value.isFinite then value else Double.NaN }
        }
        for offset<-0 until total by window do
          val chunk=values.slice(offset*variables.size,(offset+window)*variables.size)
          if representation!="vehicle" || chunk.exists(_.isFinite) then
            val left=start+offset*bin
            rows+=chunk;labels+=s"$entity|${labelTime(left)}";runs+=run;classes+=groups.field(entity,JsString("all")).text()
            contributors+=local.filter(r=>r.time>left && r.time<=left+window*bin).map(_.vehicle).distinct.sorted
      }
    }
    val data=rows.result();require(data.nonEmpty,"No observations in requested interval")
    val metadata=obj("representation"->JsString(representation),"alignment"->JsString(alignment),"bin_seconds"->params.field("bin_seconds"),"start_seconds"->params.field("start_seconds"),"end_seconds"->params.field("end_seconds"),
      "complete_snapshots"->JsBoolean(snapshots),"complete_events"->JsBoolean(events),"count_source"->JsString(countSource),"sampling"->JsString("interval-ending endpoint hold"),"variable_units"->JsObject(variables.map(v=>v->JsString(units(v))).toMap))
    SeriesDataset(data.toArray,labels.result(),variables,Array.tabulate(window)(i=>i*bin),runs.result(),classes.result(),contributors.result(),metadata)
