package org.interscity.htc.parity

import spray.json.*
import Json.*
import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import org.tslearn.utils.PythonRandom
import java.nio.file.{Files,Path}
import scala.util.Using

final case class Universe(units: Vector[String],vehicles: Map[String,Vector[String]],series: Vector[Set[String]],
  labels: Vector[String],clusters: Vector[Int],primary: Map[String,Int],counts: Map[Int,Int],unmapped: Int,kind: String):
  lazy val unitSeries: Map[String,Vector[Int]] = series.indices.flatMap(i=>series(i).map(_->i)).groupMap(_._1)(_._2).view.mapValues(_.toVector).toMap
  def onLinks(links: Set[String]): Set[String] = labels.indices.filter(i=>links.contains(labels(i).takeWhile(_!='-'))).flatMap(series).toSet

object Selection:
  val strategies=Vector("random","cluster_uniform","cluster_weighted","simulation_points","simulation_points_weighted","greedy_coverage","greedy_coverage_balanced","centroid_nearest","topology_balanced","topology_weighted")
  val spStrategies=Set("simulation_points","simulation_points_weighted","topology_balanced","topology_weighted")
  val clusterStrategies=Set("cluster_uniform","cluster_weighted","simulation_points_weighted","greedy_coverage_balanced","centroid_nearest","topology_balanced","topology_weighted")
  def universe(vehicles: Vector[Vector[String]],labels: Vector[String],clusters: Vector[Int],mapping: Option[Map[String,String]]=None,kind: String="vehicle"): Universe =
    require(vehicles.size==labels.size && clusters.size==labels.size,"Series, membership and clusters must align")
    val owners=mutable.Map.empty[String,mutable.Set[String]];val unmapped=mutable.Set.empty[String]
    val series=vehicles.map(_.map { v =>
      val unit=mapping.flatMap(_.get(v)).getOrElse { if mapping.nonEmpty then unmapped+=v;v }
      owners.getOrElseUpdate(unit,mutable.Set.empty)+=v;unit
    }.toSet)
    val primary=owners.keysIterator.map { u =>
      val counts=series.indices.filter(series(_).contains(u)).groupMapReduce(clusters(_))(_=>1)(_+_)
      u->counts.keys.minBy(c=>(-counts(c),c))
    }.toMap
    Universe(owners.keys.toVector.sorted,owners.toMap.view.mapValues(_.toVector.sorted).toMap,series,labels,clusters,primary,clusters.groupMapReduce(identity)(_=>1)(_+_),unmapped.size,kind)

  def quotas(weights: Map[Int,Double],capacities: Map[Int,Int],total: Int): Map[Int,Int] =
    val result=mutable.Map.from(weights.keys.map(_->0));var remaining=total
    var active=weights.keys.filter(k=>capacities.getOrElse(k,0)>0 && weights(k)>0).toSet
    while remaining>0 && active.nonEmpty do
      val sum=active.toVector.sorted.map(weights).sum
      val raw=active.map(k=>k->(remaining*weights(k)/sum)).toMap
      val give=mutable.Map.from(active.map(k=>k->math.min(raw(k).toInt,capacities(k)-result(k))))
      var left=remaining-give.values.sum
      active.toVector.sortBy(k=>(-(raw(k)-raw(k).toInt),k.toString)).foreach { k =>
        if left>0 && result(k)+give(k)<capacities(k) then {give(k)+=1;left-=1}
      }
      val granted=give.values.sum
      if granted==0 then active=Set.empty
      else
        give.foreach((k,g)=>result(k)+=g);remaining-=granted
        active=active.filter(k=>result(k)<capacities(k))
    result.toMap

  def bfs(seeds: Set[String],topology: Map[String,Set[String]]): Vector[String] =
    val seen=mutable.Set.from(seeds);var frontier=seeds.toVector.sorted;val order=mutable.ArrayBuffer.from(frontier)
    while frontier.nonEmpty do
      val next=frontier.flatMap(l=>topology.getOrElse(l,Set.empty)).filterNot(seen).distinct.sorted
      seen++=next;order++=next;frontier=next
    order.toVector

  def manifest(u: Universe,scenario: String,params: JsValue,sp: Vector[Int]=Vector.empty,distances: Vector[Double]=Vector.empty,
    topology: Map[String,Set[String]]=Map.empty,decisions: Map[String,Vector[JsValue]]=Map.empty): JsObject =
    val strategy=params.field("strategy",JsString("random")).text()
    require(strategies.contains(strategy),s"Unknown selection strategy: $strategy")
    if clusterStrategies(strategy) then require(!u.clusters.contains(-1),s"$strategy requires clustering")
    if spStrategies(strategy) then require(sp.nonEmpty,s"$strategy requires simulation points")
    if strategy.startsWith("topology_") then require(topology.nonEmpty,"Topology is required")
    if strategy=="centroid_nearest" then require(distances.size==u.labels.size,"centroid_nearest requires normalized series")
    val percentage=params.field("percentage",JsNumber(10))
    if percentage==JsNull then require(strategy=="simulation_points","percentage is required")
    else require(percentage.number()>0 && percentage.number()<=100,"percentage must be in (0,100]")
    val target=if percentage==JsNull then None else Some(math.max(1,math.min(u.units.size,math.rint(u.units.size*percentage.number()/100).toInt)))
    val seed=params.field("seed").number(42).toLong;val rng=new PythonRandom(seed)
    val forceLinks=params.field("force_links").names;val forced=u.onLinks(forceLinks.toSet)
    val selected=mutable.Set.from(forced);val warnings=mutable.ArrayBuffer.empty[String]
    if target.exists(_<forced.size) then warnings+=s"${forced.size} unidades forçadas excedem o alvo de ${target.get}; nenhuma outra foi adicionada."
    val spBy=sp.groupBy(u.clusters).view.mapValues(_.flatMap(u.series).toSet).toMap
    val spUnits=sp.flatMap(u.series).toSet
    def need: Int = target.map(t=>math.max(0,t-selected.size)).getOrElse(0)
    def sample(pool: Iterable[String],k: Int): Set[String] =
      val sorted=pool.toVector.distinct.sorted;rng.sample(sorted.asJava,math.min(k,sorted.size)).asScala.toSet
    def fill(): Unit = if params.field("fill_to_target").bool(true) then selected++=sample(u.units.filterNot(selected),need)
    strategy match
      case "random" => selected++=sample(u.units.filterNot(selected),need)
      case "cluster_uniform" | "cluster_weighted" =>
        val members=u.units.filterNot(selected).groupBy(u.primary)
        val weights=members.keys.map(c=>c->(if strategy=="cluster_uniform" then 1.0 else u.counts(c).toDouble)).toMap
        quotas(weights,members.view.mapValues(_.size).toMap,need).toVector.sortBy(_._1).foreach((c,q)=>selected++=sample(members(c),q))
      case "simulation_points" =>
        selected++=spUnits
        if target.nonEmpty then
          if selected.size>target.get then
            val keep=sample(selected.toSet--forced,math.max(0,target.get-forced.size));selected.clear();selected++=forced++keep
          else fill()
      case "greedy_coverage" | "greedy_coverage_balanced" =>
        val covered=u.series.map(_.exists(selected)).toArray
        val weights=u.clusters.map(c=>if strategy=="greedy_coverage" then 1.0 else 1.0/u.counts(c))
        def gain(unit: String): Double = u.unitSeries(unit).filterNot(covered).map(weights).sum
        val comparator=new java.util.Comparator[(Double,String)]:
          def compare(a: (Double,String),b: (Double,String)): Int =
            val cmp=java.lang.Double.compare(a._1,b._1);if cmp!=0 then cmp else a._2.compareTo(b._2)
        val heap=new java.util.PriorityQueue[(Double,String)](comparator)
        u.units.filterNot(selected).foreach(v=>heap.add((-gain(v),v)))
        val stop=selected.size+need
        while !heap.isEmpty && selected.size<stop do
          val (_,unit)=heap.remove();val fresh=gain(unit)
          if fresh>0 then
            if !heap.isEmpty && fresh < -heap.peek()._1-1e-12 then heap.add((-fresh,unit))
            else {selected+=unit;u.unitSeries(unit).foreach(i=>covered(i)=true)}
        fill()
      case "centroid_nearest" =>
        val groups=u.labels.indices.toVector.groupBy(u.clusters).view.mapValues(_.sortBy(i=>(distances(i),i))).toMap
        val caps=groups.map((c,is)=>c->(is.flatMap(u.series).toSet--selected).size)
        val qs=quotas(groups.keys.map(c=>c->u.counts(c).toDouble).toMap,caps,need)
        qs.toVector.sortBy(_._1).foreach { (c,q)=>
          var taken=0
          groups(c).foreach(i=>u.series(i).toVector.sorted.foreach { unit=>if taken<q && !selected(unit) then {selected+=unit;taken+=1} })
        };fill()
      case "simulation_points_weighted" =>
        val members=u.units.groupBy(u.primary);val pools=mutable.LinkedHashMap.empty[Int,Vector[String]]
        u.units.map(u.primary).distinct.foreach { c=>
          val spc=spBy.getOrElse(c,Set.empty);val rest=new java.util.ArrayList[String]((members(c).toSet--spc).toVector.sorted.asJava)
          rng.shuffle(rest);pools(c)=(spc.toVector.sorted++rest.asScala).filterNot(selected)
        }
        val qs=quotas(pools.keys.map(c=>c->spBy.getOrElse(c,Set.empty).size.toDouble).toMap,pools.toMap.view.mapValues(_.size).toMap,need)
        qs.toVector.sortBy(_._1).foreach { (c,q)=>
          var taken=0;pools(c).foreach(unit=>if taken<q && !selected(unit) then {selected+=unit;taken+=1})
        };fill()
      case "topology_balanced" | "topology_weighted" =>
        val linkUnits=u.labels.indices.groupMap(i=>u.labels(i).takeWhile(_!='-'))(u.series).view.mapValues(_.flatten.toSet).toMap
        val seeds=sp.groupBy(u.clusters).view.mapValues(_.map(i=>u.labels(i).takeWhile(_!='-')).toSet).toMap
        val orders=seeds.map((c,ss)=>c->bfs(ss,topology))
        seeds.toVector.sortBy(_._1).foreach { (c,ss)=>
          val missing=ss.filterNot(topology.contains).toVector.sorted
          if missing.nonEmpty then warnings+=s"Cluster $c: via(s) do simulation point fora da topologia (${missing.take(3).mkString(", ")}); sem vizinhas."
        }
        val caps=orders.map((c,ls)=>c->(ls.flatMap(l=>linkUnits.getOrElse(l,Set.empty)).toSet--selected).size)
        val weights=orders.keys.map(c=>c->(if strategy=="topology_balanced" then 1.0 else u.counts(c).toDouble)).toMap
        quotas(weights,caps,need).toVector.sortBy(_._1).foreach { (c,q)=>
          var taken=0
          orders(c).foreach { link=>
            if taken<q then
              val fresh=(linkUnits.getOrElse(link,Set.empty)--selected).toVector.sorted
              val chosen=if fresh.size+taken>q then sample(fresh,q-taken) else fresh.toSet
              selected++=chosen;taken+=chosen.size
          }
        };fill()
      case _ => throw IllegalArgumentException("Unknown strategy")
    def pct(n: Int,d: Int): JsValue = num(if d==0 then 0 else math.rint(n*10000.0/d)/100)
    val covered=u.series.map(_.exists(selected))
    val per=if u.clusters.contains(-1) then empty else JsObject(u.counts.map { (c,n)=>c.toString->obj("series"->JsNumber(n),"covered_pct"->pct(u.clusters.indices.count(i=>u.clusters(i)==c && covered(i)),n)) })
    val policy=params.field("mode_policy",JsString("free")).text();require(Set("free","frozen")(policy),"Invalid mode_policy")
    var without=0
    val entries=selected.toVector.sorted.map { unit=>
      val fields=mutable.Map[String,JsValue]("unit"->JsString(unit),"forced"->JsBoolean(forced(unit)))
      if u.clusters.nonEmpty && !u.clusters.contains(-1) then fields("cluster")=JsNumber(u.primary(unit))
      if spUnits.nonEmpty then fields("simulation_point")=JsBoolean(spUnits(unit))
      if u.kind!="vehicle" then fields("vehicles")=strings(u.vehicles(unit))
      if policy=="frozen" then
        decisions.get(unit).filter(_.nonEmpty) match
          case Some(rows)=>fields("modes")=JsArray(rows.zipWithIndex.map((r,i)=>obj("trip_index"->r.field("trip_index",JsNumber(i)),"mode"->r.field("mode"))))
          case None=>without+=1
      JsObject(fields.toMap)
    }
    if policy=="frozen" && without>0 then warnings+=s"$without unidades selecionadas sem decisões no log; o modo delas ficará livre no engine."
    val diag=mutable.Map[String,JsValue]("units_total"->JsNumber(u.units.size),"units_selected"->JsNumber(selected.size),"achieved_pct"->pct(selected.size,u.units.size),
      "series_total"->JsNumber(covered.size),"series_covered_pct"->pct(covered.count(identity),covered.size),"per_cluster"->per,
      "forced_units"->JsNumber(forced.size),"unmapped_vehicles"->JsNumber(u.unmapped),"warnings"->strings(warnings.toVector))
    if spUnits.nonEmpty then diag("simulation_points_covered_pct")=pct(spUnits.count(selected),spUnits.size)
    if policy=="frozen" then diag("units_without_decisions")=JsNumber(without)
    obj("schema"->JsString("htc.simedape.selection.v1"),"scenario_id"->JsString(scenario),"unit_kind"->JsString(u.kind),
      "strategy"->obj("name"->JsString(strategy),"percentage"->percentage,"target_units"->target.map(JsNumber(_)).getOrElse(JsNull),"seed"->JsNumber(seed),"force_links"->strings(forceLinks)),
      "mode_policy"->JsString(policy),"universe"->obj("units"->JsNumber(u.units.size),"series"->JsNumber(u.labels.size),"clusters"->JsNumber(if u.clusters.contains(-1) then 0 else u.counts.size)),
      "selected"->JsArray(entries),"diagnostics"->JsObject(diag.toMap))

  def read(path: Path): JsValue = Files.readString(path).parseJson
  private def files(path: Path,prefix: String): Vector[Path] =
    if !Files.isDirectory(path) then Vector(path)
    else Using.resource(Files.list(path))(_.iterator().asScala.filter(p=>p.getFileName.toString.startsWith(prefix) && p.toString.endsWith(".json")).toVector.sortBy(_.toString))
  def unitMap(path: Path): Map[String,String] = files(path,"persons").flatMap { file=>
    read(file) match
      case o: JsObject=>o.fields.toVector.map((k,v)=>k->v.text())
      case a: JsArray=>a.elements.flatMap { person=>
        person.field("data").field("content").field("ownedVehicles",empty).asJsObject.fields.values.flatMap(v=>Option(v.field("id")).filter(_!=JsNull).map(_.text()->person.field("id").text()))
      }
      case _=>throw IllegalArgumentException("Invalid unit map")
  }.toMap
  def decisions(path: Path): Map[String,Vector[JsValue]] = Using.resource(Files.lines(path)) { lines=>
    lines.iterator().asScala.filter(_.trim.nonEmpty).map(_.parseJson).filter(r=>r.field("unit",r.field("person_id"))!=JsNull && r.field("mode")!=JsNull)
      .toVector.groupBy(r=>r.field("unit",r.field("person_id")).text())
      .view.mapValues(_.sortBy(r=>(r.field("trip_index").int(),r.field("tick").number()))).toMap
  }
  def topology(path: Path): Map[String,Set[String]] =
    val candidates=if !Files.isDirectory(path) then Vector(path) else Vector(path,path.resolve("data")).iterator.map { base=>
      if Files.isRegularFile(base.resolve("city_map.json")) then Vector(base.resolve("city_map.json")) else if Files.isDirectory(base) then files(base,"links") else Vector.empty
    }.find(_.nonEmpty).getOrElse(Vector.empty)
    require(candidates.nonEmpty,s"No topology found at $path")
    val adjacency=mutable.Map.empty[String,mutable.Set[String]];val ends=mutable.Map.empty[String,mutable.Set[String]]
    def addNode(node: JsValue,link: String): Unit = if node!=JsNull then ends.getOrElseUpdate(node.text(),mutable.Set.empty)+=link
    candidates.foreach { file=>read(file) match
      case o: JsObject if o.fields.contains("edges")=>o.field("edges").values.foreach { edge=>
        val link=edge.field("label").field("id",edge.field("id")).text();addNode(edge.field("source_id"),link);addNode(edge.field("target_id"),link)
      }
      case o: JsObject=>o.fields.foreach { (link,ns)=>ns.names.foreach { n=>
        adjacency.getOrElseUpdate(link,mutable.Set.empty)+=n;adjacency.getOrElseUpdate(n,mutable.Set.empty)+=link
      } }
      case a: JsArray=>a.elements.foreach { actor=>
        val c=actor.field("data").field("content");if c.field("from")!=JsNull && c.field("to")!=JsNull then {addNode(c.field("from"),actor.field("id").text());addNode(c.field("to"),actor.field("id").text())}
      }
      case _=>throw IllegalArgumentException("Invalid topology")
    }
    ends.values.foreach(ls=>ls.foreach(l=>adjacency.getOrElseUpdate(l,mutable.Set.empty)++=(ls.toSet-l)))
    adjacency.toMap.view.mapValues(_.toSet).toMap

  def selective(model: TrainedModel,scenario: String,bufferRatio: Double): JsObject =
    require(bufferRatio>=0 && bufferRatio.isFinite,"Invalid buffer ratio")
    val points=model.clusters.simulationPoints.toVector;val labels=points.map(model.labels)
    val core=points.flatMap(model.vehicles).toSet;val all=model.vehicles.flatten.toSet
    val agents=core++(all--core).toVector.sorted.take(math.ceil(all.size*bufferRatio).toInt)
    obj("scenario_id"->JsString(scenario),"partial_sim_ratio"->num(math.rint(model.clusters.count*10000.0/model.size)/10000),
      "selected_links"->strings(labels.map(_.takeWhile(_!='-')).distinct.sorted),"selected_routes"->strings(labels),"selected_agent_ids"->strings(agents.toVector.sorted),
      "centroid_to_series_mapping"->JsObject((0 until model.clusters.count).map(k=>k.toString->strings(model.clusters.members(k).map(model.labels).toVector)).toMap))
