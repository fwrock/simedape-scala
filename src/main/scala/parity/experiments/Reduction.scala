package org.interscity.htc.parity.experiments
import org.interscity.htc.parity.*
import Json.*
import spray.json.*
import java.nio.file.{Files,Path}
import java.util.concurrent.TimeUnit
import scala.jdk.CollectionConverters.*
import scala.collection.mutable

object Reduction:
  val policies=Set("vehicle_sample","neighborhood","selected_links","meso_background")
  private def link(label: String): String = label.substring(0,label.lastIndexOf('|').max(0))
  def plan(data: SeriesDataset,selected: Vector[Int],params: JsValue,seed: Long,base: Path): JsObject =
    val policy=params.field("policy").text();require(policies(policy),"Unknown reduction policy")
    require(selected.nonEmpty && selected.distinct.size==selected.size && selected.forall(i=>i>=0 && i<data.n),"Select distinct valid training series")
    val hops=params.field("hops").int(1);require(hops>=0,"hops must be nonnegative")
    val isLink=data.metadata.field("representation").text()=="link"
    require(policy=="vehicle_sample" || isLink,"Link policies require a link representation")
    val links=mutable.Set.from(if isLink then selected.map(i=>link(data.labels(i))) else Vector.empty)
    val core=links.toSet;val allLinks=if isLink then data.labels.map(link).toSet else Set.empty[String]
    if policy=="neighborhood" then
      val adjacency=params.field("adjacency");require(adjacency.isInstanceOf[JsObject],"neighborhood requires explicit adjacency")
      val graph=mutable.Map.empty[String,mutable.Set[String]]
      adjacency.asJsObject.fields.foreach { (node,neighbors)=>
        val ns=graph.getOrElseUpdate(node,mutable.Set.empty)
        neighbors.names.foreach { other=>ns+=other;graph.getOrElseUpdate(other,mutable.Set.empty)+=node }
      }
      require(core.subsetOf(graph.keySet.toSet),"Selected links are absent from adjacency")
      var frontier=core
      for _<-0 until hops do {frontier=frontier.flatMap(l=>graph.getOrElse(l,mutable.Set.empty).toSet)--links;links++=frontier}
    val selectedLabels=selected.map(data.labels).toSet
    val chosen=if policy=="meso_background" then data.labels.indices.toVector else data.labels.indices.filter(i=>if policy=="neighborhood" then links(link(data.labels(i))) else selectedLabels(data.labels(i))).toVector
    val units=mutable.Set.from(chosen.flatMap(data.units));require(units.nonEmpty,"Selected series have no executable unit identities")
    val dependencies=params.field("dependencies",Json.empty);val pending=mutable.Queue.from(units)
    while pending.nonEmpty do
      dependencies.field(pending.dequeue()).names.foreach { unit=>if !units(unit) then {units+=unit;pending.enqueue(unit)} }
    val boundary=if policy!="selected_links" then JsNull else
      val file=params.field("boundary_file").text();require(file.nonEmpty,"selected_links requires explicit boundary demand")
      val path=base.resolve(file).toAbsolutePath.normalize()
      obj("path"->JsString(path.toString),"sha256"->JsString(SeriesDataset.fingerprint(path)))
    obj("schema"->JsString("simedape.reduction.v1"),"policy"->JsString(policy),"seed"->JsNumber(seed),"training_runs"->strings(data.runs.distinct.sorted),
      "selected_series"->strings(selected.map(data.labels)),"selected_units"->strings(units.toVector.sorted),"core_links"->strings(core.toVector.sorted),"simulated_links"->strings(links.toVector.sorted),
      "background_links"->strings(if policy=="meso_background" then (allLinks--links).toVector.sorted else Vector.empty),"background_mode"->(if policy=="meso_background" then JsString("MESO") else JsNull),"boundary_demand"->boundary,
      "diagnostics"->obj("representative_series"->JsNumber(selected.size),"retained_units"->JsNumber(units.size),"universe_units"->JsNumber(data.units.flatten.distinct.size),"retained_links"->JsNumber(links.size)))
  def align(reference: SeriesDataset,partial: SeriesDataset): SeriesDataset =
    Provenance.compatible(reference,partial)
    val identities=reference.runs.zip(reference.labels).toSet
    require(partial.runs.zip(partial.labels).forall(identities),"Simulator produced unknown run/series identities")
    val lookup=partial.runs.zip(partial.labels).zipWithIndex.toMap
    val values=reference.runs.zip(reference.labels).map(key=>lookup.get(key).map(i=>partial.values(i).clone()).getOrElse(Array.fill(reference.t*reference.p)(Double.NaN))).toArray
    val carry=Set("observation_semantics","reduction_context","run_provenance")
    reference.copy(values=values,metadata=JsObject(reference.metadata.fields.filterNot((k,_)=>carry(k))++partial.metadata.fields.filter((k,_)=>carry(k))))

final class CommandSimulator(settings: JsValue,base: Path,check: ()=>Unit=()=>()):
  val command=settings.field("command").names;val capabilities=settings.field("capabilities").names.toSet
  val timeout=settings.field("timeout_seconds").number(300)
  val cwd=settings.field("cwd").text()
  require(command.nonEmpty && capabilities.nonEmpty && capabilities.subsetOf(Reduction.policies) && timeout>0 && timeout.isFinite,"Invalid simulator command/capabilities/timeout")
  private def stop(process: Process): Unit =
    val descendants=process.toHandle.descendants().iterator().asScala.toVector.reverse
    descendants.foreach(_.destroyForcibly());process.destroyForcibly();process.waitFor(10,TimeUnit.SECONDS)
  def run(plan: JsObject,output: Path,runId: String): (SeriesDataset,JsObject) =
    val policy=plan.field("policy").text();require(capabilities(policy),s"Simulator does not support $policy")
    Files.createDirectories(output.toAbsolutePath.getParent);Files.createDirectory(output)
    val manifest=output.toAbsolutePath.resolve("reduction.json");Json.write(manifest,plan)
    val digest=SeriesDataset.fingerprint(manifest)
    val substitutions=Map("manifest"->manifest.toString,"output_dir"->output.toAbsolutePath.toString,"run_id"->runId,"seed"->plan.field("seed").text())
    val args=command.map { arg=>substitutions.foldLeft(arg)((text,pair)=>text.replace("{"+pair._1+"}",pair._2)) }
    val started=System.nanoTime();check()
    val process=new ProcessBuilder(args.asJava).directory((if cwd.isEmpty then output.toAbsolutePath else base.resolve(cwd).toAbsolutePath).toFile)
      .redirectErrorStream(true).redirectOutput(output.resolve("simulator.log").toFile).start()
    try
      var complete=false
      while !complete do
        check()
        if (System.nanoTime()-started)/1e9>timeout then throw new java.util.concurrent.TimeoutException(s"Simulator exceeded ${timeout}s; see ${output.resolve("simulator.log")}")
        complete=process.waitFor(100,TimeUnit.MILLISECONDS)
      require(process.exitValue()==0,s"Simulator exited ${process.exitValue()}; see ${output.resolve("simulator.log")}")
    finally if process.isAlive then stop(process)
    val receiptFile=output.resolve("receipt.json");require(Files.isRegularFile(receiptFile),"Simulator produced no completion receipt")
    val receipt=Json.read(receiptFile)
    require(receipt.field("schema").text()=="simedape.simulator_receipt.v1" && receipt.field("status").text()=="complete" && receipt.field("run_id").text()==runId && receipt.field("manifest_sha256").text()==digest && receipt.field("policy")==plan.field("policy") && receipt.field("seed")==plan.field("seed"),"Simulator receipt disagrees with manifest or completion")
    require(receipt.field("instantiated_units").names.sorted==plan.field("selected_units").names.sorted,"Simulator instantiated units differ from selection")
    val dataset=Json.safe(output,receipt.field("dataset").text())
    val expected=receipt.field("dataset_sha256").text()
    require(expected.isEmpty || SeriesDataset.fingerprint(dataset)==expected,"Simulator dataset fingerprint disagrees with receipt")
    val data=SeriesDataset.load(dataset);require(data.runs.toSet==Set(runId),"Simulator observations have incorrect run provenance")
    (data,obj("wall_seconds"->num((System.nanoTime()-started)/1e9),"command"->strings(args),"receipt"->receipt,"output_dir"->JsString(output.toAbsolutePath.toString)))
