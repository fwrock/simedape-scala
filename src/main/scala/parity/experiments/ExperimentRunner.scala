package org.interscity.htc.parity.experiments
import org.interscity.htc.parity.*
import Json.*
import spray.json.*
import java.nio.file.{Files,Path}
import java.util.zip.{ZipOutputStream,ZipEntry}
import java.io.BufferedOutputStream
import scala.collection.mutable
import scala.util.Using

object ExperimentRunner:
  private def predictionFile(file: Path,values: Array[Array[Double]],score: Array[Array[Boolean]],t: Int,p: Int): Unit =
    Using.resource(new ZipOutputStream(new BufferedOutputStream(Files.newOutputStream(file)))) { zip=>
      SeriesDataset.doubles(zip,"prediction",Vector(values.length,t,p),values.iterator.flatMap(_.iterator))
      zip.putNextEntry(new ZipEntry("score_mask.npy"));zip.write(Npy.header("|b1",Vector(values.length,t,p)));score.foreach(_.foreach(v=>zip.write(if v then 1 else 0)));zip.closeEntry()
    }
  private def classFingerprint(clazz: Class[?]): String =
    Using.resource(clazz.getResourceAsStream("/"+clazz.getName.replace('.','/')+".class"))(s=>java.util.HexFormat.of().formatHex(java.security.MessageDigest.getInstance("SHA-256").digest(s.readAllBytes())))
  def run(config: JsObject,base: Path,output: Path,log: String=>Unit=_=>(),check: ()=>Unit=()=>()): JsObject =
    Files.createDirectories(output)
    require(!Files.exists(output.resolve("experiment.json")),"Experiment output already exists; choose a new directory")
    val started=System.nanoTime()
    val dataset=if config.field("dataset")!=JsNull then SeriesDataset.load(base.resolve(config.field("dataset").text())) else
      val observations=Observations.read(base.resolve(config.field("observations").text()))
      val data=Observations.aggregate(observations,config.field("aggregation"));data.save(output.resolve("dataset.npz"));data
    val source=base.resolve(config.field("dataset",config.field("observations")).text())
    val sourceDigest=SeriesDataset.fingerprint(source)
    val trainRuns=config.field("training_runs").names;val testRuns=config.field("test_runs").names
    require(trainRuns.nonEmpty && testRuns.nonEmpty && (trainRuns.toSet intersect testRuns.toSet).isEmpty,"Nonempty disjoint training_runs/test_runs are required")
    require((trainRuns++testRuns).forall(dataset.runs.toSet),"Unknown run in explicit train/test split")
    val training=dataset.forRuns(trainRuns);val testing=dataset.forRuns(testRuns)
    val mode=config.field("experiment",JsString("masked")).text();require(Set("masked","reduced")(mode),"experiment must be masked or reduced")
    val models=config.field("models",JsArray(obj("name"->JsString("joint-pca"),"method"->JsString("pca")))).values
    val names=models.map(_.field("name").text());require(names.nonEmpty && names.forall(_.nonEmpty) && names.distinct.size==names.size,"Model names must be unique and nonempty")
    val fractions=config.field("fractions",arr(Vector(.25))).values.map(_.number());val seeds=config.field("seeds",ints(Vector(0))).values.map(_.number().toLong)
    require(fractions.nonEmpty && seeds.nonEmpty && fractions.distinct.size==fractions.size && seeds.distinct.size==seeds.size,"Use nonempty distinct fractions and seeds")
    require(fractions.forall(f=>f.isFinite && f>0 && f<=1),"fractions must be in (0,1]")
    val simulator=if mode=="reduced" then Some(new CommandSimulator(config.field("simulator"),base,check)) else None
    val results=Vector.newBuilder[JsValue];var caseCount=0
    seeds.foreach { seed=>models.zipWithIndex.foreach { (definition,modelIndex)=>
      check()
      val settings=JsObject((definition.asJsObject.fields--Set("name","selection"))+("seed"->JsNumber(seed)))
      val paired=settings.field("method").text()=="paired_residual"
      val fitStart=System.nanoTime()
      var pattern: Option[PatternEstimator]=None;var calibration: Option[PairedResidualEstimator]=None
      if paired then
        require(mode=="reduced","paired_residual requires genuine reduced-simulation experiments")
        require(definition.field("selection")==JsNull,"Paired selection is fixed by calibration context")
        val source=base.resolve(settings.field("paired_training_dataset").text())
        val model=new PairedResidualEstimator(settings.field("ridge").number(1),settings.field("nonnegative").bool(true),settings.field("departure_tolerance_seconds").number(1)).fit(training,SeriesDataset.load(source))
        model.sourceHash=Some(SeriesDataset.fingerprint(source));calibration=Some(model)
      else pattern=Some(new PatternEstimator(settings).fit(training))
      val fitSeconds=(System.nanoTime()-fitStart)/1e9
      val fitDir=output.resolve(s"fit-$seed-$modelIndex");Files.createDirectory(fitDir)
      val modelFile=fitDir.resolve("model.simedape")
      pattern.foreach(ModelArtifacts.write(modelFile,_));calibration.foreach(ModelArtifacts.write(modelFile,_))
      fractions.foreach { fraction=>
        check();val id=f"case-$caseCount%04d";caseCount+=1
        val folder=output.resolve(id);Files.createDirectory(folder)
        val selection=if paired then "calibration_context" else definition.field("selection",config.field("selection",JsString("medoid"))).text()
        val budget=math.max(1,math.ceil(training.labels.distinct.size*fraction).toInt)
        val selected=pattern.map(_.selectDistinctLabels(budget,selection,seed)).getOrElse(calibration.get.selectDistinctLabels(budget))
        val selectedLabels=selected.map(training.labels)
        var simulation=Vector.empty[JsValue]
        val (partial,score)=if mode=="masked" then
          val mask=Evaluation.mask(testing,fraction,seed,config.field("mask",JsString("random")).text(),Some(selectedLabels))
          val observed=testing.values.indices.map(i=>testing.values(i).indices.map(j=>if mask(i)(j) then testing.values(i)(j) else Double.NaN).toArray).toArray
          (testing.copy(values=observed),testing.values.indices.map(i=>testing.values(i).indices.map(j=>testing.values(i)(j).isFinite && !mask(i)(j)).toArray).toArray)
        else
          val plan=Reduction.plan(training,selected,config.field("reduction"),seed,base)
          val chunks=testRuns.zipWithIndex.map { (run,index)=>
            val (produced,receipt)=simulator.get.run(plan,folder.resolve(s"simulation-$index"),run)
            val data=if produced.metadata.field("observation_semantics")==JsNull then produced.copy(metadata=JsObject(produced.metadata.fields+("observation_semantics"->JsString("reduced_proxy")))) else produced
            Reduction.align(testing.forRuns(Vector(run)),data);simulation:+=receipt;data
          }
          val partial=Reduction.align(testing,SeriesDataset.combine(chunks))
          (partial,testing.values.map(_.map(_.isFinite)))
        val predictionStart=System.nanoTime()
        val predicted=pattern.map(_.predict(partial,allowReducedProxy=mode=="reduced")).getOrElse(calibration.get.predict(partial))
        val predictionSeconds=(System.nanoTime()-predictionStart)/1e9
        val metrics=Evaluation.evaluate(testing,predicted,score)
        val perRun=JsObject(testRuns.map { run=>
          val indices=testing.runs.indices.filter(testing.runs(_)==run).toVector
          run->Evaluation.evaluate(testing.subset(indices),indices.map(predicted).toArray,indices.map(score).toArray)
        }.toMap)
        partial.save(folder.resolve("observations.npz"));predictionFile(folder.resolve("predictions.npz"),predicted,score,testing.t,testing.p)
        Json.write(folder.resolve("model.json"),pattern.map(_.summary).getOrElse(calibration.get.summary))
        Json.write(folder.resolve("diagnostics.json"),JsArray(pattern.map(_.diagnostics).getOrElse(calibration.get.diagnostics)))
        val row=obj("id"->JsString(id),"name"->JsString(names(modelIndex)),"seed"->JsNumber(seed),"fraction"->num(fraction),"experiment"->JsString(mode),"selection"->JsString(selection),"representative_budget"->JsNumber(budget),
          "selection_applied"->JsBoolean(mode=="reduced" || config.field("mask",JsString("random")).text()=="representatives"),"input_semantics"->partial.metadata.field("observation_semantics",JsString("full")),
          "uncalibrated_proxy_ablation"->JsBoolean(pattern.nonEmpty && partial.metadata.field("observation_semantics").text()=="reduced_proxy"),"model_artifact"->JsString(output.relativize(modelFile).toString),"selected_labels"->strings(selectedLabels),
          "observed_values"->JsNumber(partial.observed),"reference_values"->JsNumber(testing.observed),"fit_seconds"->num(fitSeconds),"prediction_seconds"->num(predictionSeconds),"metrics"->metrics,"per_run"->perRun,"simulation"->JsArray(simulation))
        Json.write(folder.resolve("result.json"),row);results+=row;log(s"$id ${names(modelIndex)}: ${fraction*100}% complete")
      }
    } }
    val rows=results.result()
    val summary=names.flatMap { name=>fractions.flatMap { fraction=>testing.variables.map { variable=>
      val errors=rows.filter(r=>r.field("name").text()==name && r.field("fraction").number()==fraction).map(_.field("metrics").field(variable).field("mae")).collect {case JsNumber(n)=>n.toDouble}
      val mean=if errors.isEmpty then Double.NaN else errors.sum/errors.size
      obj("name"->JsString(name),"fraction"->num(fraction),"variable"->JsString(variable),"seeds"->JsNumber(errors.size),"mae_mean"->num(mean),"mae_std_across_seeds"->num(if errors.size<2 then Double.NaN else math.sqrt(errors.map(x=>math.pow(x-mean,2)).sum/(errors.size-1))))
    } } }
    val classes=Vector(classOf[PatternEstimator],classOf[MaskedPCA],classOf[PairedResidualEstimator],classOf[SeriesDataset],classOf[CommandSimulator],classOf[org.tslearn.utils.NativeLinearAlgebra],classOf[org.tslearn.utils.NumpyGenerator],ExperimentRunner.getClass,MaskedPCA.getClass,Distances.getClass,Evaluation.getClass,Observations.getClass,Reduction.getClass,Provenance.getClass,ModelArtifacts.getClass)
    val report=obj("schema"->JsString("simedape.experiment.v1"),"config"->config,"source_sha256"->JsString(sourceDigest),"implementation_files"->JsObject(classes.map(c=>c.getName->JsString(classFingerprint(c))).toMap),
      "training_runs"->strings(trainRuns),"test_runs"->strings(testRuns),"versions"->obj("java"->JsString(System.getProperty("java.version")),"scala"->JsString("3.3.9"),"tslearn4j"->JsString("1.1.0-SNAPSHOT"),"duckdb_jdbc"->JsString("2.0.0.1")),
      "wall_seconds"->num((System.nanoTime()-started)/1e9),"jvm_heap_used_bytes"->JsNumber(Runtime.getRuntime.totalMemory()-Runtime.getRuntime.freeMemory()),"memory_scope"->JsString("Current JVM heap usage after experiments; excludes native allocations and simulator memory; not peak RSS"),"results"->JsArray(rows),"summary"->JsArray(summary))
    Json.write(output.resolve("experiment.json"),report);report
