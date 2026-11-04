package org.interscity.htc.parity.experiments
import org.interscity.htc.parity.*
import Json.*
import spray.json.*
import java.nio.file.{Files,Path,StandardCopyOption}
import java.util.zip.{ZipOutputStream,ZipEntry,ZipFile}
import java.io.BufferedOutputStream
import scala.util.Using

/** Portable primitive-array experiment models: no pickle, framework objects or refitting at load time. */
object ModelArtifacts:
  private final class Writer(file: Path) extends AutoCloseable:
    Files.createDirectories(file.toAbsolutePath.getParent)
    private val temporary=Files.createTempFile(file.toAbsolutePath.getParent,".experiment-model-",".tmp")
    private val zip=new ZipOutputStream(new BufferedOutputStream(Files.newOutputStream(temporary)))
    private val members=scala.collection.mutable.Map.empty[String,JsValue]
    private var committed=false
    private def entry(name: String,info: JsObject)(write: =>Unit): Unit =
      val e=new ZipEntry(name);zip.putNextEntry(e);write;zip.closeEntry()
      members(name)=JsObject(info.fields++Map("raw_bytes"->JsNumber(e.getSize),"stored_bytes"->JsNumber(e.getCompressedSize)))
    def json(name: String,value: JsValue): Unit = entry(name+".json",obj("kind"->JsString("json")))(zip.write(value.compactPrint.getBytes(java.nio.charset.StandardCharsets.UTF_8)))
    def array(name: String,shape: Vector[Int],values: Iterator[Double]): Unit = entry(name+".npy",obj("kind"->JsString("array"),"dtype"->JsString("float64"),"shape"->ints(shape))) {
      zip.write(Npy.header("<f8",shape));values.foreach(Npy.writeDouble(zip,_))
    }
    def dataset(name: String,data: SeriesDataset): Unit =
      val temporaryData=Files.createTempFile(file.toAbsolutePath.getParent,".dataset-",".npz")
      try {data.save(temporaryData);entry(name+".npz",obj("kind"->JsString("series_dataset")))(Files.copy(temporaryData,zip))}
      finally Files.deleteIfExists(temporaryData)
    def commit(format: String,meta: JsValue,summary: JsValue): Unit =
      val manifest=obj("format"->JsString(format),"created"->JsString(java.time.Instant.now().toString),"meta"->meta,"summary"->summary,"members"->JsObject(members.toMap),
        "total_raw_bytes"->num(members.values.map(_.field("raw_bytes").number()).sum),"total_stored_bytes"->num(members.values.map(_.field("stored_bytes").number()).sum))
      json("manifest",manifest);zip.close();Files.move(temporary,file.toAbsolutePath,StandardCopyOption.REPLACE_EXISTING);committed=true
    def close(): Unit = {zip.close();if !committed then Files.deleteIfExists(temporary)}
  private final class Reader(file: Path) extends AutoCloseable:
    val zip=new ZipFile(file.toFile)
    def json(name: String): JsValue = Using.resource(zip.getInputStream(zip.getEntry(name+".json")))(s=>new String(s.readAllBytes(),java.nio.charset.StandardCharsets.UTF_8).parseJson)
    def array(name: String): Npy.ArrayData = Using.resource(zip.getInputStream(zip.getEntry(name+".npy")))(Npy.read)
    def dataset(name: String): SeriesDataset =
      val temporary=Files.createTempFile("simedape-model-data-",".npz")
      try {Using.resource(zip.getInputStream(zip.getEntry(name+".npz")))(s=>Files.copy(s,temporary,StandardCopyOption.REPLACE_EXISTING));SeriesDataset.load(temporary)}
      finally Files.deleteIfExists(temporary)
    def close(): Unit = zip.close()
  def write(file: Path,model: PatternEstimator): Unit = Using.resource(new Writer(file)) { writer=>
    writer.dataset("training",model.training)
    for (name,values)<-Vector("offset"->model.offset,"scale"->model.scale,"weights"->model.weights) do writer.array(name,Vector(values.length),values.iterator)
    val blocks=model.blocks.zipWithIndex.map { (b,index)=>
      writer.array(s"block_$index/distances",Vector(b.ids.size,b.ids.size),b.distances.iterator.flatMap(_.iterator))
      val clusters=b.clusters.zipWithIndex.map { (c,k)=>
        val prefix=s"block_$index/cluster_$k"
        writer.array(s"$prefix/center",Vector(c.center.length),c.center.iterator)
        writer.array(s"$prefix/mean",Vector(c.basis.mean.length),c.basis.mean.iterator)
        writer.array(s"$prefix/components",Vector(c.basis.components.length,c.basis.mean.length),c.basis.components.iterator.flatMap(_.iterator))
        writer.array(s"$prefix/variance_ratio",Vector(c.basis.varianceRatio.length),c.basis.varianceRatio.iterator)
        obj("members"->ints(c.members),"medoid"->JsNumber(c.medoid),"ridge"->num(c.basis.ridge))
      }
      obj("context"->JsString(b.context),"columns"->ints(b.columns),"ids"->ints(b.ids),"assignment"->ints(b.assignment.toVector),"clusters"->JsArray(clusters))
    }
    writer.json("state",obj("settings"->model.settings,"blocks"->JsArray(blocks)))
    writer.commit("simedape.pattern.v1",model.settings,model.summary)
  }
  def readPattern(file: Path): PatternEstimator = Using.resource(new Reader(file)) { reader=>
    require(reader.json("manifest").field("format").text()=="simedape.pattern.v1","Not a pattern model")
    val state=reader.json("state");val model=new PatternEstimator(state.field("settings").asJsObject)
    model.training=reader.dataset("training");model.prior=new TrainingPrior(model.training)
    model.offset=reader.array("offset").doubles;model.scale=reader.array("scale").doubles;model.weights=reader.array("weights").doubles
    model.normalized=model.training.values.map(row=>row.indices.map(j=>(row(j)-model.offset(j%model.training.p))/model.scale(j%model.training.p)).toArray)
    model.blocks=state.field("blocks").values.zipWithIndex.map { (b,index)=>
      val ids=b.field("ids").values.map(_.int());val flat=reader.array(s"block_$index/distances").doubles
      val clusters=b.field("clusters").values.zipWithIndex.map { (c,k)=>
        val prefix=s"block_$index/cluster_$k";val mean=reader.array(s"$prefix/mean").doubles;val raw=reader.array(s"$prefix/components")
        val components=Array.tabulate(raw.shape(0))(i=>Array.tabulate(mean.length)(j=>raw.number(i*mean.length+j)))
        PatternCluster(c.field("members").values.map(_.int()),c.field("medoid").int(),reader.array(s"$prefix/center").doubles,MaskedPCA(mean,components,reader.array(s"$prefix/variance_ratio").doubles,c.field("ridge").number()))
      }
      PatternBlock(b.field("context").text(),b.field("columns").values.map(_.int()),ids,b.field("assignment").values.map(_.int()).toArray,clusters,flat.grouped(ids.size).map(_.toArray).toArray)
    }
    model
  }
  def write(file: Path,model: PairedResidualEstimator): Unit = Using.resource(new Writer(file)) { writer=>
    writer.dataset("training",model.full)
    model.mappings.toVector.sortBy(_._1).zipWithIndex.foreach { (entry,index)=>
      val (_,mapping)=entry
      writer.array(s"mapping_$index/template",Vector(mapping.template.length),mapping.template.iterator)
    }
    writer.json("state",model.summary);writer.commit("simedape.paired.v1",Json.empty,model.summary)
  }
  def readPaired(file: Path): PairedResidualEstimator = Using.resource(new Reader(file)) { reader=>
    require(reader.json("manifest").field("format").text()=="simedape.paired.v1","Not a paired model")
    val state=reader.json("state");val model=new PairedResidualEstimator(state.field("ridge").number(),state.field("nonnegative").bool(),state.field("departure_tolerance_seconds").number())
    model.full=reader.dataset("training");model.prior=new TrainingPrior(model.full)
    model.context=state.field("reduction_context").asJsObject;model.engineHash=state.field("engine_jar_sha256").text();model.clock=state.field("input_clock")
    model.envelope=state.field("departure_envelope").asJsObject.fields.map((id,v)=>id->(v.values(0).number(),v.values(1).number()))
    model.sourceHash=Option(state.field("calibration_source_sha256")).filter(_!=JsNull).map(_.text())
    model.mappings=state.field("mappings").values.zipWithIndex.map { (m,index)=>
      (m.field("group").text(),m.field("label").text())->PairedMapping(reader.array(s"mapping_$index/template").doubles,m.field("gain").values.map(_.number()).toArray,m.field("paired_values").values.map(_.int()).toArray,m.field("paired_runs").values.map(_.int()).toArray)
    }.toMap
    model
  }
