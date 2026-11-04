package org.interscity.htc.parity.experiments
import org.interscity.htc.parity.*
import Json.*
import spray.json.*
import java.nio.file.{Files,Path}
import java.util.zip.{ZipFile,ZipOutputStream,ZipEntry}
import java.io.BufferedOutputStream
import scala.util.Using
import scala.jdk.CollectionConverters.*

/** Physical time and missingness are explicit; withheld values never enter model fitting. */
final case class SeriesDataset(values: Array[Array[Double]],labels: Vector[String],variables: Vector[String],times: Array[Double],runs: Vector[String],groups: Vector[String],units: Vector[Vector[String]],metadata: JsObject=Json.empty):
  val n: Int=values.length
  val t: Int=times.length
  val p: Int=variables.length
  require(n>0 && t>0 && p>0 && values.forall(_.length==t*p),"Dataset needs matching nonempty (series,time,variable) axes")
  require(labels.size==n && runs.size==n && groups.size==n && units.size==n,"Row metadata does not match values")
  require(variables.distinct.size==p && runs.zip(labels).distinct.size==n,"Variables and (run,label) identities must be unique")
  require(times.forall(_.isFinite) && times.sliding(2).forall(xs=>xs.length<2 || xs(1)>xs(0)),"Time axis must be finite and strictly increasing")
  require(values.forall(_.forall(v=>v.isNaN || v.isFinite)),"Observed values must be finite")
  def subset(indices: Vector[Int]): SeriesDataset =
    val chosenRuns=indices.map(runs).toSet
    val provenance=metadata.field("run_provenance")
    val meta=if provenance==JsNull then metadata else JsObject(metadata.fields+("run_provenance"->JsObject(provenance.asJsObject.fields.filter((r,_)=>chosenRuns(r)))))
    copy(values=indices.map(values).toArray,labels=indices.map(labels),runs=indices.map(runs),groups=indices.map(groups),units=indices.map(units),metadata=meta)
  def forRuns(selected: Vector[String]): SeriesDataset = subset(runs.indices.filter(i=>selected.contains(runs(i))).toVector)
  def observed: Long = values.iterator.map(_.count(_.isFinite).toLong).sum
  def save(file: Path): Unit =
    val info=obj("schema"->JsString("simedape.series.v1"),"labels"->strings(labels),"variables"->strings(variables),"runs"->strings(runs),"groups"->strings(groups),"units"->JsArray(units.map(strings)),"metadata"->metadata)
    Files.createDirectories(file.toAbsolutePath.getParent)
    Using.resource(new ZipOutputStream(new BufferedOutputStream(Files.newOutputStream(file)))) { zip=>
      SeriesDataset.doubles(zip,"values",Vector(n,t,p),values.iterator.flatMap(_.iterator))
      zip.putNextEntry(new ZipEntry("observed.npy"));zip.write(Npy.header("|b1",Vector(n,t,p)));values.foreach(_.foreach(v=>zip.write(if v.isFinite then 1 else 0)));zip.closeEntry()
      SeriesDataset.doubles(zip,"times",Vector(t),times.iterator)
      val codepoints=info.compactPrint.codePoints().toArray
      zip.putNextEntry(new ZipEntry("info.npy"));zip.write(Npy.header(s"<U${codepoints.length}",Vector.empty));codepoints.foreach(c=>Npy.writeLong(zip,c,4));zip.closeEntry()
    }

object SeriesDataset:
  private[experiments] def doubles(zip: ZipOutputStream,name: String,shape: Vector[Int],values: Iterator[Double]): Unit =
    zip.putNextEntry(new ZipEntry(s"$name.npy"));zip.write(Npy.header("<f8",shape));values.foreach(Npy.writeDouble(zip,_));zip.closeEntry()
  def load(file: Path): SeriesDataset = Using.resource(new ZipFile(file.toFile)) { zip=>
    def array(name: String): Npy.ArrayData =
      val entry=Option(zip.getEntry(s"$name.npy")).getOrElse(throw IllegalArgumentException(s"Missing dataset array: $name"))
      Using.resource(zip.getInputStream(entry))(Npy.read)
    val info=array("info").text.parseJson;require(info.field("schema").text()=="simedape.series.v1","Unsupported series schema")
    val raw=array("values");val mask=array("observed")
    require(raw.shape.length==3 && mask.shape==raw.shape,"Dataset value/mask shapes differ")
    val n=raw.shape(0);val width=raw.shape(1)*raw.shape(2)
    val values=Array.tabulate(n)(i=>Array.tabulate(width)(j=>if mask.integer(i*width+j)!=0 then {val v=raw.number(i*width+j);require(v.isFinite,"Observed values must be finite");v} else Double.NaN))
    val labels=info.field("labels").names
    SeriesDataset(values,labels,info.field("variables").names,array("times").doubles,info.field("runs").names,
      info.field("groups",strings(Vector.fill(n)("all"))).names,info.field("units",JsArray(labels.map(l=>strings(Vector(l))))).values.map(_.names),info.field("metadata",Json.empty).asJsObject)
  }
  def combine(datasets: Vector[SeriesDataset]): SeriesDataset =
    require(datasets.nonEmpty,"No datasets to combine")
    val first=datasets.head
    val global=JsObject(first.metadata.fields-"run_provenance")
    datasets.foreach(d=>require(d.variables==first.variables && d.times.toVector==first.times.toVector && JsObject(d.metadata.fields-"run_provenance")==global,"Dataset observation semantics differ"))
    val provenance=scala.collection.mutable.Map.empty[String,JsValue]
    datasets.foreach(d=>d.metadata.field("run_provenance",Json.empty).asJsObject.fields.filter((r,_)=>d.runs.contains(r)).foreach { (r,value)=>
      require(!provenance.contains(r) || provenance(r)==value,"Conflicting provenance for one run identity");provenance(r)=value
    })
    first.copy(values=datasets.flatMap(_.values).toArray,labels=datasets.flatMap(_.labels),runs=datasets.flatMap(_.runs),groups=datasets.flatMap(_.groups),units=datasets.flatMap(_.units),
      metadata=if provenance.isEmpty then global else JsObject(global.fields+("run_provenance"->JsObject(provenance.toMap))))
  def fingerprint(file: Path): String =
    val digest=java.security.MessageDigest.getInstance("SHA-256")
    val files=if Files.isDirectory(file) then Using.resource(Files.walk(file))(_.iterator().asScala.filter(p=>Files.isRegularFile(p) && p.toString.endsWith(".parquet")).toVector.sortBy(_.toString)) else Vector(file)
    require(files.nonEmpty,s"No input files to fingerprint: $file")
    files.foreach { path=>
      if Files.isDirectory(file) then {digest.update(file.relativize(path).toString.getBytes(java.nio.charset.StandardCharsets.UTF_8));digest.update(0.toByte)}
      Using.resource(Files.newInputStream(path)) { input=>val buffer=new Array[Byte](1048576);var n=input.read(buffer)
        while n>=0 do
          if n>0 then digest.update(buffer,0,n)
          n=input.read(buffer)
      }
    }
    java.util.HexFormat.of().formatHex(digest.digest())
