package org.interscity.htc.parity

import spray.json.*
import Json.*
import java.io.{InputStream,OutputStream,DataOutputStream,BufferedOutputStream,ByteArrayInputStream,ByteArrayOutputStream}
import java.nio.{ByteBuffer,ByteOrder}
import java.nio.file.{Files,Path,StandardCopyOption}
import java.util.zip.{ZipFile,ZipOutputStream,ZipEntry}
import scala.util.Using
import scala.jdk.CollectionConverters.*
import org.interscity.htc.data.Models.WarpingStep
import org.tslearn.decomposition.PCAModel

final case class PCACluster(model: PCAModel,members: Array[Int],trainWeights: Array[Array[Double]],spWeights: Array[Double])
final case class TrainedModel(config: JsObject,labels: Vector[String],clusters: Clusters,means: Array[Double],stds: Array[Double],
  roads: Map[String,Road],vehicles: Vector[Vector[String]],pca: Map[Int,PCACluster]=Map.empty,dtw: Option[Path]=None,report: Option[ClusterReport]=None,streetVehicles: Map[String,Vector[Vector[String]]]=Map.empty,storedSimulationVehicles: Option[JsObject]=None):
  lazy val index: Map[String,Int] = labels.zipWithIndex.toMap
  def size: Int = means.length
  def length: Int = clusters.centroids(0).length
  def simulationPointVehicles: JsObject = storedSimulationVehicles.getOrElse(JsObject(clusters.simulationPoints.toVector.map { i=>
    val link=labels(i).takeWhile(_!='-')
    val members=streetVehicles.get(link).map(_.flatten).getOrElse(labels.indices.filter(j=>labels(j).takeWhile(_!='-')==link).toVector.flatMap(vehicles))
    labels(i)->strings(members.distinct.sorted)
  }.toMap))
  def context: JsObject =
    val lengths=roads.values.toVector.map(_.length)
    val cuts=if lengths.size<8 then Vector.empty else Vector(.25,.5,.75).map(q=>math.rint(Metrics.quantile(lengths,q)*10)/10).distinct.sorted
    obj("cluster_of"->JsObject(labels.zip(clusters.labels).map((l,k)=>l->JsNumber(k)).toMap),
      "length_of"->JsObject(roads.map((id,r)=>id->num(r.length))),"length_cuts"->arr(cuts))

object Npy:
  final case class ArrayData(dtype: String,shape: Vector[Int],data: Array[Byte]):
    private val buffer=ByteBuffer.wrap(data).order(if dtype.startsWith(">") then ByteOrder.BIG_ENDIAN else ByteOrder.LITTLE_ENDIAN)
    val width: Int = dtype.drop(2).toInt
    private val elementBytes=width*(if dtype.charAt(1)=='U' then 4 else 1)
    def size: Int = if elementBytes==0 then 0 else data.length/elementBytes
    def text: String =
      require(dtype.charAt(1)=='U' || dtype.charAt(1)=='S',"Expected a primitive string array")
      if dtype.charAt(1)=='S' then new String(data.takeWhile(_!=0),java.nio.charset.StandardCharsets.UTF_8)
      else
        val chars=(0 until data.length/4).map(i=>buffer.getInt(i*4)).takeWhile(_!=0).toArray
        new String(chars,0,chars.length)
    def number(i: Int): Double = dtype.charAt(1) match
      case 'f' => if width==8 then buffer.getDouble(i*8) else buffer.getFloat(i*4).toDouble
      case _ => integer(i).toDouble
    def integer(i: Int): Long = (dtype.charAt(1),width) match
      case ('i',8) => buffer.getLong(i*8)
      case ('i',4) => buffer.getInt(i*4).toLong
      case ('i',2) => buffer.getShort(i*2).toLong
      case ('i',1) => data(i).toLong
      case ('u',1) | ('b',1) => (data(i)&255).toLong
      case ('u',2) => (buffer.getShort(i*2)&65535).toLong
      case ('u',4) => java.lang.Integer.toUnsignedLong(buffer.getInt(i*4))
      case _ => throw IllegalArgumentException(s"Unsupported integer dtype $dtype")
    def doubles: Array[Double] = Array.tabulate(size)(number)
  def header(dtype: String,shape: Seq[Int]): Array[Byte] =
    val tuple=shape.mkString("(",", ",if shape.size==1 then ",)" else ")")
    val raw=s"{'descr': '$dtype', 'fortran_order': False, 'shape': $tuple, }"
    val padded=raw+(" "*((64-(10+raw.length+1)%64)%64))+"\n"
    val bytes=padded.getBytes(java.nio.charset.StandardCharsets.US_ASCII)
    require(bytes.length<65536,"NPY header too long")
    Array(0x93.toByte,'N'.toByte,'U'.toByte,'M'.toByte,'P'.toByte,'Y'.toByte,1.toByte,0.toByte,(bytes.length&255).toByte,(bytes.length>>8).toByte)++bytes
  def read(stream: InputStream): ArrayData =
    val magic=stream.readNBytes(8)
    require(magic.length==8 && magic(0)==0x93.toByte && new String(magic.slice(1,6),java.nio.charset.StandardCharsets.US_ASCII)=="NUMPY","Invalid NPY member")
    val major=magic(6).toInt
    require(major>=1 && major<=3,"Unsupported NPY version")
    val lengthBytes=stream.readNBytes(if major==1 then 2 else 4)
    val length=if major==1 then (lengthBytes(0)&255)+((lengthBytes(1)&255)<<8) else ByteBuffer.wrap(lengthBytes).order(ByteOrder.LITTLE_ENDIAN).getInt()
    require(length>0 && length<1048576,"Invalid NPY header size")
    val text=new String(stream.readNBytes(length),java.nio.charset.StandardCharsets.UTF_8)
    val dtype="'descr':\\s*'([^']+)'".r.findFirstMatchIn(text).map(_.group(1)).getOrElse(throw IllegalArgumentException("Missing dtype"))
    require(!text.contains("'fortran_order': True"),"Fortran-order NPY is not supported")
    require(dtype.matches("[<>=|](f[48]|[iu][1248]|b1|[US][0-9]+)"),"Unsupported or object NPY dtype")
    val shape="'shape':\\s*\\(([^)]*)\\)".r.findFirstMatchIn(text).get.group(1).split(',').map(_.trim).filter(_.nonEmpty).map(_.toInt).toVector
    require(shape.forall(_>=0),"Invalid NPY shape")
    val size=shape.foldLeft(1L)((a,b)=>Math.multiplyExact(a,b.toLong))*dtype.drop(2).toInt*(if dtype.charAt(1)=='U' then 4 else 1)
    require(size<=Int.MaxValue,"NPY member exceeds JVM array size")
    val data=stream.readNBytes(size.toInt);require(data.length==size,"Truncated NPY member")
    ArrayData(dtype,shape,data)
  private val scratch = new ThreadLocal[Array[Byte]]:
    override def initialValue(): Array[Byte] = new Array[Byte](8)
  private def little(output: OutputStream,bits: Long,width: Int): Unit =
    val bytes=scratch.get()
    var i=0
    while i<width do
      bytes(i)=((bits >>> (8*i))&255).toByte
      i+=1
    output.write(bytes,0,width)
  def writeDouble(output: OutputStream,value: Double,precision: String="float64"): Unit =
    if precision=="float32" then little(output,java.lang.Float.floatToIntBits(value.toFloat).toLong,4)
    else little(output,java.lang.Double.doubleToLongBits(value),8)
  def writeLong(output: OutputStream,value: Long,width: Int): Unit = little(output,value,width)

final class ClusterPaths(private val arrays: Map[String,Npy.ArrayData]):
  val size: Int = arrays("offsets").size-1
  def stepCount: Long = arrays("offsets").integer(size)
  def path(index: Int): List[WarpingStep] =
    require(index>=0 && index<size,"Warping-path index out of range")
    var i=arrays("i0").integer(index).toInt;var j=arrays("j0").integer(index).toInt
    val start=arrays("offsets").integer(index).toInt;val end=arrays("offsets").integer(index+1).toInt
    (start until end).iterator.map { pos =>
      i+=arrays("di").integer(pos).toInt;j+=arrays("dj").integer(pos).toInt
      WarpingStep(i,j,arrays("d").number(pos),arrays("fun").integer(pos)!=0)
    }.toList
  def finalCosts: Vector[Double] = (0 until size).flatMap { i=>
    val start=arrays("offsets").integer(i).toInt;val end=arrays("offsets").integer(i+1).toInt
    if end>start then Some(arrays("d").number(end-1)) else None
  }.toVector
  def translate(index: Int,source: Array[Double]): Array[Double] =
    val out=new Array[Double](source.length)
    var i=arrays("i0").integer(index).toInt;var j=arrays("j0").integer(index).toInt
    val start=arrays("offsets").integer(index).toInt;val end=arrays("offsets").integer(index+1).toInt
    var previous=0.0
    for pos<-start until end do
      i+=arrays("di").integer(pos).toInt;j+=arrays("dj").integer(pos).toInt
      val cost=arrays("d").number(pos)
      val delta=if pos==start then cost else math.sqrt(math.max(0,cost*cost-previous*previous))
      out(j)=source(i)+(if arrays("fun").integer(pos)!=0 then -delta else delta)
      previous=cost
    out

object ModelIO:
  val format="simedape.model.v1"
  val pathKeys=Vector("offsets","i0","j0","di","dj","d","fun")
  def paths(source: Path,cluster: Int,fields: Vector[String]=pathKeys): ClusterPaths =
    val isParts=Files.isDirectory(source)
    val file=if isParts then source.resolve(s"cluster_$cluster.wpz") else source
    val prefix=if isParts then "" else s"dtw/cluster_$cluster/"
    Using.resource(new ZipFile(file.toFile)) { zip =>
      new ClusterPaths(fields.map { key =>
        val entry=Option(zip.getEntry(s"$prefix$key.npy")).getOrElse(throw IllegalArgumentException(s"Model has no DTW cluster $cluster"))
        key->Using.resource(zip.getInputStream(entry))(Npy.read)
      }.toMap)
    }
  /** Disk-backed field streams keep a city-sized DTW training run out of object maps. */
  def writePaths(folder: Path,cluster: Int,count: Int,paths: Iterator[List[WarpingStep]],precision: String="float64"): Path =
    require(Set("float32","float64").contains(precision),"Invalid precision")
    Files.createDirectories(folder)
    val scratch=Files.createTempDirectory(folder,".paths-")
    try
      val outputs=pathKeys.map(k=>k->new BufferedOutputStream(Files.newOutputStream(scratch.resolve(k)),65536)).toMap
      var total=0L;var written=0
      try
        Npy.writeLong(outputs("offsets"),0,8)
        paths.foreach { path =>
          Npy.writeLong(outputs("i0"),path.headOption.map(_.i).getOrElse(0),4)
          Npy.writeLong(outputs("j0"),path.headOption.map(_.j).getOrElse(0),4)
          var pi=path.headOption.map(_.i).getOrElse(0);var pj=path.headOption.map(_.j).getOrElse(0)
          path.foreach { step =>
            Npy.writeLong(outputs("di"),step.i-pi,1);Npy.writeLong(outputs("dj"),step.j-pj,1)
            Npy.writeDouble(outputs("d"),step.cost,precision);Npy.writeLong(outputs("fun"),if step.descending then 1 else 0,1)
            pi=step.i;pj=step.j;total+=1
          }
          written+=1;Npy.writeLong(outputs("offsets"),total,8)
        }
        require(written==count,"Warping-path member count mismatch")
      finally outputs.values.foreach(_.close())
      val destination=folder.resolve(s"cluster_$cluster.wpz");val temporary=scratch.resolve("cluster.wpz")
      Using.resource(new ZipOutputStream(new BufferedOutputStream(Files.newOutputStream(temporary)))) { zip =>
        pathKeys.foreach { key =>
          val dtype=key match
            case "offsets" => "<i8"
            case "i0" | "j0" => "<i4"
            case "d" => if precision=="float32" then "<f4" else "<f8"
            case _ => "|u1"
          val length=key match
            case "offsets" => count+1
            case "i0" | "j0" => count
            case _ => require(total<=Int.MaxValue,"A cluster has too many path steps");total.toInt
          zip.putNextEntry(new ZipEntry(s"$key.npy"));zip.write(Npy.header(dtype,Seq(length)))
          Files.copy(scratch.resolve(key),zip);zip.closeEntry()
        }
      }
      Files.move(temporary,destination,StandardCopyOption.REPLACE_EXISTING)

      destination
    finally
      Using.resource(Files.list(scratch))(_.iterator().asScala.foreach(Files.deleteIfExists(_)))
      Files.deleteIfExists(scratch)

  def write(file: Path,model: TrainedModel,includeVehicles: Boolean=true): JsObject =
    Files.createDirectories(file.toAbsolutePath.getParent)
    val temporary=Files.createTempFile(file.toAbsolutePath.getParent,".model-",".tmp")
    val members=scala.collection.mutable.LinkedHashMap.empty[String,JsValue]
    var manifest: JsObject=empty
    try
      Using.resource(new ZipOutputStream(new BufferedOutputStream(Files.newOutputStream(temporary)))) { zip =>
        def entry(name: String,info: JsObject)(write: OutputStream=>Unit): Unit =
          val item=new ZipEntry(name);zip.putNextEntry(item);write(zip);zip.closeEntry()
          members(name)=JsObject(info.fields++Map("raw_bytes"->JsNumber(item.getSize),"stored_bytes"->JsNumber(item.getCompressedSize)))
        def json(name: String,value: JsValue): Unit = entry(s"$name.json",obj("kind"->JsString("json")))(_.write(value.compactPrint.getBytes(java.nio.charset.StandardCharsets.UTF_8)))
        def doubles(name: String,shape: Seq[Int],values: Iterator[Double]): Unit =
          entry(s"$name.npy",obj("kind"->JsString("array"),"dtype"->JsString("float64"),"shape"->ints(shape))) { out =>
            out.write(Npy.header("<f8",shape));values.foreach(Npy.writeDouble(out,_))
          }
        def integers(name: String,values: Iterable[Int]): Unit =
          entry(s"$name.npy",obj("kind"->JsString("array"),"dtype"->JsString("int32"),"shape"->ints(Seq(values.size)))) { out =>
            out.write(Npy.header("<i4",Seq(values.size)));values.foreach(v=>Npy.writeLong(out,v,4))
          }
        integers("clustering/labels",model.clusters.labels.toVector)
        doubles("clustering/centroids",Seq(model.clusters.count,model.length),model.clusters.centroids.iterator.flatMap(_.iterator))
        doubles("clustering/mean",Seq(model.size),model.means.iterator);doubles("clustering/std",Seq(model.size),model.stds.iterator)
        json("clustering/meta",obj("number_of_clusters"->JsNumber(model.clusters.count),"mean_shape"->ints(Seq(model.size,1,1))))
        json("json/time_series_labels",strings(model.labels));json("json/simulation_points_indexs",ints(model.clusters.simulationPoints.toVector))
        json("json/simulation_points_labels",strings(model.clusters.simulationPoints.toVector.map(model.labels)))
        json("json/simulation_points_vehicles",model.simulationPointVehicles)
        json("json/clustering_vehicles",JsObject((0 until model.clusters.count).map(k=>k.toString->strings(model.clusters.members(k).toVector.flatMap(model.vehicles).distinct.sorted)).toMap))
        if includeVehicles then
          json("json/street_series_vehicles",JsArray(model.vehicles.map(strings)))
        json("json/street_info",JsObject(model.roads.map((id,r)=>id->r.json)))
        model.dtw.foreach { source =>
          if Files.isDirectory(source) then
            for k<-0 until model.clusters.count do
              Using.resource(new ZipFile(source.resolve(s"cluster_$k.wpz").toFile)) { part =>
                pathKeys.foreach { key => val name=s"$key.npy";val item=part.getEntry(name)
                  entry(s"dtw/cluster_$k/$name",obj("kind"->JsString("array"))) { out => Using.resource(part.getInputStream(item))(_.transferTo(out));() }
                }
              }
          else Using.resource(new ZipFile(source.toFile)) { previous =>
            previous.entries().asScala.filter(_.getName.startsWith("dtw/")).foreach { item =>
              entry(item.getName,obj("kind"->JsString("array"))) { out=>Using.resource(previous.getInputStream(item))(_.transferTo(out));() }
            }
          }
        }
        if model.pca.nonEmpty then
          json("pca/meta",obj("clusters"->ints(model.pca.keys.toVector.sorted)))
          model.pca.toVector.sortBy(_._1).foreach { (k,cluster) =>
            val p=cluster.model;val prefix=s"pca/cluster_$k"
            doubles(s"$prefix/components",Seq(p.components().length,model.length),p.components().iterator.flatMap(_.iterator))
            doubles(s"$prefix/mean",Seq(model.length),p.mean().iterator)
            doubles(s"$prefix/explained_variance",Seq(p.eigenvalues().length),p.eigenvalues().iterator)
            doubles(s"$prefix/explained_variance_ratio",Seq(p.varianceRatio().length),p.varianceRatio().iterator)
            doubles(s"$prefix/train_weights",Seq(cluster.members.length,p.components().length),cluster.trainWeights.iterator.flatMap(_.iterator))
            json(s"$prefix/info",obj("size"->JsNumber(p.size()),"n_components"->JsNumber(p.components().length),
              "explained_variance_pct"->num(p.varianceRatio().sum*100),"labels"->strings(cluster.members.map(model.labels).toVector),"sp_weights_train"->arr(cluster.spWeights.toVector)))
          }
        model.report.foreach { report=>
          json("report/summary",report.summary)
          report.arrays.toVector.sortBy(_._1).foreach { (name,a)=>
            val dtype=a.dtype match {case "float64"=>"<f8";case "int64"=>"<i8";case _=>"<f4"}
            entry(s"report/$name.npy",obj("kind"->JsString("array"),"dtype"->JsString(a.dtype),"shape"->ints(a.shape))) { out=>
              out.write(Npy.header(dtype,a.shape))
              a.values.foreach(v=>if a.dtype=="int64" then Npy.writeLong(out,v.toLong,8) else Npy.writeDouble(out,v,a.dtype))
            }
          }
        }
        val dtwInfo=model.dtw.map { source=>
          var paths=0L;var length=0L
          for k<-0 until model.clusters.count do
            val p=ModelIO.paths(source,k,Vector("offsets"));paths+=p.size;length+=p.stepCount
          obj("clusters"->JsNumber(model.clusters.count),"paths"->JsNumber(paths),"mean_path_length"->num(if paths==0 then 0 else math.rint(length*100.0/paths)/100))
        }.getOrElse(JsNull)
        val pcaInfo=if model.pca.isEmpty then JsNull else obj("clusters"->JsNumber(model.pca.size),"n_components"->JsNumber(model.pca.values.map(_.model.components().length).max),
          "explained_variance_pct_mean"->num(math.rint(model.pca.values.map(c=>c.model.varianceRatio().sum*100).sum/model.pca.size*100)/100))
        val summary=obj("n_series"->JsNumber(model.size),"n_clusters"->JsNumber(model.clusters.count),"series_length"->JsNumber(model.length),
          "simulation_points"->JsNumber(model.clusters.simulationPoints.length),"cluster_sizes"->ints((0 until model.clusters.count).map(model.clusters.members(_).length)),
          "dtw"->dtwInfo,"pca"->pcaInfo,"methods"->strings((if model.dtw.nonEmpty then Vector("dtw") else Vector.empty)++(if model.pca.nonEmpty then Vector("pca") else Vector.empty)))
        manifest=obj("format"->JsString(format),"created"->JsString(java.time.Instant.now().toString),"meta"->model.config,"summary"->summary,
          "members"->JsObject(members.toMap),"total_raw_bytes"->JsNumber(members.values.map(_.field("raw_bytes").number()).sum),
          "total_stored_bytes"->JsNumber(members.values.map(_.field("stored_bytes").number()).sum))
        zip.putNextEntry(new ZipEntry("manifest.json"));zip.write(manifest.prettyPrint.getBytes(java.nio.charset.StandardCharsets.UTF_8));zip.closeEntry()
      }
      Files.move(temporary,file.toAbsolutePath,StandardCopyOption.REPLACE_EXISTING)
      JsObject(manifest.fields+("file_bytes"->JsNumber(Files.size(file))))
    finally Files.deleteIfExists(temporary)

  def read(file: Path,roads: Map[String,Road]=Map.empty): TrainedModel = Using.resource(new ZipFile(file.toFile)) { zip =>
    def json(name: String): JsValue = Using.resource(zip.getInputStream(Option(zip.getEntry(s"$name.json")).getOrElse(throw IllegalArgumentException(s"Missing model member $name"))))(s=>new String(s.readAllBytes(),java.nio.charset.StandardCharsets.UTF_8).parseJson)
    def array(name: String): Npy.ArrayData = Using.resource(zip.getInputStream(Option(zip.getEntry(s"$name.npy")).getOrElse(throw IllegalArgumentException(s"Missing array $name"))))(Npy.read)
    val manifest=json("manifest");require(manifest.field("format").text()==format,"Unsupported model format")
    val meta=json("clustering/meta");val count=meta.field("number_of_clusters").int()
    val labels=json("json/time_series_labels").names
    val group=array("clustering/labels");require(group.size==labels.size,"Clustering labels do not match series")
    val rawCentroids=array("clustering/centroids");val length=rawCentroids.shape(1)
    val centroids=Array.tabulate(count)(k=>Array.tabulate(length)(j=>rawCentroids.number(k*length+j)))
    val clusters=Clusters(Array.tabulate(group.size)(i=>group.integer(i).toInt),centroids,json("json/simulation_points_indexs").values.map(_.int()).toArray)
    val roadMap=if zip.getEntry("json/street_info.json")==null then roads else json("json/street_info").asJsObject.fields.map { (id,v)=>val xs=v.values;id->Road(id,xs(0).number(),xs(1).number(),xs(2).number()) }
    val vehicles=if zip.getEntry("json/street_series_vehicles.json")==null then Vector.fill(labels.size)(Vector.empty[String]) else json("json/street_series_vehicles").values.map(_.names)
    val pca=if zip.getEntry("pca/meta.json")==null then Map.empty[Int,PCACluster] else json("pca/meta").field("clusters").values.map { key =>
      val k=key.int();val info=json(s"pca/cluster_$k/info");val components=array(s"pca/cluster_$k/components");val rank=components.shape(0)
      val p=PCAModel(array(s"pca/cluster_$k/mean").doubles,Array.tabulate(rank)(i=>Array.tabulate(length)(j=>components.number(i*length+j))),
        array(s"pca/cluster_$k/explained_variance").doubles,array(s"pca/cluster_$k/explained_variance_ratio").doubles,info.field("size").int())
      val members=clusters.members(k);val weights=array(s"pca/cluster_$k/train_weights")
      k->PCACluster(p,members,Array.tabulate(members.length)(i=>Array.tabulate(rank)(j=>weights.number(i*rank+j))),info.field("sp_weights_train").values.map(_.number()).toArray)
    }.toMap
    val model=TrainedModel(manifest.field("meta",empty).asJsObject,labels,clusters,array("clustering/mean").doubles,array("clustering/std").doubles,
      roadMap,vehicles,pca,if zip.getEntry("dtw/cluster_0/offsets.npy")!=null then Some(file.toAbsolutePath) else None,
      if zip.getEntry("report/summary.json")==null then None else Some(ClusterReport(json("report/summary").asJsObject,
        zip.entries().asScala.filter(e=>e.getName.startsWith("report/") && e.getName.endsWith(".npy")).map { e=>
          val a=Using.resource(zip.getInputStream(e))(Npy.read)
          e.getName.stripPrefix("report/").stripSuffix(".npy")->ReportArray(a.shape,a.doubles,if a.dtype.charAt(1)=='i' then "int64" else if a.width==8 then "float64" else "float32")
        }.toMap)))
    model.copy(storedSimulationVehicles=if zip.getEntry("json/simulation_points_vehicles.json")==null then None else Some(json("json/simulation_points_vehicles").asJsObject))
  }
