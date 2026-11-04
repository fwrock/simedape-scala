package org.interscity.htc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import parity.*
import parity.Json.*
import spray.json.*
import java.nio.file.{Files,Path}
import scala.util.Using

class ModelVariantParitySpec extends AnyFlatSpec with Matchers:
  lazy val resource=Path.of(getClass.getResource("/parity/python-model.simedape").toURI)
  lazy val model=ModelIO.read(resource)
  lazy val reference=Using.resource(getClass.getResourceAsStream("/parity/estimation-reference.json"))(s=>new String(s.readAllBytes(),java.nio.charset.StandardCharsets.UTF_8).parseJson)
  def close(got: JsValue,expected: JsValue,path: String=""): Unit = (got,expected) match
    case (JsNumber(a),JsNumber(b)) => withClue(path+": "){ a.toDouble shouldBe b.toDouble +- (2e-6*math.max(1,math.abs(b.toDouble))) }
    case (a: JsObject,b: JsObject) =>
      a.fields.keySet shouldBe b.fields.keySet
      b.fields.foreach((k,v)=>close(a.fields(k),v,s"$path/$k"))
    case (JsArray(a),JsArray(b)) => a.size shouldBe b.size;a.zip(b).zipWithIndex.foreach((v,i)=>close(v._1,v._2,s"$path/$i"))
    case _ => withClue(path+": "){got shouldBe expected}
  "Model bundle" should "read Python typed arrays and round-trip the portable format" in {
    model.size shouldBe 6
    model.pca.size shouldBe 2
    model.clusters.count shouldBe 2
    val destination=Path.of("target/parity/scala-model.simedape")
    ModelIO.write(destination,model)
    val reread=ModelIO.read(destination)
    reread.labels shouldBe model.labels
    reread.means.toVector shouldBe model.means.toVector
    for k<-0 until model.clusters.count do
      val before=ModelIO.paths(resource,k);val after=ModelIO.paths(destination,k)
      before.size shouldBe after.size
      for i<-0 until before.size do before.path(i) shouldBe after.path(i)
  }
  "Estimation catalogue" should "execute all 100 Python variants against the same trained model" in {
    val partial=reference.field("partial").values.zip(reference.field("partial_labels").names).map { (row,label) =>
      Series(label.takeWhile(_!='-'),0,3600,row.values.map(_.number()).toArray,Vector.empty)
    }
    val data=Dataset(partial,model.roads,Map.empty,"fixture")
    Variants.catalog.size shouldBe 100
    val preparations=scala.collection.mutable.Map.empty[(Int,String),Prepared]
    Variants.catalog.foreach { variant =>
      val key=Estimator.preparationKey(variant,model,data)
      val prepared=preparations.getOrElseUpdate(key,Sources.prepare(data,key._1,key._2))
      val result=Estimator.metrics(model,prepared,variant,reference.field("percentage").number())
      close(result,reference.field("expected").field(variant.key),variant.key)
    }
  }

  "Batched DTW" should "preserve every Python normalization and interpolation result while sharing paths" in {
    val partial=reference.field("partial").values.zip(reference.field("partial_labels").names).map((row,label)=>Series(label.takeWhile(_!='-'),0,3600,row.values.map(_.number()).toArray,Vector.empty))
    val data=Dataset(partial,model.roads,Map.empty,"fixture")
    Variants.catalog.filter(_.family=="dtw").groupBy(v=>Estimator.preparationKey(v,model,data)).foreach { (key,vs)=>
      val prep=Sources.prepare(data,key._1,key._2)
      val output=scala.collection.mutable.Map.from(vs.map(v=>v.key->Vector.empty[(String,Array[Double])]))
      val missing=Estimator.dtwBatch(model,prep,vs,reference.field("percentage").number())((v,label,values)=>output(v.key)=output(v.key):+(label->values))
      vs.foreach(v=>close(Metrics.result(output(v.key),model.roads,missing.simulationPoints,missing.series),reference.field("expected").field(v.key),"batch/"+v.key))
    }
  }
