package org.interscity.htc.parity
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import spray.json.*
import Json.*

class TrainingParitySpec extends AnyFlatSpec with Matchers:
  def close(got: JsValue,expected: JsValue,path: String=""): Unit = (got,expected) match
    case (JsNumber(a),JsNumber(b))=>withClue(path+": "){a.toDouble shouldBe b.toDouble +- (2e-6*math.max(1,math.abs(b.toDouble)))}
    case (a: JsObject,b: JsObject)=>
      withClue(path){a.fields.keySet shouldBe b.fields.keySet};b.fields.foreach((k,v)=>close(a.fields(k),v,s"$path/$k"))
    case (JsArray(a),JsArray(b))=>a.size shouldBe b.size;a.zip(b).zipWithIndex.foreach((v,i)=>close(v._1,v._2,s"$path/$i"))
    case _=>withClue(path+": "){got shouldBe expected}
  "Native training" should "match Python K-Shape, raw-count PCA and model clustering diagnostics" in {
    val source=scala.io.Source.fromResource("parity/estimation-reference.json").mkString.parseJson
    val expected=scala.io.Source.fromResource("parity/training.json").mkString.parseJson
    val rows=source.field("raw").values.zip(source.field("labels").names).map((r,l)=>Series(l.takeWhile(_!='-'),0,3600,r.values.map(_.number()).toArray,Vector.empty))
    val data=Sources.prepare(Dataset(rows,Map.empty,Map.empty,"fixture"),12)
    val cfg=RunConfig(obj("clustering"->obj("n_clusters"->JsNumber(2),"max_iter"->JsNumber(8),"random_state"->JsNumber(10))))
    val model=Training.cluster(data,cfg)
    close(ints(model.clusters.labels.toVector),expected.field("clusters"),"clusters")
    close(JsArray(model.clusters.centroids.toVector.map(v=>arr(v.toVector))),expected.field("centroids"),"centroids")
    val pca=Training.pca(model,data,2)
    expected.field("pca").asJsObject.fields.foreach { (key,reference)=>
      val p=pca.pca(key.toInt).model
      close(arr(p.eigenvalues().toVector),reference.field("variance"),s"pca/$key/variance")
      close(arr(p.varianceRatio().toVector),reference.field("ratio"),s"pca/$key/ratio")
      // Projection matrices compare the subspaces independent of SVD sign choice.
      val c=p.components();val e=reference.field("components").values.map(_.values.map(_.number()))
      for a<-0 until 12;b<-0 until 12 do
        c.map(row=>row(a)*row(b)).sum shouldBe e.map(row=>row(a)*row(b)).sum +- 1e-6
    }
    close(model.report.get.summary,expected.field("report"),"report")
  }
  it should "retain occupied clusters when fewer distinct shapes exist than requested" in {
    val rows=Vector.tabulate(4)(i=>Series(i.toString,0,60,Array(1.0,2,3,2,1),Vector.empty))
    val data=Sources.prepare(Dataset(rows,Map.empty,Map.empty,"degenerate"),5)
    val m=Training.cluster(data,RunConfig(obj("clustering"->obj("n_clusters"->JsNumber(3)))))
    m.clusters.labels.distinct.length shouldBe m.clusters.count
    m.clusters.count shouldBe 1
  }
