package org.interscity.htc.parity
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.interscity.htc.parity.experiments.*
import spray.json.*
import Json.*
import java.nio.file.{Files,Path}

class MultivariableParitySpec extends AnyFlatSpec with Matchers:
  def resource(name: String): Path = Path.of(getClass.getResource(s"/parity/$name").toURI)
  def close(got: JsValue,expected: JsValue,path: String=""): Unit = (got,expected) match
    case (JsNumber(a),JsNumber(b))=>withClue(path+": "){a.toDouble shouldBe b.toDouble +- 2e-6*math.max(1,math.abs(b.toDouble))}
    case (a: JsObject,b: JsObject)=>withClue(path){a.fields.keySet shouldBe b.fields.keySet};b.fields.foreach((k,v)=>close(a.fields(k),v,s"$path/$k"))
    case (JsArray(a),JsArray(b))=>withClue(path){a.size shouldBe b.size};a.zip(b).zipWithIndex.foreach((v,i)=>close(v._1,v._2,s"$path/$i"))
    case _=>withClue(path+": "){got shouldBe expected}
  "Multivariable estimators" should "match Python joint, separate and grouped held-out predictions for all four model families" in {
    val train=SeriesDataset.load(resource("multivariable-training.npz"));val query=SeriesDataset.load(resource("multivariable-query.npz"));val truth=SeriesDataset.load(resource("multivariable-truth.npz"))
    val fixture=Json.read(resource("multivariable.json"))
    fixture.field("cases").values.foreach { c=>
      val model=new PatternEstimator(c.field("config").asJsObject).fit(train)
      val predicted=model.predict(query)
      val json=JsArray(predicted.toVector.map(row=>JsArray(row.grouped(train.p).map(v=>arr(v.toVector)).toVector)))
      val name=c.field("config").compactPrint
      close(json,c.field("prediction"),name)
      close(model.summary,c.field("summary"),s"$name/summary")
      c.field("selection").asJsObject.fields.foreach((strategy,indices)=>close(ints(model.select(3,strategy,42)),indices,s"$name/select/$strategy"))
      c.field("distinct_selection").asJsObject.fields.foreach((strategy,indices)=>close(ints(model.selectDistinctLabels(2,strategy,42)),indices,s"$name/distinct/$strategy"))
      val artifact=Files.createTempFile("native-pattern-",".simedape");ModelArtifacts.write(artifact,model)
      val reloaded=ModelArtifacts.readPattern(artifact)
      close(JsArray(reloaded.predict(query).toVector.map(row=>JsArray(row.grouped(train.p).map(v=>arr(v.toVector)).toVector))),c.field("prediction"),s"$name/reloaded")
      Files.delete(artifact)
      val score=query.values.map(_.map(v=> !v.isFinite))
      close(Evaluation.evaluate(truth,predicted,score),c.field("metrics"),s"$name/metrics")
      intercept[IllegalArgumentException] {model.predict(train)}
    }
    val saved=Files.createTempFile("native-multivariable-",".npz");train.save(saved)
    val loaded=SeriesDataset.load(saved)
    loaded.labels shouldBe train.labels;loaded.variables shouldBe train.variables
    loaded.observed shouldBe train.observed
    Files.delete(saved)
  }
  "Masked PCA" should "use training-only imputation, ridge fitting and exact observed values" in {
    val f=Json.read(resource("multivariable.json")).field("basis")
    val matrix=f.field("matrix").values.map(_.values.map(_.number(Double.NaN)).toArray).toArray
    val model=MaskedPCA.fit(matrix,2,1e-6)
    val (row,diagnostics)=model.reconstruct(f.field("row").values.map(_.number(Double.NaN)).toArray)
    close(arr(row.toVector),f.field("expected"),"basis/reconstruction")
    close(diagnostics,f.field("diagnostics"),"basis/diagnostics")
  }

  "Observation masks" should "match NumPy PCG64 masks without changing measured support" in {
    val truth=SeriesDataset.load(resource("multivariable-truth.npz"))
    val masks=Json.read(resource("multivariable.json")).field("masks").asJsObject.fields
    masks.foreach { (key,expected)=>
      val parts=key.split('-');val mask=Evaluation.mask(truth,.4,parts(1).toLong,parts(0),Some(Vector("a|0")))
      close(JsArray(mask.toVector.map(row=>JsArray(row.grouped(truth.p).map(a=>JsArray(a.toVector.map(JsBoolean(_)))).toVector))),expected,key)
    }
  }
  "Paired calibration" should "match Python gains, held-out predictions, provenance gates and portable reload" in {
    val full=SeriesDataset.load(resource("paired-full.npz"));val training=SeriesDataset.load(resource("paired-training.npz"));val query=SeriesDataset.load(resource("paired-query.npz"))
    val expected=Json.read(resource("multivariable.json")).field("paired")
    val model=new PairedResidualEstimator().fit(full,training)
    val prediction=model.predict(query)
    def tensor(rows: Array[Array[Double]]): JsValue = JsArray(rows.toVector.map(row=>JsArray(row.grouped(full.p).map(a=>arr(a.toVector)).toVector)))
    close(tensor(prediction),expected.field("prediction"),"paired/prediction")
    close(model.summary,expected.field("summary"),"paired/summary")
    close(JsArray(model.diagnostics),expected.field("diagnostics"),"paired/diagnostics")
    val original=query.metadata.field("run_provenance").field("r3").asJsObject
    val departures=JsObject(original.field("departure_seconds").asJsObject.fields+("v0"->JsNumber(100.0)))
    val changed=JsObject((original.fields-"sha256")+("departure_seconds"->departures))
    val record=JsObject(changed.fields+("sha256"->JsString(Provenance.digest(changed))))
    close(model.assessInputs(model.context,record),expected.field("assessment"),"paired/assessment")
    val unsafe=query.copy(metadata=JsObject(query.metadata.fields+("run_provenance"->obj("r3"->record))))
    intercept[IllegalArgumentException] {model.predict(unsafe)}
    model.predict(unsafe,allowDepartureExtrapolation=true).length shouldBe query.n
    val artifact=Files.createTempFile("native-paired-",".simedape");ModelArtifacts.write(artifact,model)
    close(tensor(ModelArtifacts.readPaired(artifact).predict(query)),expected.field("prediction"),"paired/reloaded")
    Files.delete(artifact)
  }
