package org.interscity.htc.parity
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.apache.pekko.actor.testkit.typed.scaladsl.ActorTestKit
import org.apache.pekko.http.scaladsl.Http
import spray.json.*
import Json.*
import java.nio.file.{Files,Path}
import java.net.URI
import java.net.http.{HttpClient,HttpRequest,HttpResponse}
import scala.concurrent.Await
import scala.concurrent.duration.*
import scala.util.Using
import scala.jdk.CollectionConverters.*

class NativePipelineApiSpec extends AnyFlatSpec with Matchers:
  private val fixture=scala.io.Source.fromResource("parity/estimation-reference.json").mkString.parseJson
  private def setup(root: Path): RunConfig =
    val csv=root.resolve("csv");Files.createDirectories(csv)
    val raw=fixture.field("raw").values
    // Fixture is a normal legacy CSV scenario: ticks in (0,3600], one file per link.
    raw.zipWithIndex.foreach { (values,index)=>
      Files.writeString(csv.resolve(s"${index+1}.csv"),"time;vehicle;count\n"+values.values.zipWithIndex.map((v,j)=>s"${j+1};v$index;${v.number()}").mkString("\n"))
    }
    val roads=root.resolve("roads.csv")
    Files.writeString(roads,"link;length;capacity;freespeed\n"+fixture.field("roads").asJsObject.fields.toVector.sortBy(_._1).map((k,v)=>s"$k;${v.values.map(_.number()).mkString(";")}").mkString("\n"))
    RunConfig(obj("scenario_name"->JsString("native-fixture"),"output_dir"->JsString(root.resolve("output").toString),"scenario_dir"->JsString(csv.toString),"street_info_file"->JsString(roads.toString),
      "intervals"->obj("end_time"->JsNumber(3600),"interval_size"->JsNumber(3600)),"clustering"->obj("n_clusters"->JsNumber(2),"max_iter"->JsNumber(8),"random_state"->JsNumber(10))))
  "Native pipeline" should "train, persist, resume, estimate, compare and produce a self-contained report without Python" in {
    val root=Files.createTempDirectory("simedape-native-");val cfg=setup(root);val out=Path.of("target/parity/native-pipeline").toAbsolutePath
    Files.createDirectories(out)
    val stages=Vector("data_load","process_dataset","clustering","simulation_points","pca_train","warping_paths","scenario_metrics","selection","selective_instantiation").map(StageRequest(_))++
      Vector(StageRequest("estimation_variants",obj("variants"->strings(Vector("all")),"workers"->JsNumber(2))),
        StageRequest("compare",obj("estimated_files"->strings(Vector("result_avg","result_pca_sp","result_pca_d_delta")),"generate_plots"->JsBoolean(true))),StageRequest("model_export"))
    val events=scala.collection.mutable.ArrayBuffer.empty[JsObject]
    new Pipeline(cfg,out).run(stages,e=>events+=e)
    events.count(_.field("event").text()=="stage_end") shouldBe stages.size
    Exports.methods(out,"result").size shouldBe 100
    new Pipeline(cfg,out,Some(out)).run(Vector(StageRequest("sweep",obj("variants"->strings(Vector("avg")),"generate_plots"->JsBoolean(true),
      "scenarios"->JsArray(obj("label"->JsString("100%"),"dir"->JsString("training"),"percentage"->JsNumber(100)))))))
    Files.isRegularFile(out.resolve("plots/sweep_compare_100p_consolidated.pdf")) shouldBe true
    val isolated=new Pipeline(cfg,Files.createTempDirectory(root,"isolated-"),Some(out))
    val statuses=isolated.variants(Vector("avg","pca_d_base"),root.resolve("missing-input").toString,100,"isolated")()
    statuses.field("avg").field("ok").bool() shouldBe false
    statuses.field("pca_d_base").field("ok").bool() shouldBe true
    val m=ModelIO.read(out.resolve("model.simedape"));m.pca.size shouldBe 2;m.dtw.nonEmpty shouldBe true;m.report.nonEmpty shouldBe true
    m.report.get.api(Some(0)).field("cluster").field("near").values.nonEmpty shouldBe true
    Exports.read(out,"variants_status").asJsObject.fields.values.forall(_.field("ok").bool()) shouldBe true
    val resumed=Files.createTempDirectory(root,"resumed-")
    new Pipeline(cfg,resumed,Some(out)).run(Vector(StageRequest("estimation_variants",obj("variants"->strings(Vector("pca_d_base"))))))
    Files.isRegularFile(resumed.resolve("result_pca_d_base.json")) shouldBe true
    Using.resource(org.apache.pdfbox.Loader.loadPDF(Files.readAllBytes(out.resolve("plots/compare_consolidated.pdf")))) { pdf=>
      pdf.getNumberOfPages should be > 3
      new org.apache.pdfbox.text.PDFTextStripper().getText(pdf) should include("MAPE")
    }
    val report=Reports.build(out);Files.write(out.resolve("article.zip"),report.zip("Native fixture"))
    Using.resource(new java.util.zip.ZipFile(out.resolve("article.zip").toFile)) { zip=>
      zip.getEntry("main.tex") should not be null
      zip.getEntry("tables/ranking.tex") should not be null
      zip.getEntry("figures/ranking.pdf") should not be null
    }
  }
  "Native API" should "persist configs, execute a Pekko-managed run, expose progress and reject traversal" in {
    val kit=ActorTestKit("NativeApiSpec");given org.apache.pekko.actor.typed.ActorSystem[?]=kit.system
    val root=Files.createTempDirectory("simedape-api-");val cfg=setup(root);val service=new Api(root.resolve("state"))
    val binding=Await.result(Http()(kit.system).newServerAt("127.0.0.1",0).bind(service.route),15.seconds)
    val client=HttpClient.newHttpClient();val base=s"http://127.0.0.1:${binding.localAddress.getPort}/api"
    def call(method: String,path: String,body: JsValue=Json.empty): (Int,JsValue) =
      val request=HttpRequest.newBuilder(URI.create(base+path)).header("Content-Type","application/json").method(method,if method=="GET" || method=="DELETE" then HttpRequest.BodyPublishers.noBody() else HttpRequest.BodyPublishers.ofString(body.compactPrint)).build()
      val response=client.send(request,HttpResponse.BodyHandlers.ofString());(response.statusCode(),response.body().parseJson)
    try
      call("GET","/health")._1 shouldBe 200
      call("POST","/configs",obj("name"->JsString("fixture"),"config"->cfg.json))._1 shouldBe 201
      call("GET","/configs/fixture")._2.field("config").field("scenario_name").text() shouldBe "native-fixture"
      call("POST","/configs",obj("name"->JsString(".."),"config"->cfg.json))._1 shouldBe 400
      call("POST","/runs",obj("stages"->strings(Vector("bogus"))))._1 shouldBe 400
      val created=call("POST","/runs",obj("config"->JsString("fixture"),"stages"->strings(Vector("data_load","process_dataset"))))
      created._1 shouldBe 202;val id=created._2.field("run_id").text()
      val deadline=System.nanoTime()+15.seconds.toNanos
      var state=call("GET",s"/runs/$id")._2
      while Set("queued","running")(state.field("status").text()) && System.nanoTime()<deadline do
        Thread.sleep(50);state=call("GET",s"/runs/$id")._2
      withClue(state.compactPrint) {state.field("status").text() shouldBe "completed"}
      call("GET",s"/runs/$id/progress")._2.field("percent").number() shouldBe 100
      call("GET","/estimation-variants")._2.field("variants").values.size shouldBe 100
      call("GET",s"/runs/$id/estimation?prefix=..%2F")._1 shouldBe 400
      call("DELETE",s"/runs/$id")._1 shouldBe 409
      Using.resource(Files.list(root.resolve("state/jobs")))(_.count()) shouldBe 1
    finally
      Await.result(binding.unbind(),10.seconds);service.close();kit.shutdownTestKit()
  }
