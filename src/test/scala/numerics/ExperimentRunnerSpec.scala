package org.interscity.htc.parity
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.interscity.htc.parity.experiments.*
import spray.json.*
import Json.*
import java.nio.file.{Files,Path}
import scala.util.Using

class ExperimentRunnerSpec extends AnyFlatSpec with Matchers:
  def resource(name: String): Path = Path.of(getClass.getResource(s"/parity/$name").toURI)
  def close(got: JsValue,expected: JsValue,path: String=""): Unit = (got,expected) match
    case (JsNumber(a),JsNumber(b))=>withClue(path+": "){a.toDouble shouldBe b.toDouble +- 2e-6*math.max(1,math.abs(b.toDouble))}
    case (a: JsObject,b: JsObject)=>withClue(path){a.fields.keySet shouldBe b.fields.keySet};b.fields.foreach((k,v)=>close(a.fields(k),v,s"$path/$k"))
    case (JsArray(a),JsArray(b))=>withClue(path){a.size shouldBe b.size};a.zip(b).zipWithIndex.foreach((v,i)=>close(v._1,v._2,s"$path/$i"))
    case _=>withClue(path+": "){got shouldBe expected}
  "Multivariable pipeline stage" should "match the Python held-out runner and write reloadable models and observation/prediction artifacts" in {
    val expected=Json.read(resource("experiment-reference.json"));val config=expected.field("config").asJsObject
    val output=Files.createTempDirectory("simedape-experiment-")
    val params=obj("experiment_config"->config,"base_dir"->JsString(resource("experiment-dataset.npz").getParent.toString))
    new Pipeline(RunConfig(Json.empty),output).run(Vector(StageRequest("multivariable_experiment",params)))
    val got=Json.read(output.resolve("experiment.json"))
    got.field("source_sha256") shouldBe expected.field("source_sha256")
    close(got.field("summary"),expected.field("summary"),"summary")
    val compared=Vector("id","name","seed","fraction","experiment","selection","representative_budget","selection_applied","input_semantics","uncalibrated_proxy_ablation","selected_labels","observed_values","reference_values","metrics","per_run","simulation")
    got.field("results").values.zip(expected.field("results").values).foreach { (row,reference)=>
      compared.foreach(key=>close(row.field(key),reference.field(key),row.field("id").text()+"/"+key))
      val modelFile=output.resolve(row.field("model_artifact").text());Files.isRegularFile(modelFile) shouldBe true
      val model=ModelArtifacts.readPattern(modelFile);model.training.runs.toSet shouldBe Set("r0","r1","r2")
      val folder=output.resolve(row.field("id").text())
      SeriesDataset.load(folder.resolve("observations.npz")).runs.toSet shouldBe Set("r3")
      Using.resource(new java.util.zip.ZipFile(folder.resolve("predictions.npz").toFile)) { zip=>
        val prediction=Using.resource(zip.getInputStream(zip.getEntry("prediction.npy")))(Npy.read)
        prediction.shape shouldBe Vector(3,5,2)
      }
    }
    got.field("results").values.size shouldBe expected.field("results").values.size
    intercept[IllegalArgumentException] {ExperimentRunner.run(config,resource("experiment-dataset.npz").getParent,output)}
  }
  private def adapter(root: Path,mode: String="complete"): JsObject =
    val source=root.resolve("SimulatorStub.java")
    Files.writeString(source,"""import java.nio.file.*;
import java.security.MessageDigest;
import java.util.HexFormat;
import java.util.regex.Pattern;
public class SimulatorStub {
  static String digest(Path path) throws Exception {return HexFormat.of().formatHex(MessageDigest.getInstance("SHA-256").digest(Files.readAllBytes(path)));}
  public static void main(String[] args) throws Exception {
    Path manifest=Path.of(args[0]),output=Path.of(args[1]);
    if(args[5].equals("timeout")){Thread.sleep(10000);return;}
    Path dataset=output.resolve("dataset.npz");Files.copy(Path.of(args[4]),dataset);
    if(args[5].equals("missing"))return;
    String text=Files.readString(manifest);
    var match=Pattern.compile("\"selected_units\"\\s*:\\s*(\\[[^\\]]*\\])",Pattern.DOTALL).matcher(text);
    if(!match.find())throw new Exception("No unit manifest");
    String units=args[5].equals("units")?"[\"wrong\"]":match.group(1);
    String hash=args[5].equals("hash")?"wrong":digest(manifest);
    String file=args[5].equals("escape")?"../dataset.npz":"dataset.npz";
    String receipt="{\"schema\":\"simedape.simulator_receipt.v1\",\"status\":\"complete\",\"run_id\":\""+args[2]+"\",\"manifest_sha256\":\""+hash+"\",\"policy\":\"vehicle_sample\",\"seed\":"+args[3]+",\"instantiated_units\":"+units+",\"dataset\":\""+file+"\",\"dataset_sha256\":\""+digest(dataset)+"\"}";
    Files.writeString(output.resolve("receipt.json"),receipt);
  }
}
""")
    obj("command"->strings(Vector(Path.of(System.getProperty("java.home"),"bin","java").toString,source.toString,"{manifest}","{output_dir}","{run_id}","{seed}",resource("paired-query.npz").toString,mode)),"capabilities"->strings(Vector("vehicle_sample")),"timeout_seconds"->JsNumber(if mode=="timeout" then .1 else 30))
  "Reduced simulator boundary" should "require a matching receipt, exact instantiated units and an output-contained dataset" in {
    val root=Files.createTempDirectory("simedape-adapter-")
    val plan=obj("schema"->JsString("simedape.reduction.v1"),"policy"->JsString("vehicle_sample"),"seed"->JsNumber(42),"selected_units"->strings(Vector("v0")))
    val (data,receipt)=new CommandSimulator(adapter(root),root).run(plan,root.resolve("complete"),"r3")
    data.runs.toSet shouldBe Set("r3");receipt.field("receipt").field("status").text() shouldBe "complete"
    for mode<-Vector("missing","hash","units","escape") do
      intercept[IllegalArgumentException] {new CommandSimulator(adapter(root,mode),root).run(plan,root.resolve(mode),"r3")}
    intercept[java.util.concurrent.TimeoutException] {new CommandSimulator(adapter(root,"timeout"),root).run(plan,root.resolve("timeout"),"r3")}
  }
  it should "run the paired experiment path using an isolated deterministic adapter" in {
    val root=Files.createTempDirectory("simedape-paired-experiment-")
    val base=resource("paired-experiment-full.npz").getParent
    val config=obj("dataset"->JsString("paired-experiment-full.npz"),"training_runs"->strings(Vector("r0","r1","r2")),"test_runs"->strings(Vector("r3")),
      "experiment"->JsString("reduced"),"fractions"->arr(Vector(.25)),"seeds"->ints(Vector(42)),"models"->JsArray(obj("name"->JsString("paired"),"method"->JsString("paired_residual"),"paired_training_dataset"->JsString("paired-training.npz"))),
      "simulator"->adapter(root),"reduction"->obj("policy"->JsString("vehicle_sample")))
    val report=ExperimentRunner.run(config,base,root.resolve("experiment"))
    val row=report.field("results").values.head
    close(row.field("metrics"),Json.read(resource("multivariable.json")).field("paired").field("metrics"),"paired-runner/metrics")
    row.field("uncalibrated_proxy_ablation").bool() shouldBe false
    row.field("selected_labels").names shouldBe Vector("a|0")
    ModelArtifacts.readPaired(root.resolve("experiment").resolve(row.field("model_artifact").text())).full.runs.toSet shouldBe Set("r0","r1","r2")
  }
