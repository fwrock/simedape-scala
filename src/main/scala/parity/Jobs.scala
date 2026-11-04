package org.interscity.htc.parity

import spray.json.*
import Json.*
import java.nio.file.{Files,Path}
import java.util.concurrent.{CancellationException,Executors,ExecutorService}
import java.util.concurrent.atomic.AtomicBoolean
import org.apache.pekko.actor.typed.{ActorRef,ActorSystem,Behavior}
import org.apache.pekko.actor.typed.scaladsl.Behaviors
import scala.concurrent.{ExecutionContext,Future}
import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.util.{Try,Success,Failure,Using}

/** Durable job metadata; numerical work executes outside Pekko's actor and HTTP dispatchers. */
final class Jobs(root: Path,parallelism: Int=1)(using system: ActorSystem[?]) extends AutoCloseable:
  require(parallelism>=1,"Job parallelism must be positive")
  Files.createDirectories(root)
  private val lock=new Object
  private val records=mutable.LinkedHashMap.empty[String,JsObject]
  private val flags=mutable.Map.empty[String,AtomicBoolean]
  private val executor=Executors.newFixedThreadPool(parallelism)
  private given ExecutionContext=ExecutionContext.fromExecutor(executor)
  def now: JsNumber = num(System.currentTimeMillis()/1000.0).asInstanceOf[JsNumber]
  private def persist(id: String): Unit = Json.write(root.resolve(s"$id.json"),records(id))
  Using.resource(Files.list(root))(_.iterator().asScala.filter(_.toString.endsWith(".json")).foreach { file=>
    val record=Selection.read(file).asJsObject;val id=record.field("run_id").text()
    val saved=if Set("queued","running")(record.field("status").text()) then JsObject(record.fields++Map("status"->JsString("failed"),"finished_at"->now,"error"->JsString("Service restarted before job completion"))) else record
    records(id)=saved;persist(id)
  })
  private sealed trait Command
  private case class Submit(id: String,run: ()=>JsObject) extends Command
  private case class Done(id: String,result: Try[JsObject]) extends Command
  private val coordinator: ActorRef[Command]=system.systemActorOf(Behaviors.setup[Command] { ctx=>
    val pending=mutable.Queue.empty[Submit];var active=0
    def launch(): Unit =
      while active<parallelism && pending.nonEmpty do
        val job=pending.dequeue()
        if !cancelled(job.id) then
          active+=1;update(job.id)(_.copyFields("status"->JsString("running"),"started_at"->now))
          ctx.pipeToSelf(Future(job.run()))(out=>Done(job.id,out))
    Behaviors.receiveMessage {
      case submit: Submit=>pending.enqueue(submit);launch();Behaviors.same
      case Done(id,result)=>
        active-=1
        update(id) { row=>
          if cancelled(id) || result.failed.toOption.exists(_.isInstanceOf[CancellationException]) then row.copyFields("status"->JsString("cancelled"),"finished_at"->now)
          else result match
            case Success(value)=>row.copyFields("status"->JsString("completed"),"finished_at"->now,"result"->value)
            case Failure(error)=>row.copyFields("status"->JsString("failed"),"finished_at"->now,"error"->JsString(error.toString))
        }
        launch();Behaviors.same
    }
  },s"native-jobs-${java.util.UUID.randomUUID().toString}")
  extension (row: JsObject) private def copyFields(fields: (String,JsValue)*): JsObject = JsObject(row.fields++fields)
  private def update(id: String)(f: JsObject=>JsObject): Unit = lock.synchronized { records(id)=f(records(id));persist(id) }
  private def cancelled(id: String): Boolean = lock.synchronized(flags.get(id).exists(_.get()))
  def contains(id: String): Boolean = lock.synchronized(records.contains(id))
  def get(id: String): JsObject = lock.synchronized(records.getOrElse(id,throw new java.io.FileNotFoundException(s"Unknown run: $id")))
  def list: Vector[JsObject] = lock.synchronized(records.values.toVector.sortBy(-_.field("created_at").number()))
  def cancel(id: String): Boolean = lock.synchronized {
    val row=get(id)
    if !Set("queued","running")(row.field("status").text()) then false
    else
      flags.getOrElseUpdate(id,new AtomicBoolean()).set(true)
      if row.field("status").text()=="queued" then update(id)(_.copyFields("status"->JsString("cancelled"),"finished_at"->now))
      true
  }
  def submit(config: RunConfig,stages: Vector[StageRequest],output: Path,input: Option[Path],name: String="",configRef: JsValue=JsNull): String =
    Pipeline.validate(stages)
    val id=java.util.UUID.randomUUID().toString.replace("-","")
    val meta=obj("scenario_name"->JsString(config.name),"output_dir"->JsString(output.toAbsolutePath.toString),"data_source"->config.sourceDescription,
      "config"->configRef,"stages"->strings(stages.map(_.name)),"stage_params"->JsObject(stages.map(s=>s.name->s.params).toMap),
      "clustering_file"->config.clustering.field("output_file",JsString("clustering")))
    val progress=obj("total"->JsNumber(stages.size),"completed"->JsNumber(0),"percent"->JsNumber(0),"current_index"->JsNull,"current_stage"->JsNull,
      "stages"->JsArray(stages.map(s=>obj("name"->JsString(s.name),"status"->JsString("pending"),"started_at"->JsNull,"elapsed"->JsNull))))
    lock.synchronized {
      require(!records.values.exists(r=>Set("queued","running")(r.field("status").text()) && Path.of(r.field("meta").field("output_dir").text()).toAbsolutePath.normalize()==output.toAbsolutePath.normalize()),"Another active run owns this output directory")
      flags(id)=new AtomicBoolean(false)
      records(id)=obj("run_id"->JsString(id),"name"->JsString(name),"status"->JsString("queued"),"created_at"->now,"started_at"->JsNull,"finished_at"->JsNull,
        "logs"->JsArray(),"result"->JsNull,"error"->JsNull,"meta"->meta,"progress"->progress)
      persist(id)
    }
    def check(): Unit = if cancelled(id) || Thread.currentThread().isInterrupted then throw new CancellationException("Run cancelled")
    def log(message: String): Unit = update(id)(row=>row.copyFields("logs"->JsArray((row.field("logs").values:+JsString(message)).takeRight(1000))))
    def onProgress(event: JsObject): Unit = update(id) { row=>
      val p=row.field("progress");val index=event.field("index").int()-1;val kind=event.field("event").text()
      val stageRows=p.field("stages").values
      val changed=if index<0 then stageRows else stageRows.updated(index,JsObject(stageRows(index).asJsObject.fields++Map(
        "status"->JsString(if kind=="stage_start" then "running" else if kind=="stage_end" then "completed" else "failed"),
        "started_at"->(if kind=="stage_start" then now else stageRows(index).field("started_at")),"elapsed"->event.field("elapsed"))))
      val completed=changed.count(_.field("status").text()=="completed")
      val done=kind=="pipeline_end"
      row.copyFields("progress"->obj("total"->JsNumber(stages.size),"completed"->JsNumber(completed),"percent"->num(100.0*completed/stages.size),
        "current_index"->(if done then JsNull else event.field("index",p.field("current_index"))),"current_stage"->(if done then JsNull else event.field("name",p.field("current_stage"))),"stages"->JsArray(changed)))
    }
    coordinator ! Submit(id,()=>new Pipeline(config,output,input,log,()=>check()).run(stages,onProgress))
    id
  def close(): Unit =
    lock.synchronized(flags.values.foreach(_.set(true)))
    executor.shutdownNow()
