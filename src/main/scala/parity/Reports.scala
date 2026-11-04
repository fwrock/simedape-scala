package org.interscity.htc.parity

import spray.json.*
import Json.*
import java.nio.file.{Files,Path}
import java.io.{ByteArrayOutputStream,FileNotFoundException}
import java.util.zip.{ZipOutputStream,ZipEntry}
import org.apache.pdfbox.pdmodel.{PDDocument,PDPage,PDPageContentStream}
import org.apache.pdfbox.pdmodel.common.PDRectangle
import org.apache.pdfbox.pdmodel.font.PDType0Font
import scala.util.Using
import scala.collection.mutable

final case class ReportTable(name: String,caption: String,header: Vector[String],rows: Vector[Vector[String]],best: Map[Int,Set[Int]]=Map.empty)
final case class ReportPackage(pdf: Array[Byte],tables: Vector[(String,String)],figures: Vector[(String,Array[Byte])],meta: JsObject):
  def zip(title: String): Array[Byte] =
    val buffer=new ByteArrayOutputStream()
    Using.resource(new ZipOutputStream(buffer)) { zip=>
      def put(name: String,data: Array[Byte]): Unit = {zip.putNextEntry(new ZipEntry(name));zip.write(data);zip.closeEntry()}
      put("report.pdf",pdf)
      tables.foreach((n,t)=>put(s"tables/$n.tex",t.getBytes(java.nio.charset.StandardCharsets.UTF_8)))
      figures.foreach((n,p)=>put(s"figures/$n.pdf",p))
      val tex="\\documentclass{article}\n\\usepackage[utf8]{inputenc}\n\\usepackage{booktabs,graphicx,geometry}\n\\geometry{margin=1.5cm}\n\\begin{document}\n\\title{"+Reports.escape(title)+"}\\maketitle\n"+
        tables.map((n,_)=>s"\\input{tables/$n.tex}").mkString("\n")+"\n"+figures.map((n,_)=>s"\\begin{figure}[ht]\\centering\\includegraphics[width=\\linewidth]{figures/$n.pdf}\\caption{${Reports.escape(n)}}\\end{figure}").mkString("\n")+"\n\\end{document}\n"
      put("main.tex",tex.getBytes(java.nio.charset.StandardCharsets.UTF_8));put("metadata.json",meta.prettyPrint.getBytes(java.nio.charset.StandardCharsets.UTF_8))
    }
    buffer.toByteArray

object Reports:
  private val language=Using.resource(getClass.getResourceAsStream("/parity/report-language.json"))(s=>new String(s.readAllBytes(),java.nio.charset.StandardCharsets.UTF_8).parseJson)
  private val escapes=Map('\\'->"\\textbackslash{}",'&'->"\\&",'%'->"\\%",'_'->"\\_",'#'->"\\#",'$'->"\\$",'{'->"\\{",'}'->"\\}",'~'->"\\textasciitilde{}",'^'->"\\textasciicircum{}",'†'->"\\textdagger{}",'Δ'->"$\\Delta$",'λ'->"$\\lambda$")
  def escape(s: String): String = s.flatMap(c=>escapes.getOrElse(c,c.toString))
  private def fmt(v: JsValue): String = v match {case JsNumber(n)=>f"${n.toDouble}%.2f";case JsString(s)=>s;case _=>"--"}
  private def latex(table: ReportTable): String =
    val cols=if table.header.size>6 then "p{0.22\\linewidth}"+"r"*(table.header.size-1) else "p{0.32\\linewidth}"+"r"*(table.header.size-1)
    val b=new StringBuilder("\\begin{table*}[ht]\n\\centering\\small\n\\caption{"+escape(table.caption)+"}\n\\label{tab:"+table.name+"}\n\\resizebox{\\linewidth}{!}{%\n\\begin{tabular}{"+cols+"}\n\\toprule\n")
    b.append(table.header.map(escape).mkString(" & ")+" \\\\\n\\midrule\n")
    table.rows.zipWithIndex.foreach { (row,i)=>b.append(row.zipWithIndex.map { (cell,j)=>val v=escape(cell);if table.best.getOrElse(j,Set.empty)(i) then s"\\textbf{$v}" else v }.mkString(" & ")+" \\\\\n") }
    b.append("\\bottomrule\n\\end{tabular}}\n\\end{table*}\n").toString

  /** Low-level vector renderer with embedded Unicode font, repeated headers and bounded row pages. */
  private final class PDF(title: String) extends AutoCloseable:
    val doc=new PDDocument()
    private val font=Using.resource(getClass.getResourceAsStream("/parity/fonts/DejaVuSans.ttf"))(s=>PDType0Font.load(doc,s,true))
    private var stream: PDPageContentStream=null
    private var y=0f
    val width=842f;val height=595f;val margin=36f
    def page(caption: String): Unit =
      if stream!=null then stream.close()
      val page=new PDPage(new PDRectangle(width,height));doc.addPage(page);stream=new PDPageContentStream(doc,page)
      text(title,margin,height-25,9);text(caption,margin,height-52,13);y=height-78
      text(s"SimEDaPE · ${doc.getNumberOfPages}",width-145,18,8)
    def text(value: String,x: Float,top: Float,size: Float): Unit =
      val clean=value.map(c=>if c=='\n' || c=='\r' || c=='\t' then ' ' else c).filter(c=> !Character.isISOControl(c))
      stream.beginText();stream.setFont(font,size);stream.newLineAtOffset(x,top);stream.showText(clean);stream.endText()
    def line(x1: Float,y1: Float,x2: Float,y2: Float): Unit = {stream.moveTo(x1,y1);stream.lineTo(x2,y2);stream.stroke()}
    def color(r: Float,g: Float,b: Float): Unit = stream.setNonStrokingColor(r,g,b)
    private def wrap(value: String,w: Float,size: Float): Vector[String] =
      val lines=mutable.ArrayBuffer.empty[String];var current=""
      value.foreach { c=>
        val candidate=current+c
        if current.nonEmpty && font.getStringWidth(candidate)*size/1000>w then {lines+=current;current=c.toString} else current=candidate
      }
      lines+=current;lines.toVector
    def overview(notes: Vector[String]): Unit =
      page(title)
      notes.foreach { note=>
        wrap(note,width-margin*2,10).foreach(t=>{text(t,margin,y,10);y-=15});y-=12
      }
    def table(table: ReportTable): Unit =
      val fontSize=if table.header.size>7 then 7f else 8f
      val available=width-2*margin;val first=if table.header.size>7 then 150f else available*.30f
      val widths=Vector(first)++Vector.fill(table.header.size-1)((available-first)/(table.header.size-1))
      def row(cells: Vector[String],header: Boolean): Unit =
        val lines=cells.zip(widths).map((c,w)=>wrap(c,w-7,fontSize))
        val size=lines.map(_.size).maxOption.getOrElse(1)*(fontSize+3)+8
        if y-size<margin then {page(table.caption+" (continued)");draw(table.header,true)}
        drawLines(lines,size,header)
      def drawLines(lines: Vector[Vector[String]],size: Float,header: Boolean): Unit =
        if header then {color(.91f,.94f,.97f);stream.addRect(margin,y-size,available,size);stream.fill();color(0,0,0)}
        var x=margin
        lines.zip(widths).foreach { (ls,w)=>ls.zipWithIndex.foreach((s,i)=>text(s,x+3,y-12-i*(fontSize+3),fontSize));x+=w }
        y-=size;line(margin,y,margin+available,y)
      def draw(cells: Vector[String],header: Boolean): Unit =
        val ls=cells.zip(widths).map((c,w)=>wrap(c,w-7,fontSize));drawLines(ls,ls.map(_.size).max*(fontSize+3)+8,header)
      page(table.caption);row(table.header,true);table.rows.foreach(row(_,false))
    def bars(caption: String,labels: Vector[String],values: Vector[Option[Double]]): Unit =
      labels.indices.grouped(22).foreach { chunk=>
        page(caption);val max=values.flatten.map(math.abs).maxOption.getOrElse(1.0).max(1e-12)
        val x=300f;val barWidth=width-x-margin-65
        chunk.foreach { i=>
          val label=wrap(labels(i),245,8).headOption.getOrElse("");text(label,margin,y,8)
          values(i).foreach { v=>
            val size=(math.abs(v)/max*barWidth).toFloat
            val baseline=labels(i).endsWith("†")
            if baseline then color(.68f,.68f,.68f) else color(.12f,.43f,.69f)
            stream.addRect(x,y-3,size,10);stream.fill()
            if baseline then
              stream.setStrokingColor(.95f,.95f,.95f)
              var offset=0f
              while offset<size do {val height=math.min(10f,size-offset);line(x+offset,y-3,x+offset+height,y-3+height);offset+=8}
              stream.setStrokingColor(0f,0f,0f)
            color(0,0,0)
            text(f"$v%.2f",width-margin-55,y,8)
          };y-=19
        }
      }
    def scatter(caption: String,points: Vector[(Double,Double)]): Unit =
      page(caption);val lo=math.min(0,points.flatMap((a,b)=>Vector(a,b)).minOption.getOrElse(0.0));val hi=points.flatMap((a,b)=>Vector(a,b)).maxOption.getOrElse(1.0).max(lo+1)
      def x(v: Double): Float = (margin+35+(v-lo)/(hi-lo)*650).toFloat
      def yy(v: Double): Float = (70+(v-lo)/(hi-lo)*380).toFloat
      line(x(lo),yy(lo),x(hi),yy(hi));line(x(lo),yy(lo),x(hi),yy(lo));line(x(lo),yy(lo),x(lo),yy(hi))
      color(.12f,.43f,.69f);points.foreach((a,b)=>{stream.addRect(x(a),yy(b),2,2);stream.fill()});color(0,0,0)
      text("Real",x((hi+lo)/2),48,9);text("Estimated",margin,yy(hi)+8,9)
    def heatmap(caption: String,labels: Vector[String],columns: Vector[String],values: Vector[Vector[Option[Double]]]): Unit =
      val lows=columns.indices.map(j=>values.flatMap(_.lift(j).flatten).minOption.getOrElse(0.0))
      val highs=columns.indices.map(j=>values.flatMap(_.lift(j).flatten).maxOption.getOrElse(1.0))
      labels.indices.grouped(20).foreach { chunk=>
        page(caption);val start=230f;val cell=(width-margin-start)/math.max(1,columns.size)
        columns.zipWithIndex.foreach((label,j)=>wrap(label,cell-6,7).take(3).zipWithIndex.foreach((t,i)=>text(t,start+j*cell+3,y-i*9,7)))
        y-=38
        chunk.foreach { i=>
          text(wrap(labels(i),185,8).headOption.getOrElse(""),margin,y-13,8)
          values(i).zipWithIndex.foreach { (value,j)=>
            value.foreach { v=>
              val norm=if highs(j)==lows(j) then 0.0 else (v-lows(j))/(highs(j)-lows(j))
              color((.22+.68*norm).toFloat,(.75-.50*norm).toFloat,.30f);stream.addRect(start+j*cell,y-21,cell-1,20);stream.fill()
              color(0,0,0);text(f"$v%.1f",start+j*cell+3,y-14,7)
            }
          };y-=22
        }
      }
    def lines(caption: String,xs: Vector[Double],series: Vector[(String,Vector[Option[Double]])]): Unit =
      page(caption)
      val lo=xs.minOption.getOrElse(0.0);val hi=xs.maxOption.getOrElse(1.0).max(lo+1)
      val max=series.flatMap(_._2.flatten).maxOption.getOrElse(1.0).max(1e-12)
      def x(v: Double): Float = (margin+35+(v-lo)/(hi-lo)*650).toFloat
      def yy(v: Double): Float = (110+v/max*330).toFloat
      line(x(lo),110,x(hi),110);line(x(lo),110,x(lo),440)
      xs.foreach(v=>text(f"$v%.1f",x(v)-8,96,8));text("Partial scenario (%)",x((hi+lo)/2)-50,75,9)
      series.zipWithIndex.foreach { (s,index)=>
        val c=java.awt.Color.getHSBColor((index*.61803398875%1).toFloat,.7f,.65f)
        stream.setStrokingColor(c);stream.setNonStrokingColor(c)
        var previous: Option[(Float,Float)]=None
        xs.zip(s._2).foreach { (v,value)=>
          value match
            case None=>previous=None
            case Some(n)=>
              val point=(x(v),yy(n));previous.foreach((px,py)=>line(px,py,point._1,point._2))
              stream.addRect(point._1-2,point._2-2,4,4);stream.fill();previous=Some(point)
        }
        text(wrap(s._1,165,7).headOption.getOrElse(""),margin+(index%4)*190,52-(index/4)*10,7)
      }
      stream.setStrokingColor(0f,0f,0f);color(0,0,0)
    def chart(spec: JsValue): Unit =
      val kind=spec.field("type").text();val caption=spec.field("title").text();val series=spec.field("series").values
      if kind=="scatter" then
        scatter(caption,series.flatMap(_.field("data").values).filter(p=>p.field("x")!=JsNull && p.field("y")!=JsNull).map(p=>(p.field("x").number(),p.field("y").number())))
      else
        val categories=spec.field("x").field("categories").values.map(_.text())
        series.foreach { s=>
          val vs=s.field("data").values
          if vs.forall(v=>v==JsNull || v.isInstanceOf[JsNumber]) then
            bars(caption+" · "+s.field("name").text(),categories.zipWithIndex.map((v,i)=>v),vs.map(v=>if v==JsNull then None else Some(v.number())))
        }
    def bytes(): Array[Byte] =
      if stream!=null then {stream.close();stream=null}
      val out=new ByteArrayOutputStream();doc.save(out);out.toByteArray
    def close(): Unit = {if stream!=null then stream.close();doc.close()}

  def build(root: Path,params: Map[String,String]=Map.empty,compare: String="compare",sweep: String="sweep",title: String="SimEDaPE"): ReportPackage =
    val matrix=Exports.comparison(root,compare);val methods=matrix.field("methods").names
    if methods.isEmpty then throw new FileNotFoundException("No comparisons to report")
    val metrics=matrix.field("metrics").values.map(_.field("key").text())
    val lang=params.getOrElse("lang","pt");val english=lang=="en";val top=math.max(1,math.min(50,params.getOrElse("top","10").toInt))
    val metric=params.get("metric").filter(metrics.contains).getOrElse(metrics.head)
    val dictionary=language.field("STR").field(if english then "en" else "pt")
    def metricLabel(key: String): String = if english then language.field("METRIC_EN").field(key,JsString(key)).text() else Exports.labels.getOrElse(key,key)
    def tr(key: String): String = dictionary.field(key,JsString(key)).text()

    val ranked=methods.map { m=>val xs=metrics.map(k=>matrix.field("mape").field(k).field(m)).collect {case JsNumber(n)=>n.toDouble};m->(if xs.isEmpty then Double.PositiveInfinity else xs.sum/xs.size) }.sortBy(_._2)
    val finiteRanking=ranked.filter(_._2.isFinite)
    val shown=(finiteRanking.take(top)++finiteRanking.takeRight(top)).map(_._1).distinct
    val method=params.get("method").filter(methods.contains).getOrElse(ranked.find((m,_)=> !m.matches("^(base|pca_d_base)(_|$).*" )).map(_._1).getOrElse(methods.head))
    val detail=Exports.read(root,s"${compare}_$method");val tables=mutable.ArrayBuffer.empty[ReportTable]
    val names=params.getOrElse("names","short")
    def display(key: String): String =
      val entry=Variants.catalog.find(_.key==key)
      val label=entry.map(e=>if names=="key" then key else e.metadata.field(if names=="long" then (if english then "label_en" else "label") else "short").text(key)).getOrElse(key)
      label+(if key.matches("^(base|pca_d_base)(_|$).*" ) then " †" else "")
    def add(name: String,caption: String,header: Vector[String],rows: Vector[Vector[String]],best: String=""): Unit =
      val choices=if best.isEmpty then Map.empty[Int,Set[Int]] else (1 until header.size).map { j=>
        val vs=rows.zipWithIndex.flatMap((r,i)=>r(j).toDoubleOption.map(v=>i->(if best=="absmin" then math.abs(v) else v)))
        val optimum=if name=="ranking" && j==2 then vs.map(_._2).maxOption else vs.map(_._2).minOption; j->vs.filter((_,v)=>optimum.contains(v)).map(_._1).toSet
      }.toMap
      tables+=ReportTable(name,caption,header,rows,choices)
    add("ranking",tr("ranking_caption").replace("{top}",top.toString),Vector(tr("variation"),tr("mean_mape"),tr("coverage")),shown.map(m=>Vector(display(m),fmt(num(ranked.toMap.apply(m))),fmt(matrix.field("coverage_pct").field(m)))),"min")
    for (field,name,caption,best)<-Vector(("mape","mape_by_metric",tr("mape_caption"),"min"),("aggregate_error_pct","aggregate_error",tr("agg_caption"),"absmin")) do
      add(name,caption,Vector(tr("variation"))++metrics.map(k=>metricLabel(k).split(" \\(")(0)),shown.map(m=>Vector(display(m))++metrics.map(k=>fmt(matrix.field(field).field(k).field(m)))),best)
    add("robust_errors",tr("robust_caption").replace("{metric}",metricLabel(metric)),Vector(tr("variation"))++Exports.robust.map(key=>dictionary.field("stats").values.find(_.values.head.text()==key).map(_.values(1).text()).getOrElse(key.toUpperCase)),shown.map(m=>Vector(display(m))++Exports.robust.map(s=>fmt(matrix.field("robust").field(metric).field(s).field(m)))))
    val spec=detail.field("specific")
    val worst=spec.field("worst").field(metric).values
    if worst.nonEmpty then add("worst_series",tr("worst_caption").replace("{method}",display(method)).replace("{metric}",metricLabel(metric)),dictionary.field("worst_cols").names,worst.map(w=>Vector("label","real","estimated","error_pct").map(k=>fmt(w.field(k)))))
    for key<-Vector("by_cluster","by_interval","by_length") do
      val groups=spec.field("breakdowns").field(metric).field(key,empty).asJsObject.fields.toVector.sortBy(_._1)
      if groups.nonEmpty then add(s"breakdown_$key",s"${display(method)} · $key · $metric",Vector(key,"Series","WAPE (%)","Bias (%)","Median APE (%)"),groups.map((k,v)=>Vector(k)++Vector("n","wape","bias_pct","median_ape").map(s=>fmt(v.field(s)))))
    if Files.isRegularFile(root.resolve(s"${sweep}_summary.json")) then
      val sw=Charts.sweep(root,sweep);val ms=sw.field("methods").names.filter(shown.contains)
      add("sweep",tr("sweep_caption").replace("{metric}",metricLabel(metric)),Vector(tr("variation"))++sw.field("scenarios").values.map(_.field("label").text()),ms.map(m=>Vector(display(m))++sw.field("mape").field(metric).field(m).values.map(fmt)),"min")
    if names=="short" then add("notation",tr("notation_caption"),dictionary.field("notation_cols").names,shown.flatMap(k=>Variants.catalog.find(_.key==k).map(v=>Vector(display(k),v.metadata.field(if english then "help_en" else "help").text()+v.interpolation.map("; "+_).getOrElse("")+(if v.eigenvalueFilter then "; λ" else ""),k))))
    val figures=mutable.ArrayBuffer.empty[(String,Array[Byte])]
    val render=mutable.ArrayBuffer.empty[(String,PDF=>Unit)]
    render+=("ranking"->((p: PDF)=>p.bars(tr("x_mape"),shown.map(display),shown.map(m=>Option(ranked.toMap.apply(m)).filter(_.isFinite)))))
    val heat=shown.map(m=>metrics.map(k=>matrix.field("mape").field(k).field(m) match {case JsNumber(n)=>Some(n.toDouble);case _=>None}))
    render+=("heatmap"->((p: PDF)=>p.heatmap(tr("heat_title"),shown.map(display),metrics.map(metricLabel),heat)))
    val points=spec.field("scatter").field(metric).values.filter(_.values.size>=2).map(v=>(v.values(0).number(),v.values(1).number()))
    if points.nonEmpty then render+=("scatter"->((p: PDF)=>p.scatter(s"${display(method)} · $metric",points)))
    for key<-Vector("by_cluster","by_interval","by_length") do
      val groups=spec.field("breakdowns").field(metric).field(key,empty).asJsObject.fields.toVector.sortBy((_,v)=> -v.field("wape").number())
      if groups.nonEmpty then render+=(s"breakdown_$key"->((p: PDF)=>p.bars(s"${display(method)} · $key · WAPE (%)",groups.map(_._1),groups.map(_._2.field("wape") match {case JsNumber(n)=>Some(n.toDouble);case _=>None}))))
    if Files.isRegularFile(root.resolve(s"${sweep}_summary.json")) then
      val sw=Charts.sweep(root,sweep);val all=sw.field("methods").names
      val sorted=all.sortBy { m=>val vs=sw.field("mape").field(metric).field(m).values.collect {case JsNumber(n)=>n.toDouble};if vs.isEmpty then Double.PositiveInfinity else vs.sum/vs.size }
      val picked=(sorted.take(top)++sorted.takeRight(top)).distinct
      val xs=sw.field("scenarios").values.map(_.field("percentage").number())
      val series=picked.map(m=>display(m)->sw.field("mape").field(metric).field(m).values.map {case JsNumber(n)=>Some(n.toDouble);case _=>None})
      render+=("sweep"->((p: PDF)=>p.lines(s"MAPE (%) · $metric",xs,series)))
    render.foreach((name,draw)=>figures+=name->Using.resource(new PDF(title))(p=>{draw(p);p.bytes()}))
    val cover=dictionary.field("cover").names.map(_.replace("{n}",methods.size.toString).replace("{metric}",metricLabel(metric)).replace("{method}",display(method)))
    val pdf=Using.resource(new PDF(title)) { p=>p.overview(cover);tables.foreach(p.table);render.foreach(_._2(p));p.bytes() }
    ReportPackage(pdf,tables.toVector.map(t=>t.name->latex(t)),figures.toVector,obj("title"->JsString(title),"methods"->JsNumber(methods.size),"shown"->strings(shown),"metric"->JsString(metric),"method"->JsString(method),"names"->JsString(names),"lang"->JsString(lang),"cover"->strings(cover)))

  def savePlots(root: Path,prefix: String="compare",sweep: String="sweep",title: String="SimEDaPE"): Unit =
    val report=build(root,compare=prefix,sweep=sweep,title=title);val plots=root.resolve("plots");Files.createDirectories(plots)
    Files.write(plots.resolve(s"${Exports.valid(prefix)}_consolidated.pdf"),report.pdf)
    report.figures.foreach((name,bytes)=>Files.write(plots.resolve(s"${prefix}_$name.pdf"),bytes))
