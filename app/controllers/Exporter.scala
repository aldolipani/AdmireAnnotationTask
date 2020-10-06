package controllers

import java.io.File
import java.util.Calendar

import akka.actor.{PoisonPill, UntypedActor, Props}
import akka.util.Timeout
import controllers.searchEngine.Engine
import models.{Seed, Embryon, Annotation}
import org.joda.time.DateTime
import play.Logger
import play.api.libs.concurrent.Akka
import play.api.libs.iteratee.{Iteratee, Enumerator, Concurrent}
import play.api.libs.json.{JsString, JsNumber, Json, JsValue}
import play.api.mvc.{Action, Controller, WebSocket}
import akka.pattern.ask
import scala.concurrent.Await
import scala.concurrent.duration._
import play.api.Play.current
import scala.concurrent.ExecutionContext.Implicits.global
import java.nio.file.{Paths, Files}
import java.nio.charset.StandardCharsets

import scala.util.Success

/**
 * Created by Aldo on 04/08/14.
 */
object Exporter extends Controller with Secured {

  implicit val timeout = Timeout(5 seconds)

  case class UserInfo(username: String, annotations: Int = 0, garbagedWords: Int = 0, termNClass: Map[String, Int])

  def initializeExporter = {
    new File("exportation").mkdir()
  }

  def markSeedsIn(sentence: String, categoryId: String, userId: Int, seedId:String):String = {
    if (sentence.size > 2 && seedId.toLowerCase.last == 'o' && sentence.indexOf(seedId + "OS") >= 0) {
      sentence.replace(seedId+"OS", "<" + categoryId + " user_id=\"" + userId + "\">" + seedId + "OS</" + categoryId + ">")
    } else if (sentence.size > 2 && seedId.toLowerCase.last == 'o' && sentence.indexOf(seedId + "os") >= 0) {
      sentence.replace(seedId+"os", "<" + categoryId + " user_id=\"" + userId + "\">" + seedId + "os</" + categoryId + ">")
    } else if (sentence.size > 3 && seedId.toLowerCase.last == 'y' && sentence.indexOf(seedId + "ies") >= 0) {
      sentence.replace(seedId+"ies", "<" + categoryId + " user_id=\"" + userId + "\">" + seedId + "ies</" + categoryId + ">")
    } else if (sentence.size > 3 && seedId.toLowerCase.last == 'y' && sentence.indexOf(seedId + "IES") >= 0) {
      sentence.replace(seedId+"IES", "<" + categoryId + " user_id=\"" + userId + "\">" + seedId + "IES</" + categoryId + ">")
    } else if (sentence.indexOf(seedId + "s") >= 0) {
      sentence.replace(seedId+"s", "<" + categoryId + " user_id=\"" + userId + "\">" + seedId + "s</" + categoryId + ">")
    } else if (sentence.indexOf(seedId + "S") >= 0) {
      sentence.replace(seedId+"S", "<" + categoryId + " user_id=\"" + userId + "\">" + seedId + "S</" + categoryId + ">")
    } else if (sentence.indexOf(seedId) >= 0) {
      sentence.replace(seedId, "<" + categoryId + " user_id=\"" + userId + "\">" + seedId + "</" + categoryId + ">")
    } else {
      sentence
    }
  }
/*    val nSeedId = if (seedId.size > 1 && seedId.charAt(seedId.size - 1) == 'y') seedId.substring(0, seedId.size - 1) else seedId
    val ray = Dashboard.computeRay(sentence.split(" "), nSeedId)
    sentence.split(" ").map(t => {
      if (t.contains(nSeedId) && Math.abs(t.size - nSeedId.size) <= ray)
        "<" + categoryId + " user_id=\""+userId+"\">" + t + "</" + categoryId + ">"
      else
        t
    }).mkString(" ")
  }*/

  def markSeedsIn(sentence: String, userCat: List[(Int, String)], seedId:String):String = {
    if (sentence.size > 2 && seedId.toLowerCase.last == 'o' && sentence.indexOf(seedId + "OS") >= 0) {
      sentence.replace(seedId+"OS", "<Conflict" + " user_ids=\""+userCat.map(_._1).mkString(",")+"\" " + "category_ids=\""+userCat.map(_._2).mkString(",")+"\">" + seedId + "OS</Conflict>")
    } else if (sentence.size > 2 && seedId.toLowerCase.last == 'o' && sentence.indexOf(seedId + "os") >= 0) {
      sentence.replace(seedId+"os", "<Conflict" + " user_ids=\""+userCat.map(_._1).mkString(",")+"\" " + "category_ids=\""+userCat.map(_._2).mkString(",")+"\">" + seedId + "os</Conflict>")
    } else if (sentence.size > 3 && seedId.toLowerCase.last == 'y' && sentence.indexOf(seedId + "ies") >= 0) {
      sentence.replace(seedId+"ies", "<Conflict" + " user_ids=\""+userCat.map(_._1).mkString(",")+"\" " + "category_ids=\""+userCat.map(_._2).mkString(",")+"\">" + seedId + "ies</Conflict>")
    } else if (sentence.size > 3 && seedId.toLowerCase.last == 'y' && sentence.indexOf(seedId + "IES") >= 0) {
      sentence.replace(seedId+"IES",  "<Conflict" + " user_ids=\""+userCat.map(_._1).mkString(",")+"\" " + "category_ids=\""+userCat.map(_._2).mkString(",")+"\">" + seedId + "IES</Conflict>")
    } else if (sentence.indexOf(seedId + "s") >= 0) {
      sentence.replace(seedId+"s",  "<Conflict" + " user_ids=\""+userCat.map(_._1).mkString(",")+"\" " + "category_ids=\""+userCat.map(_._2).mkString(",")+"\">" + seedId + "s</Conflict>")
    } else if (sentence.indexOf(seedId + "S") >= 0) {
      sentence.replace(seedId+"S",  "<Conflict" + " user_ids=\""+userCat.map(_._1).mkString(",")+"\" " + "category_ids=\""+userCat.map(_._2).mkString(",")+"\">" + seedId + "S</Conflict>")
    } else if (sentence.indexOf(seedId) >= 0) {
      sentence.replace(seedId,  "<Conflict" + " user_ids=\""+userCat.map(_._1).mkString(",")+"\" " + "category_ids=\""+userCat.map(_._2).mkString(",")+"\">" + seedId + "</Conflict>")
    } else {
      sentence
    }

  /*  val nSeedId = if (seedId.size > 1 && seedId.charAt(seedId.size - 1) == 'y') seedId.substring(0, seedId.size - 1) else seedId
    val ray = Dashboard.computeRay(sentence.split(" "), nSeedId)
    sentence.split(" ").map(t => {
      if (t.contains(nSeedId) && Math.abs(t.size - nSeedId.size) <= ray)
        "<Conflict" + " user_ids=\""+userCat.map(_._1).mkString(",")+"\" " + "category_ids=\""+userCat.map(_._2).mkString(",")+"\">" + t + "</Conflict>"
      else
        t
    }).mkString(" ")*/
  }

  def getDateYYYYMMDD: String = {
    DateTime.now.year.get.toString+DateTime.now.monthOfYear.get.toString+DateTime.now.dayOfMonth.get.toString
  }

  def exportAnnotationsAsDocuments = Action {
    val dateYYYYMMDD = getDateYYYYMMDD
    initializeExporter
    val nEmbryon = Annotation.getSizeAnnotatedEmbryonXInOrder
    Logger.info("Exporter - exportAnnotationsAsDocuments - " + nEmbryon + " embryons to export found!")
    val engine = new Engine
    var document = Map[Int, String]()
    var pFileId = ""

    for(i <- (0 until nEmbryon)) {
      val embryon = Embryon.findByID(Annotation.getAnnotatedEmbryonXInOrder(i))
      Logger.info("Exporter - exportAnnotationsAsDocuments - Analyzing embryon " + embryon.id)
      val (fileId: String, sentenceId: Int) = (embryon.idEmbryon.split("#").head, embryon.idEmbryon.split("#").last.toInt); if (pFileId == "") pFileId = fileId
      Logger.info("Exporter - exportAnnotationsAsDocuments - Embryon fileId " + fileId)
      if (pFileId != fileId) {
        def getMissingSentences(document: Map[Int, String]): Map[Int, String] = {
          def getMissingSentences(dt: Map[Int, String], step: Int): Map[Int, String] = {
            if (dt.contains(step))
              getMissingSentences(dt, step + 1)
            else {
              val d = engine.getDocumentByID(pFileId, step)
              if (d.content == "Error: document not found!")
                dt
              else
                getMissingSentences(dt + (step -> ("<Sentence>" + d.content + "</Sentence>")), step + 1)
            }
          }
          getMissingSentences(document, 0)
        }

        val text = "<Content>" + getMissingSentences(document.mapValues("<AnnotatedSentence>"+_+"</AnnotatedSentence>")).toList.sortBy(_._1).map(_._2).mkString("\n") + "</Content>"

        val xmlFile = new File("exportation" + File.separator + dateYYYYMMDD + File.separator + pFileId.replaceAll("[\\\\/]", File.separator)+".xml")
        if (!xmlFile.getParentFile.exists()) xmlFile.getParentFile.mkdirs()
        Logger.info("Exporter - exportAnnotationsAsDocuments - Print file " + xmlFile.getCanonicalPath)
        Files.write(Paths.get(xmlFile.getAbsolutePath), text.getBytes(StandardCharsets.UTF_8))
        document = Map[Int, String]()
      }
      val seedId = embryon.seed.id
      Logger.info("Exporter - exportAnnotationsAsDocuments - getDocumentByID " + embryon.idEmbryon)
      val sentence =
        if (document.contains(sentenceId))
          document.get(sentenceId).get
        else
          engine.getDocumentByID(embryon.idEmbryon).content

      val categories = Annotation.getAnnotationsBy(embryon.id, seedId)
      val markedSentence = if(categories.size > 1){ // conflict
        Logger.info("Exporter - exportAnnotationsAsDocuments - getDocumentByID " + embryon.idEmbryon + " has a conflict!")
        val userCat = categories.map(id => Annotation.findByID(id)).map(a => (a.user.id, a.category.id))
        markSeedsIn(sentence, userCat, seedId)
      }else{
        val a = Annotation.findByID(categories.head)
        (a.user.id, a.category.id)
        markSeedsIn(sentence, a.category.id, a.user.id, seedId)
      }
      document = document + (sentenceId -> markedSentence)
      pFileId=fileId
    }

    //val actor = runExportAnnotationsAsDocuments
    //Ok(views.html.dashboard.exporter.render(actor))
    Ok("Exportation complete!")
  }

  class ConfigMessage(val str:String)

  def runExportAnnotationsAsDocuments:String = {
    val myActor = Akka.system.actorOf(Props[MyGeneratorMaster])
    myActor ! new ConfigMessage("blarg message")
    myActor.path.name
  }

  def status(name :String) = Action {
    val myActor = Akka.system.actorSelection("akka://application/user/"+name)
    val future = (myActor ask new ProgressStatus(0))
    var result = 0
    future onComplete {
      case ps:ProgressStatus => {Logger.info(ps.toString);result=ps.state}
      case _ => result = -1
    }
    Ok(Json.toJson(result))
  }

  class ProgressStatus(val state: Int)
  class ProgressUpdate(val state: Int)

  class MyGeneratorMaster extends UntypedActor {

   var completed = 0

   override def onReceive(message: Any):Unit = message match {
      case m: ConfigMessage => {
      }
      case m: ProgressStatus => {
        completed = completed + 1
        Logger.info(completed+"")
        getSender ! (new ProgressStatus(completed),getSelf)

        if(completed == 100)
          this.getSelf ! PoisonPill
      }
      case m: ProgressUpdate => {
      }
    }
  }

}