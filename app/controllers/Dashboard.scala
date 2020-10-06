package controllers

import com.avaje.ebean.Ebean
import controllers.searchEngine.Engine
import controllers.semanticVectors.RandomIndexSearch
import models._
import play.api.Logger
import play.api.Play.current
import play.api.libs.concurrent.Akka
import play.api.mvc._


/**
 * Created by Aldo on 04/08/14.
 */
object Dashboard extends Controller with Secured{

  val stopWords = "and the in on an a at is are that of isn aren be".split(" ").toSet

  def initializeDatabase = {
    //List("None", "Garbage", "Skip", "Challenge", "Campaign", "Measure", "Event", "Experiment", "Task", "Run", "Score", "TestCollection", "Collection", "Topic", "Topics", "Groundtruth")
    List("None", "Garbage", "Skip", "Challenge", "Measure", "Task", "Run", "TestCollection", "Collection")
      .filter(Category.findByID(_)==null).map(cat => {
      Logger.info("initializeDatabase: initializing category " + cat)
      val category = new Category
      category.id = cat
      category.save})

    List(
      "Ad-Hoc", "Robust", "Web", "Math", "track",
      "task", "subtask",
      "MAP", "Precision", "Recall",
      ".GOV", "TIPSTER", "TREC-8", "WT2g", "WT10g", "ClueWeb09", "ClueWeb12")
      .map(Engine.replaceChars(_))
      .filter(Seed.findByID(_)==null).map(s => {
      Logger.info("initializeDatabase: initializing seed " + s)
      generateSeedAndEmbryon(s)
    })
  }

  def logTime[A](name:String, a: => A) = {
       val now = System.nanoTime
       val result = a
       val millis = (System.nanoTime - now).toFloat / 1000000
       Logger.info(name + " performed in %f ms".format(millis))
       result}

  def betweenTransaction[A](a: => A) = {
    val t = Ebean.beginTransaction()
    t.setBatchMode(true)
    t.setBatchSize(1)
    try {
      a
      Ebean.commitTransaction()
    } finally {
      Ebean.endTransaction()
    }
  }

  def saveAllInBatch[A](a: => A) = betweenTransaction(a)


  def unreplaceString(str: String) = {
    def unreplaceNumber(str:String) = {
      val number = Array("bbZerobb", "bbOnebb", "bbTwobb", "bbThreebb", "bbFourbb", "bbFivebb", "bbSixbb", "bbSevenbb", "bbEightbb", "bbNinebb")
      val str2num = number.zip(0 to 9)
      str2num.foldLeft(str)((s, number) => s.replaceAll(number._1, number._2.toString))
    }
    unreplaceNumber(str.replaceAll("bbATbb","@").replaceAll("bbPOINTbb",".").replaceAll("bbHYPHENbb","-"))
  }

    def indexEmbryon(idEmbryon: Int) = withAuth { username  => implicit request =>
    Logger.info(username + " - indexEmbryon: " + idEmbryon)
    val user = User.findByUsername(username)
    //val classTermN = Annotation.getTermsWithClass(user.id).groupBy(_._1)
    //val termClass = classTermN.mapValues(_.map(t => unreplaceString(t._2)))
    val termNClass = Annotation.getNClassPerUser(user.id).toMap//classTermN.mapValues(_.foldLeft(0)((acc, e) => acc + e._3))
    val embryon = Embryon.findByID(idEmbryon)
    val engine = new Engine
    val doc = engine.getDocumentByID(embryon.idEmbryon)
    Ok(
      views.html.dashboard.index.render(
        user,
        unreplaceString(embryon.seed.id),
        embryon,
        doc,
        doc.getSnippet,
        Category.getAll,
        Annotation.getLastGarbagedTerms(user.id, 100).map(unreplaceString(_)),
        models.Annotation.getGarbageTerms(user.id).map(unreplaceString(_)),
        null,//termClass,
        termNClass))}

  def index = withAuth { username  => implicit request =>
    val user = User.findByUsername(username)
    val annotation = logTime("getRandomEmbryon", getRandomAnnotation(username))
    (if(annotation == null){ // check if embryons has to be initialized
      if(Embryon.getAll.isEmpty){
        initializeDatabase
        if(Embryon.getAll.nonEmpty){
          //val classTermN = Annotation.getTermsWithClass(user.id).groupBy(_._1)
          //val termClass = classTermN.mapValues(_.map(t => unreplaceString(t._2)))
          val termNClass = Annotation.getNClassPerUser(user.id).toMap//classTermN.mapValues(_.foldLeft(0)((acc, e) => acc + e._3))
          val annotation = getRandomAnnotation(username)
          val engine = new Engine
          val doc = engine.getDocumentByID(annotation.embryon.idEmbryon)
          Ok(
            views.html.dashboard.index.render(
              user,
              unreplaceString(annotation.seed.id),
              annotation.embryon,
              doc,
              doc.getSnippet,
              Category.getAll,
              Annotation.getLastGarbagedTerms(user.id, 100).map(unreplaceString(_)),
              models.Annotation.getGarbageTerms(user.id).map(unreplaceString(_)), null,
              //termClass,
              termNClass
              ))
        }else {
          Ok(views.html.dashboard.message.render(user,
            "No seeds present!"))
        }
      }else{
        Ok(views.html.dashboard.message.render(user,
          "Congratulations, you have done everything could be possibly done!\n Come later maybe you will find others!"))
      }
    }else{
      Redirect(routes.Dashboard.indexEmbryon(annotation.embryon.id))
    })
  }

  def stats = withAuth { username => implicit request =>
    val user = User.findByUsername(username)
    Ok(views.html.dashboard.stats.render(user))
  }

  def deleteGarbagedTerm(idSeed: String) = withAuth { username  => implicit request =>
    Logger.info(username + " - deleteGarbagedTerm: " + idSeed)
    val user = User.findByUsername(username)
    Annotation.deleteGarbagedTerm(user.id, Engine.replaceChars(idSeed))
    back(request)}

  def generateEmbryons(seed: Seed) = if(!stopWords.contains(seed.id)){
    val engine = new Engine;
    val docs = logTime("generateEmbryons - engine.search(" + seed.id + ")", engine.search(seed.id))
    Logger.info("generateEmbryons " + docs.size + " from seed: " + seed.id)
    logTime("generateEmbryons - save all the " + docs.size + " embryons",
      (saveAllInBatch(
          docs.filter(doc => Embryon.findByID(doc.id,seed.id)==null).map(doc => {
            val embryon = new Embryon
            embryon.idEmbryon = doc.id
            embryon.seed = seed
            try {
              embryon.save
            } catch {
              case e: Exception => /*Logger.info("generateEmbryons - Embryon " + doc.id + " with seed " + seed.id + " already exists");*/ Logger.error(e.getMessage); null
            }
          }))
       )
      )}

  def getRandomAnnotation(username: String): Annotation = {
    val user = (User.findByUsername(username))
    Embryon.getRandomAnnotation(user.id)
  }

  def generateSeedAndEmbryon(str:String) = if(!str.matches("^\\d+\\.\\d+$") && !stopWords.contains(str)) {
    val ris = new RandomIndexSearch
    val terms = ris.search(str).filter(str => !str.matches("^\\d+\\.\\d+$") && !stopWords.contains(str) && Seed.findByID(str)==null).map(ns =>{
      val seed = new Seed
      seed.id = ns
      try {
        seed.save
        logTime("generateEmbryons", generateEmbryons(seed))
      }catch{ case e:Exception => Logger.info("generateSeedAndEmbryon - Term " + seed.id + " already exists")}
      ns
    }).filter(_ != null)
    if(terms.nonEmpty) log("generateSeedAndEmbryon", str + " -> " + terms.mkString(","))
  }

  def isCategoryDisabled(user: User, cat: Category, idSeed:String):Boolean={
    if(cat.id=="Garbage")
      return Annotation.isGarbageDisabled(user.id, idSeed)
    false
  }

  def refillAnnotationsPools = withAuth { username  => implicit request =>
    User.getAll.map(u => Embryon.generateAnnotationFromRandomEmbryon(u.id))
    Redirect(routes.Dashboard.index)
  }



  def annotate(idSeed:String, idEmbryon: String, idCat: String) = withAuth{ username => implicit request =>
    val user = User.findByUsername(username)
    if(idCat != "Skip"){
      try{
        val annotation = Annotation.findByID(idSeed, Embryon.findByID(idEmbryon, idSeed).id, user.id)
        annotation.category = Category.findByID(idCat)
        annotation.update

        import play.api.libs.concurrent.Execution.Implicits._

        import scala.concurrent.duration._
        if(idCat != "Garbage" && idCat != "None")
          Akka.system.scheduler.scheduleOnce(0.seconds){
            generateSeedAndEmbryon(idSeed)
          }
      }catch{
        case e =>
        val annotation = Annotation.findByID(idSeed, Embryon.findByID(idEmbryon, idSeed).id, user.id)
        annotation.category = Category.findByID(idCat)
        annotation.update
      }
    }else {
      log("annotate", user.id + " - " + "Skip" + " - Seed: " + idSeed + " Embryon: " + idEmbryon)
    }
    Redirect(routes.Dashboard.index)
  }

  def forceEmbryonsGeneration = withAuth { username  => implicit request =>
    Seed.getAll.map(generateEmbryons(_))
    Redirect(routes.Dashboard.index)
  }

  def forceSeedsGeneration = withAuth { username  => implicit request =>
    Seed.getAllAnnotated.map(generateSeedAndEmbryon(_))
    Redirect(routes.Dashboard.index)
  }

  def back(request: Request[AnyContent]) =     {
    //println(request.headers.get("referer").get)
    Redirect(request.headers.get("referer").get)
  }

  def log(id:String, text: String) = {
    val log = new Log
    log.text = id + " - " + text
    log.save
  }

  def computeRay(terms: Array[String], term: String): Int =
      if(terms.filter(_.contains(term)).nonEmpty) terms.filter(_.contains(term)).sortBy(_.length).head.length - term.length else 0
}


