package controllers

import com.avaje.ebean.Ebean
import controllers.searchEngine.Engine
import controllers.semanticVectors.RandomIndexSearch
import models._
import play.api.Logger
import play.api.Play.current
import play.api.libs.concurrent.Akka
import play.api.mvc._
import play.api.libs.json._
import play.api.libs.functional.syntax._


/**
 * Created by Aldo on 04/08/14.
 */
object Stats extends Controller with Secured{

  case class UserInfo(username: String, annotations:Int = 0, garbagedWords:Int= 0, termNClass: Map[String, Int])

  implicit val userInfoWrites: Writes[UserInfo] = (
      (JsPath \ "username").write[String] and
      (JsPath \ "annotations").write[Int] and
      (JsPath \ "garbagedWords").write[Int] and
      (JsPath \ "termNClass").write[Map[String, Int]]
    )(unlift(UserInfo.unapply))

  def listUsers = Action {
    val users = User.getAll
    val userInfo = users.map(user =>{
      val classTermN = Annotation.getTermsWithClass(user.id).groupBy(_._1)
      val termNClass = classTermN.mapValues(_.foldLeft(0)((acc, e) => acc + e._3))
      val termNAllClass = Category.getAll.filter(_.id!="Skip").sortBy(_.id).map(cat => (cat.id, termNClass.getOrElse(cat.id, 0))).toMap
      new UserInfo(user.email, Annotation.numberOfAnnotation(user.id), Annotation.numberOfGarbagedWords(user.id), termNAllClass)
    })
    val json = Json.toJson(userInfo)
    Ok(json)
  }

  case class UserAgreement(username1: String, username2: String, val inter: Int, val union: Int)

  implicit val userAgreementWrites: Writes[UserAgreement] = (
    (JsPath \ "username1").write[String] and
      (JsPath \ "username2").write[String] and
      (JsPath \ "inter").write[Int] and
      (JsPath \ "union").write[Int]
    )(unlift(UserAgreement.unapply))

  def userAgreementOnGarbagedWords = Action {
    val users = User.getAll
    val userAgreement = for(a <- users; b<- users if(a.id!=b.id && a.id>b.id)) yield {
      new UserAgreement(a.email, b.email, Annotation.userAgreementOnGarbagedWordsIntersection(a.id, b.id), Annotation.userAgreementOnGarbagedWordsUnion(a.id, b.id))
    }
    val json = Json.toJson(userAgreement)
    Ok(json)
  }

  case class UserWords(username: String, size: Int, words: String)

  implicit val userWordsWrites: Writes[UserWords] = (
    (JsPath \ "username").write[String] and
    (JsPath \ "size").write[Int] and
    (JsPath \ "words").write[String]
    )(unlift(UserWords.unapply))

  def usersGarbagedWords = Action {
    val users = User.getAll
    val userGarbagedWords = users.map(u => {
      val garbagedWords = Annotation.getUserGarbageTerms(u.id)
      new UserWords(u.email,garbagedWords.size, garbagedWords.mkString(" "))
    })
    val json = Json.toJson(userGarbagedWords)
    Ok(json)
  }

  case class PairwiseUsersWords(username1: String, username2: String, size: Int, words: String)

  implicit val pairwiseUserWordsWrites: Writes[PairwiseUsersWords] = (
    (JsPath \ "username1").write[String] and
    (JsPath \ "username2").write[String] and
    (JsPath \ "size").write[Int] and
    (JsPath \ "words").write[String]
    )(unlift(PairwiseUsersWords.unapply))

  def pairwiseUsersGarbagedWords = Action {
    val users = User.getAll
    val pairwiseUserGarbagedWords = for(a <- users; b<- users if(a.id!=b.id && a.id>b.id)) yield {
      val pairwiseGarbagedWords = Annotation.pairwiseUsersGarbagedWords(a.id, b.id).sorted
      new PairwiseUsersWords(a.email, b.email, pairwiseGarbagedWords.size, pairwiseGarbagedWords.mkString(" "))
    }
    val json = Json.toJson(pairwiseUserGarbagedWords)
    Ok(json)
  }

  def userPairs = Action{
    val users = User.getAll
    val userPairs = (for(a <- users; b<- users if(a.id!=b.id && a.id>b.id)) yield { 1 }).sum
    val json = Json.toJson(userPairs)
    Ok(json)
  }

  def userAgreementOn(idCat: String, idPair: Int) = Action {
    val users = User.getAll
    val userPair = (for(a <- users; b<- users if(a.id!=b.id && a.id>b.id)) yield {(a,b)}).toList(idPair-1)
    val userAgreement = new UserAgreement(userPair._1.email, userPair._2.email, Annotation.userAgreementOnIntersection(idCat, userPair._1.id, userPair._2.id), Annotation.userAgreementOnUnion(idCat, userPair._1.id, userPair._2.id))
    val json = Json.toJson(userAgreement)
    Ok(json)
  }

  case class ThingWithSize(name: String, val n: Int)

  implicit val ClassSizeWrites: Writes[ThingWithSize] = (
    (JsPath \ "name").write[String] and
    (JsPath \ "n").write[Int]
    )(unlift(ThingWithSize.unapply))

  // Random Indexing Part
  def numberTermsPerClass = Action {
    val classes = Category.getAll
    val riClassAgreement = for(c <- classes if(!(c.id=="None" || c.id=="Skip"))) yield {
      new ThingWithSize(c.id, Annotation.numberTermsPerClass(c.id))
    }
    val json = Json.toJson(riClassAgreement)
    Ok(json)
  }

  case class Point(x: Double, y: Double)

  implicit val PointWrites: Writes[Point] = (
    (JsPath \ "x").write[Double] and
    (JsPath \ "y").write[Double]
    )(unlift(Point.unapply))


  def riNewWordsFunction(i: Int) = Action {
    val ri = new RandomIndexSearch
    val seeds = Seed.getAllToOneOrderedByDateCreation(i)
    val seed = seeds.last
    val pSeeds = seeds.take(i)
    val sSeeds = Seed.getAllAnnotatedByAtLeastTwoUser
    val nSeeds = ri.search(seed)
    val p = new Point(i, nSeeds.filter(s => sSeeds.contains(s)).filter(s => !pSeeds.contains(s)).size)
    val json = Json.toJson(p)
    Ok(json)
  }

  def riNewWordsFunctionSize = Action {
    val seeds = Seed.getAllOrderedByDateCreation
    val json = Json.toJson(seeds.size)
    Ok(json)
  }

}


