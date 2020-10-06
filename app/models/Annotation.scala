package models

import javax.persistence._
import java.sql.Timestamp
import play.db.ebean.Model
import scala.collection.JavaConversions._
import com.avaje.ebean.Ebean

/**
 * Created by Aldo on 04/08/14.
 */
object Annotation {
  def findByID(id: Int): Annotation = find.byId(id)

  def findByID(idSeed:String, idEmbryon:Int, idUser:Long) = find.where.eq("idSeed", idSeed).eq("idEmbryon", idEmbryon).eq("idUser", idUser).findUnique


  def getLastGarbagedTerms(idUser: Long, n: Int): List[String] = {
    val sqlTrashTerms = Ebean.createSqlQuery("select idSeed, date_creation from annotation where idUser=:idUser and idCategory=\"Garbage\" order by  date_creation desc limit :n")
    sqlTrashTerms.setParameter("idUser", idUser)
    sqlTrashTerms.setParameter("n", n)
    val sqlResult = sqlTrashTerms.findList.toList
    for(r <- sqlResult) yield {
      r.getString("idSeed")
    }
  }

  def isGarbageDisabled(idUser: Long, idSeed:String):Boolean = {
    val sqlTrashTerms = Ebean.createSqlQuery("SELECT idSeed, idCategory from annotation where idUser=:idUser and idCategory!=\"None\" and idSeed=:idSeed")
    sqlTrashTerms.setParameter("idUser", idUser)
    sqlTrashTerms.setParameter("idSeed", idSeed)
    val sqlResult = sqlTrashTerms.findList.toList
    sqlResult.size > 0
  }

  def deleteGarbagedTerm(idUser: Long, idSeed: String) =
    find.where.eq("idUser", idUser).eq("idSeed", idSeed).findList.map(e => {println(e.id); Ebean.delete(e)})

  def getAnnotationByUserID(idUser: Long): List[Annotation] = find.where.eq("idUser", idUser).findList.toList

  def getGarbageTerms(idUser: Long): List[String] = {
    val sqlTrashTerms = Ebean.createSqlQuery("(SELECT idSeed FROM annotation WHERE idUser=:idUser AND idCategory=\"Garbage\") UNION (SELECT idSeed FROM (SELECT idSeed, Count(*) as n FROM annotation WHERE idCategory=\"Garbage\" GROUP BY idSeed HAVING n>1) as gw2)")
    sqlTrashTerms.setParameter("idUser", idUser)
    val sqlResult = sqlTrashTerms.findList.toList
    for(r <- sqlResult) yield {
      r.getString("idSeed")
    }
  }

  def getUserGarbageTerms(idUser: Long): List[String] = {
    val sqlTrashTerms = Ebean.createSqlQuery("SELECT idSeed FROM annotation WHERE idUser=:idUser AND idCategory=\"Garbage\"")
    sqlTrashTerms.setParameter("idUser", idUser)
    val sqlResult = sqlTrashTerms.findList.toList
    for(r <- sqlResult) yield {
      r.getString("idSeed")
    }
  }

  def getTermsWithClass(idUser: Long): Set[(String, String, Int)] = {
    val sqlTrashTerms = Ebean.createSqlQuery("SELECT idSeed, idCategory, Count(idSeed) as count FROM annotation WHERE idCategory IS NOT NULL AND idUser=:idUser GROUP BY idSeed, idCategory")
    sqlTrashTerms.setParameter("idUser", idUser)
    val sqlResult = sqlTrashTerms.findList.toSet
    for(r <- sqlResult) yield {
      (r.getString("idCategory"), r.getString("idSeed"), r.getInteger("count").toInt)
    }
  }

  def getNClassPerUser(idUser: Long): Set[(String, Int)] = {
    val sqlTrashTerms = Ebean.createSqlQuery("SELECT idCategory, COUNT(*) as n FROM annotation WHERE idCategory IS NOT NULL AND idUser=:idUser GROUP BY idCategory")
    sqlTrashTerms.setParameter("idUser", idUser)
    val sqlResult = sqlTrashTerms.findList.toSet
    for(r <- sqlResult) yield {
      (r.getString("idCategory"), r.getInteger("n").toInt)
    }
  }


  def numberOfGarbagedWords(idUser:Long): Int = {
    val sqlTrashTerms = Ebean.createSqlQuery("SELECT COUNT(*) as n FROM (SELECT idSeed FROM annotation WHERE idUser=:idUser AND idCategory=\"Garbage\" GROUP BY idSeed) as a")
    sqlTrashTerms.setParameter("idUser", idUser)
    val sqlResult = sqlTrashTerms.findUnique
    sqlResult.getString("n").toInt
  }

  def numberOfAnnotation(idUser:Long): Int = {
    val sqlTrashTerms = Ebean.createSqlQuery("SELECT COUNT(*) as n FROM annotation WHERE idCategory IS NOT NULL AND idUser=:idUser")
    sqlTrashTerms.setParameter("idUser", idUser)
    val sqlResult = sqlTrashTerms.findUnique
    sqlResult.getString("n").toInt
  }

  def userAgreementOnGarbagedWordsIntersection(idUser1: Long, idUser2: Long) = {
    val sqlTrashTerms = Ebean.createSqlQuery("SELECT COUNT(*) as n FROM (SELECT idSeed FROM annotation WHERE idUser=:idUser1 AND idCategory=\"Garbage\" GROUP BY idSeed HAVING idSeed IN (SELECT idSeed FROM annotation WHERE idUser=:idUser2 AND idCategory=\"Garbage\" GROUP BY idSeed)) as c")
    sqlTrashTerms.setParameter("idUser1", idUser1)
    sqlTrashTerms.setParameter("idUser2", idUser2)
    val sqlResult = sqlTrashTerms.findUnique
    sqlResult.getString("n").toInt
  }

  def userAgreementOnIntersection(idCat:String, idUser1: Long, idUser2: Long) = {
    val sqlTrashTerms = if(idCat == "CollectionTestCollection")
      Ebean.createSqlQuery(
        "SELECT COUNT(*) as n FROM (SELECT idEmbryon FROM annotation WHERE idUser=:idUser1 AND (idCategory=\"Collection\" OR idCategory=\"TestCollection\") AND idEmbryon IN (SELECT idEmbryon FROM annotation WHERE idUser=:idUser2 AND (idCategory=\"Collection\" OR idCategory=\"TestCollection\"))) as c")
    else if(idCat == "ChallengeTask")
      Ebean.createSqlQuery(
        "SELECT COUNT(*) as n FROM (SELECT idEmbryon FROM annotation WHERE idUser=:idUser1 AND (idCategory=\"Challenge\" OR idCategory=\"Task\") AND idEmbryon IN (SELECT idEmbryon FROM annotation WHERE idUser=:idUser2 AND (idCategory=\"Challenge\" OR idCategory=\"Task\"))) as c")
    else {
      Ebean.createSqlQuery(
        "SELECT COUNT(*) as n FROM (SELECT idEmbryon FROM annotation WHERE idUser=:idUser1 AND idCategory=:idCat AND idEmbryon IN (SELECT idEmbryon FROM annotation WHERE idUser=:idUser2 AND idCategory=:idCat)) as c")
    }

    if(idCat != "CollectionTestCollection" && idCat != "ChallengeTask")
      sqlTrashTerms.setParameter("idCat", idCat)

    sqlTrashTerms.setParameter("idUser1", idUser1)
    sqlTrashTerms.setParameter("idUser2", idUser2)
    val sqlResult = sqlTrashTerms.findUnique
    sqlResult.getString("n").toInt
  }

  def userAgreementOnUnion(idCat:String, idUser1: Long, idUser2: Long) = {
    val sqlTrashTerms = if(idCat == "CollectionTestCollection")
      Ebean.createSqlQuery(
        "SELECT COUNT(*) as n FROM (" +
          "(SELECT a1.idEmbryon, idCat1, idCat2 FROM " +
          "(SELECT idEmbryon, idCategory as idCat1 FROM annotation WHERE idUser=:idUser1 AND (idCategory=\"Collection\" OR idCategory=\"TestCollection\")) as a1 " +
          "LEFT JOIN " +
          "(SELECT idEmbryon, idCategory as idCat2 FROM (" +
          "(SELECT id, idSeed, idEmbryon, idUser, idCategory, date_creation FROM annotation WHERE idUser=:idUser2 AND idCategory IS NOT NULL AND idCategory!='Garbage' )" +
          "UNION " +
          "(SELECT id, idSeed, idEmbryon, :idUser2 as idUser, \"None\" as idCategory, date_creation FROM annotation WHERE idUser=:idUser2 AND idCategory IS NOT NULL AND idCategory!=\"Garbage\" AND idSeed IN (SELECT idSeed FROM annotation WHERE idUser=:idUser1 AND idCategory=\"Garbage\"))" +
          ") as aa2 " +
          "WHERE idUser=:idUser2) as a2 " +
          "ON a1.idEmbryon=a2.idEmbryon" +
          ") " +
          "UNION " +
          "(SELECT b2.idEmbryon, idCat1, idCat2 FROM " +
          "(SELECT idEmbryon, idCategory as idCat1 FROM (" +
          "(SELECT id, idSeed, idEmbryon, idUser, idCategory, date_creation FROM annotation WHERE idUser=:idUser1 AND idCategory IS NOT NULL AND idCategory!='Garbage' )" +
          "UNION " +
          "(SELECT id, idSeed, idEmbryon, :idUser1 as idUser, \"None\" as idCategory, date_creation FROM annotation WHERE idUser=:idUser1 AND idCategory IS NOT NULL AND idCategory!=\"Garbage\" AND idSeed IN (SELECT idSeed FROM annotation WHERE idUser=:idUser2 AND idCategory=\"Garbage\"))" +
          ") as bb2 " +
          "WHERE idUser=:idUser1) as b1 " +
          "RIGHT JOIN " +
          "(SELECT idEmbryon, idCategory as idCat2 FROM annotation WHERE idUser=:idUser2 AND (idCategory=\"Collection\" OR idCategory=\"TestCollection\")) as b2 " +
          "ON b1.idEmbryon=b2.idEmbryon" +
          ")" +
          ") as u " +
          "WHERE idCat1 IS NOT NULL AND idCat2 IS NOT NULL")
    else if(idCat == "ChallengeTask")
      Ebean.createSqlQuery(
        "SELECT COUNT(*) as n FROM (" +
          "(SELECT a1.idEmbryon, idCat1, idCat2 FROM " +
          "(SELECT idEmbryon, idCategory as idCat1 FROM annotation WHERE idUser=:idUser1 AND (idCategory=\"Challenge\" OR idCategory=\"Task\")) as a1 " +
          "LEFT JOIN " +
          "(SELECT idEmbryon, idCategory as idCat2 FROM (" +
          "(SELECT id, idSeed, idEmbryon, idUser, idCategory, date_creation FROM annotation WHERE idUser=:idUser2 AND idCategory IS NOT NULL AND idCategory!='Garbage' )" +
          "UNION " +
          "(SELECT id, idSeed, idEmbryon, :idUser2 as idUser, \"None\" as idCategory, date_creation FROM annotation WHERE idUser=:idUser2 AND idCategory IS NOT NULL AND idCategory!=\"Garbage\" AND idSeed IN (SELECT idSeed FROM annotation WHERE idUser=:idUser1 AND idCategory=\"Garbage\"))" +
          ") as aa2 " +
          "WHERE idUser=:idUser2) as a2 " +
          "ON a1.idEmbryon=a2.idEmbryon" +
          ") " +
          "UNION " +
          "(SELECT b2.idEmbryon, idCat1, idCat2 FROM " +
          "(SELECT idEmbryon, idCategory as idCat1 FROM (" +
          "(SELECT id, idSeed, idEmbryon, idUser, idCategory, date_creation FROM annotation WHERE idUser=:idUser1 AND idCategory IS NOT NULL AND idCategory!='Garbage' )" +
          "UNION " +
          "(SELECT id, idSeed, idEmbryon, :idUser1 as idUser, \"None\" as idCategory, date_creation FROM annotation WHERE idUser=:idUser1 AND idCategory IS NOT NULL AND idCategory!=\"Garbage\" AND idSeed IN (SELECT idSeed FROM annotation WHERE idUser=:idUser2 AND idCategory=\"Garbage\"))" +
          ") as bb2 " +
          "WHERE idUser=:idUser1) as b1 " +
          "RIGHT JOIN " +
          "(SELECT idEmbryon, idCategory as idCat2 FROM annotation WHERE idUser=:idUser2 AND (idCategory=\"Challenge\" OR idCategory=\"Task\")) as b2 " +
          "ON b1.idEmbryon=b2.idEmbryon" +
          ")" +
          ") as u " +
          "WHERE idCat1 IS NOT NULL AND idCat2 IS NOT NULL")
    else {
      Ebean.createSqlQuery(
        "SELECT COUNT(*) as n FROM (" +
          "(SELECT a1.idEmbryon, idCat1, idCat2 FROM " +
          "(SELECT idEmbryon, idCategory as idCat1 FROM annotation WHERE idUser=:idUser1 AND idCategory=:idCat) as a1 " +
          "LEFT JOIN " +
          "(SELECT idEmbryon, idCategory as idCat2 FROM (" +
          "(SELECT id, idSeed, idEmbryon, idUser, idCategory, date_creation FROM annotation WHERE idUser=:idUser2 AND idCategory IS NOT NULL AND idCategory!='Garbage' )" +
          "UNION " +
          "(SELECT id, idSeed, idEmbryon, :idUser2 as idUser, \"None\" as idCategory, date_creation FROM annotation WHERE idUser=:idUser2 AND idCategory IS NOT NULL AND idCategory!=\"Garbage\" AND idSeed IN (SELECT idSeed FROM annotation WHERE idUser=:idUser1 AND idCategory=\"Garbage\"))" +
          ") as aa2 " +
          "WHERE idUser=:idUser2) as a2 " +
          "ON a1.idEmbryon=a2.idEmbryon" +
          ") " +
          "UNION " +
          "(SELECT b2.idEmbryon, idCat1, idCat2 FROM " +
          "(SELECT idEmbryon, idCategory as idCat1 FROM (" +
          "(SELECT id, idSeed, idEmbryon, idUser, idCategory, date_creation FROM annotation WHERE idUser=:idUser1 AND idCategory IS NOT NULL AND idCategory!='Garbage' )" +
          "UNION " +
          "(SELECT id, idSeed, idEmbryon, :idUser1 as idUser, \"None\" as idCategory, date_creation FROM annotation WHERE idUser=:idUser1 AND idCategory IS NOT NULL AND idCategory!=\"Garbage\" AND idSeed IN (SELECT idSeed FROM annotation WHERE idUser=:idUser2 AND idCategory=\"Garbage\"))" +
          ") as bb2 " +
          "WHERE idUser=:idUser1) as b1 " +
          "RIGHT JOIN " +
          "(SELECT idEmbryon, idCategory as idCat2 FROM annotation WHERE idUser=:idUser2 AND idCategory=:idCat) as b2 " +
          "ON b1.idEmbryon=b2.idEmbryon" +
          ")" +
          ") as u " +
          "WHERE idCat1 IS NOT NULL AND idCat2 IS NOT NULL")
    }

    if(idCat != "CollectionTestCollection" && idCat != "ChallengeTask")
      sqlTrashTerms.setParameter("idCat", idCat)

    sqlTrashTerms.setParameter("idUser1", idUser1)
    sqlTrashTerms.setParameter("idUser2", idUser2)
    val sqlResult = sqlTrashTerms.findUnique
    sqlResult.getString("n").toInt
  }

  def userAgreementOnGarbagedWordsUnion(idUser1: Long, idUser2: Long) = {
    val sqlTrashTerms = Ebean.createSqlQuery("SELECT COUNT(*) as n FROM (SELECT idSeed FROM annotation WHERE idUser=:idUser1 AND idCategory=\"Garbage\" GROUP BY idSeed HAVING idSeed IN (SELECT idSeed FROM annotation WHERE idUser=:idUser2 AND idCategory=\"Garbage\" GROUP BY idSeed)) as c")
    sqlTrashTerms.setParameter("idUser1", idUser1)
    sqlTrashTerms.setParameter("idUser2", idUser2)
    val sqlResult = sqlTrashTerms.findUnique
    sqlResult.getString("n").toInt
  }

  def pairwiseUsersGarbagedWords(idUser1: Long, idUser2: Long): List[String] = {
    val sqlTrashTerms = Ebean.createSqlQuery("SELECT idSeed FROM (SELECT idSeed, Count(*) as n FROM annotation WHERE (idUser=:idUser1 OR idUser=:idUser2) AND idCategory=\"Garbage\" GROUP BY idSeed HAVING n>1) as gw2")
    sqlTrashTerms.setParameter("idUser1", idUser1)
    sqlTrashTerms.setParameter("idUser2", idUser2)
    val sqlResult = sqlTrashTerms.findList.toList
    for(r <- sqlResult) yield {
      r.getString("idSeed")
    }
  }

  def getAnnotatedEmbryonXInOrder(offset: Long): Int = {
    val sql = Ebean.createSqlQuery("SELECT id FROM embryon WHERE id IN (SELECT idEmbryon as id FROM annotation WHERE idCategory IS NOT NULL) ORDER BY id_embryon LIMIT 1 OFFSET :offset")
    sql.setParameter("offset", offset)
    val sqlResult = sql.findUnique
    sqlResult.getString("id").toInt
  }

  def getSizeAnnotatedEmbryonXInOrder: Int = {
    val sql = Ebean.createSqlQuery("SELECT COUNT(*) as n FROM embryon WHERE id IN (SELECT idEmbryon as id FROM annotation WHERE idCategory IS NOT NULL)")
    val sqlResult = sql.findUnique
    sqlResult.getString("n").toInt
  }

  def getAnnotationsBy(idEmbryon: Int, idSeed:String): List[Int] = {
    val sqlTrashTerms = Ebean.createSqlQuery("SELECT * FROM annotation WHERE idEmbryon=:idEmbryon AND idSeed=:idSeed AND idCategory IS NOT NULL")
    sqlTrashTerms.setParameter("idEmbryon", idEmbryon)
    sqlTrashTerms.setParameter("idSeed", idSeed)
    val sqlResult = sqlTrashTerms.findList.toList
    for(r <- sqlResult) yield {
      r.getString("id").toInt
    }
  }

  def numberTermsPerClass(idCategory: String): Int = {
    val sqlTrashTerms = Ebean.createSqlQuery("SELECT COUNT(DISTINCT idSeed) as n FROM annotation WHERE idCategory=:idCat")
    sqlTrashTerms.setParameter("idCat", idCategory)
    val sqlResult = sqlTrashTerms.findUnique()
    sqlResult.getString("n").toInt
  }

  var find: Model.Finder[Int, Annotation] = new Model.Finder[Int, Annotation](classOf[Int], classOf[Annotation])
}

@Table(uniqueConstraints = Array(new UniqueConstraint(columnNames = Array("idSeed", "idEmbryon", "idUser")))) @Entity class Annotation  extends Model {
  @Id var id: Int = 0
  @OneToOne @JoinColumn(name="idSeed", referencedColumnName = "id") var seed: Seed = null
  @OneToOne @JoinColumn(name="idEmbryon", referencedColumnName = "id") var embryon: Embryon = null
  @OneToOne @JoinColumn(name="idUser", referencedColumnName = "id") var user: User = null
  @OneToOne @JoinColumn(name="idCategory", referencedColumnName = "id") var category: Category = null
  @Column(nullable = false, columnDefinition="TIMESTAMP DEFAULT CURRENT_TIMESTAMP") var dateCreation: Timestamp = null
}
