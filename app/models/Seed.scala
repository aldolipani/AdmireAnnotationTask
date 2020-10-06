package models

import javax.persistence._
import java.util.Date
import java.sql.Timestamp
import com.avaje.ebean.Ebean
import models.utils.Hash
import play.db.ebean.Model
import scala.collection.JavaConversions._

/**
 * Created by Aldo on 04/08/14.
 */
object Seed {
  def getAll: List[Seed] = find.all.toList

  def findByID(id: String): Seed = find.byId(id)

  def getAllOrderedByDateCreation: List[String] = {
    val sqlTrashTerms = Ebean.createSqlQuery("SELECT * FROM seed ORDER BY date_creation")
    val sqlResult = sqlTrashTerms.findList.toList
    for(r <- sqlResult) yield {
      r.getString("id")
    }
  }

  def getAllAnnotated: List[String] = {
    val sqlTrashTerms = Ebean.createSqlQuery("SELECT idSeed as id FROM annotation WHERE idCategory IS NOT NULL AND idCategory!=\"Garbage\" AND idCategory!=\"None\" GROUP BY idSeed")
    val sqlResult = sqlTrashTerms.findList.toList
    for(r <- sqlResult) yield {
      r.getString("id")
    }
  }

  def getAllToOneOrderedByDateCreation(i: Int): List[String]= {
    val sqlTrashTerms = Ebean.createSqlQuery("SELECT * FROM seed ORDER BY date_creation LIMIT :limit")
    sqlTrashTerms.setParameter("limit",i+1)
    val sqlResult = sqlTrashTerms.findList.toList
    for(r <- sqlResult) yield {
      r.getString("id")
    }
  }

  def getAllGarbagedByAtLeastTwoUser: List[String] = {
    val sqlTrashTerms = Ebean.createSqlQuery("SELECT idSeed, Count(*) as n FROM annotation WHERE idCategory=\"Garbage\" GROUP BY idSeed HAVING n>1")
    val sqlResult = sqlTrashTerms.findList.toList
    for(r <- sqlResult) yield {
      r.getString("idSeed")
    }
  }

  def getAllAnnotatedByAtLeastTwoUser: List[String] = {
    val sqlTrashTerms = Ebean.createSqlQuery("SELECT idSeed, Count(*) as n FROM annotation WHERE idCategory!=\"Garbage\" AND idCategory!=\"None\" AND idCategory IS NOT NULL GROUP BY idSeed HAVING n>1")
    val sqlResult = sqlTrashTerms.findList.toList
    for(r <- sqlResult) yield {
      r.getString("idSeed")
    }
  }

  var find: Model.Finder[String, Seed] = new Model.Finder[String, Seed](classOf[String], classOf[Seed])
}

@Entity class Seed extends Model {
  @Id var id: String = ""
  @Column(nullable = false, columnDefinition="TIMESTAMP DEFAULT CURRENT_TIMESTAMP") var dateCreation: Timestamp = null
}
