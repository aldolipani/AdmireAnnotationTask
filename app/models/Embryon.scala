package models

import javax.persistence._
import java.sql.Timestamp
import play.api.Logger
import play.api.libs.concurrent.Akka
import play.db.ebean.Model
import scala.collection.JavaConversions._
import com.avaje.ebean.{SqlRow, Ebean}
import play.api.Play.current
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

/**
 * Created by Aldo on 04/08/14.
 */

object Embryon {

  def getAll: List[Embryon] = find.all.toList

  def findByID(idEmbryon: String, idSeed: String): Embryon = find.where.eq("id_embryon", idEmbryon).eq("idSeed", idSeed).findUnique

  def findByID(id:Int): Embryon = find.byId(id)

  def generateAnnotationFromRandomEmbryon(idUser: Long):List[Annotation] = {
    //val sqlEmbryonToAnnotate = Ebean.createSqlQuery("SELECT id FROM (SELECT id, embryon.idSeed FROM embryon LEFT JOIN (SELECT idSeed FROM annotation WHERE idUser=:idUser AND idCategory=\"Garbage\") as garbageSeed ON embryon.idSeed=garbageSeed.idSeed WHERE garbageSeed.idSeed Is NULL) as fEmbryon LEFT JOIN (SELECT idSeed, idEmbryon FROM annotation where idUser=:idUser) as annotated ON fEmbryon.idSeed=annotated.idSeed AND fEmbryon.id=annotated.idEmbryon WHERE annotated.idSeed Is Null ORDER BY RAND() LIMIT 1")
    val sqlEmbryonToAnnotateNew = Ebean.createSqlQuery(
      "SELECT id FROM embryon WHERE " +
        "idSeed NOT IN (SELECT idSeed FROM (" +
        "(SELECT idSeed FROM annotation WHERE idUser=:idUser AND idCategory=\"Garbage\") UNION " +
        "(SELECT idSeed FROM (SELECT idSeed, Count(*) as n FROM annotation WHERE idCategory=\"Garbage\" GROUP BY idSeed HAVING n>1) as gw2) " +
        ") as gt) AND " + "idSeed IN (SELECT id as idSeed FROM seed WHERE id NOT IN (SELECT idSeed as id FROM (SELECT idSeed, COUNT(*) as n FROM annotation WHERE idCategory IS NOT NULL GROUP BY idSeed HAVING n>1) as nw2)) AND " +
        "id NOT IN (SELECT idEmbryon as id FROM annotation WHERE idUser=:idUser) ORDER BY RAND() LIMIT 200"
    )
    sqlEmbryonToAnnotateNew.setParameter("idUser", idUser)

    /*    val rIdUser = (Math.random() * User.getAll.size + 1).toInt
    sqlEmbryonToAnnotateNew.setParameter("idUser", idUser)
    val sqlEmbryonToAnnotateAnnotated = Ebean.createSqlQuery(
      "SELECT id FROM embryon WHERE " +
        "idSeed NOT IN (SELECT idSeed FROM (" +
        "(SELECT idSeed FROM annotation WHERE idUser=:idUser AND idCategory=\"Garbage\") UNION " +
        "(SELECT idSeed FROM (SELECT idSeed, Count(*) as n FROM annotation WHERE idCategory=\"Garbage\" GROUP BY idSeed HAVING n>1) as gw2)" +
        ") as gt) AND " +
        "id NOT IN (SELECT idEmbryon as id FROM annotation WHERE idUser=:idUser) AND " +
        "id IN (SELECT idEmbryon as id FROM annotation WHERE idUser==:rIdUser AND idCategory IS NOT NULL) ORDER BY RAND() LIMIT 100"
    )
    sqlEmbryonToAnnotateAnnotated.setParameter("idUser", idUser)
    sqlEmbryonToAnnotateAnnotated.setParameter("rIdUser", rIdUser)
*/
    val sqlResult = sqlEmbryonToAnnotateNew.findList.toList// ::: sqlEmbryonToAnnotateAnnotated.findList.toList
    val numberOfUsers = User.getAll.last.id
    if(sqlResult.isEmpty) Nil else {
      sqlResult.map(r => {
        val embryonId: Int = r.getInteger("id").toInt
        val embryon = findByID(embryonId)
        val annotation = new Annotation
        annotation.seed = embryon.seed
        annotation.embryon = embryon
        annotation.category = null
        annotation.user = User.findByID(idUser)
        annotation.save

        val rIdUser = (Math.random() * numberOfUsers + 1).toLong
        if(Annotation.findByID(embryon.seed.id, embryonId, rIdUser)==null && User.findByID(rIdUser) != null) {
          val annotation2 = new Annotation
          annotation2.seed = embryon.seed
          annotation2.embryon = embryon
          annotation2.category = null
          annotation2.user = User.findByID(rIdUser)
          annotation2.save
        }
        annotation
      }).toList
    }
  }

  def getRandomAnnotation(idUser: Long): Annotation = {
    val sqlEmbryonToAnnotate = Ebean.createSqlQuery("SELECT id FROM annotation WHERE idUser=:idUser AND idCategory IS NULL ORDER BY RAND()")
    sqlEmbryonToAnnotate.setParameter("idUser", idUser)
    val sqlResult = sqlEmbryonToAnnotate.findList.toList
    val result = if(sqlResult.isEmpty) {
      Logger.info("getRandomAnnotation - sqlResult is empty")
      if(generateAnnotationFromRandomEmbryon(idUser).nonEmpty) {
        Logger.info("getRandomAnnotation - getRandomAnnotation")
        getRandomAnnotation(idUser)
      }
      else null
    } else {
      if(sqlResult.size == 150 || sqlResult.size == 100 || sqlResult.size <50)
        Akka.system.scheduler.scheduleOnce(0.seconds){
          generateAnnotationFromRandomEmbryon(idUser)
        }
      // add here that if you have a term garbaged the annotation is removed

      val garbagedTerms = Annotation.getGarbageTerms(idUser)
      val newResult = (for(a <- sqlResult) yield {
        val annotation = Annotation.findByID(a.getInteger("id"))
        if(garbagedTerms.contains(annotation.seed.id)) {
          Logger.info("getRandomAnnotation - clean generated annotations - " + annotation.id + " - " + annotation.seed.id)
          annotation.delete; null
        }else {
          annotation
        }
      })

      newResult.filter(_!=null).head
    }
    result
  }



  var find: Model.Finder[Int, Embryon] = new Model.Finder[Int, Embryon](classOf[Int], classOf[Embryon])
}

@Table(uniqueConstraints = Array(new UniqueConstraint(columnNames = Array("id_embryon", "idSeed")))) @Entity class Embryon extends Model{
  @Id var id:Int = 0
  @Column(nullable = false) var idEmbryon: String = null
  @OneToOne @JoinColumn(name="idSeed", referencedColumnName = "id") var seed: Seed = null
  @Column(nullable = false, columnDefinition="TIMESTAMP DEFAULT CURRENT_TIMESTAMP") var dateCreation: Timestamp = null
}
