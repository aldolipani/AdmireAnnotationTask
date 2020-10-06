package models

import javax.persistence.Column
import javax.persistence.Entity
import javax.persistence.Id
import java.util.Date
import models.utils.Hash
import play.db.ebean.Model
import java.sql.Timestamp
import scala.collection.JavaConversions._
/**
 * Created by Aldo on 04/08/14.
 */

object Category {

  def findByID(id: String): Category = find.byId(id)

  def getAll: List[Category] = find.all.toList

  var find: Model.Finder[String, Category] = new Model.Finder[String, Category](classOf[String], classOf[Category])
}

@Entity class Category extends Model {
  @Id var id: String = null
}
