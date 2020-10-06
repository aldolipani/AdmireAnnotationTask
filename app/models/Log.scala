package models

import play.db.ebean.Model
import javax.persistence.{Column, Id, Entity}
import java.sql.Timestamp

/**
 * Created by Aldo on 13/08/14.
 */
object Log {
  def findByID(id: Int): Log = find.byId(id)

  var find: Model.Finder[Int, Log] = new Model.Finder[Int, Log](classOf[Int], classOf[Log])
}

@Entity class Log extends Model {
  @Id var id: Int = 0
  @Column(nullable = false, columnDefinition="TEXT") var text: String = ""
  @Column(nullable = false, columnDefinition="TIMESTAMP DEFAULT CURRENT_TIMESTAMP") var dateCreation: Timestamp = null
}
