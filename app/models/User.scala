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
object User {

  def findByID(id: Long): User =
    find.byId(id)

  def findByUsername(username: String): User = {
    find.where.eq("email", username).findUnique
  }

  def getAll: List[User] = find.all.toList

  def authenticate(username: String, clearPassword: String): User = {
    val user: User = find.where.eq("email", username).findUnique
    if (user != null) {
      if (Hash.checkPassword(clearPassword, user.password)) {
        return user
      }
    }
    null
  }

  var find: Model.Finder[Long, User] = new Model.Finder[Long, User](classOf[Long], classOf[User])
}

@Entity class User extends Model {
  @Id var id: Int = 0
  @Column(unique = true, nullable = false) var email: String = null
  @Column(nullable = false) var password: String = null
  @Column(nullable = false, columnDefinition="TIMESTAMP DEFAULT CURRENT_TIMESTAMP") var lastUpdate: Timestamp  = null

  def changePassword(password: String) {
    this.password = Hash.createPassword(password)
    this.save
  }
}
