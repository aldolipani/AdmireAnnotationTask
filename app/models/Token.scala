package models

import java.util.{Calendar, Date, UUID}
import play.{Logger, Configuration}
import java.net.URL
import play.i18n.Messages
import play.db.ebean.Model
import javax.persistence._
import java.sql.Timestamp

object Token {

  def findByTokenAndType(token: String): Token = {
    return find.where.eq("token", token).findUnique
  }

  private def getNewToken(user: User, email: String): Token = {
    val token: Token = new Token
    token.id = UUID.randomUUID.toString
    token.user = user
    token.save
    return token
  }

  var find: Model.Finder[String, Token] = new Model.Finder[String, Token](classOf[String], classOf[Token])
}

@Entity class Token extends Model {
  def isExpired: Boolean =
    return dateCreation != null && dateCreation.before(expirationTime)

  private def expirationTime: Date = {
    val cal: Calendar = Calendar.getInstance
    cal.set(Calendar.DATE, -1)
    return cal.getTime
  }

  @Id var id: String = ""
  @OneToOne @JoinColumn(name="idUser", referencedColumnName = "id") var user: User = null
  @Column(nullable = false, columnDefinition="TIMESTAMP DEFAULT CURRENT_TIMESTAMP") var dateCreation: Timestamp = null
}


