package models.utils

/**
 * Created by Aldo on 04/08/14.
 */

import org.mindrot.jbcrypt.BCrypt
import java.util.UUID

object Hash {

  def createPassword(clearString: String): String = {
    if (clearString == null) {
      throw new Exception("No password defined!")
    }
    return BCrypt.hashpw(clearString, BCrypt.gensalt)
  }

  /**
   * @param candidate         the clear text
   * @param encryptedPassword the encrypted password string to check.
   * @return true if the candidate matches, false otherwise.
   */
  def checkPassword(candidate: String, encryptedPassword: String): Boolean = {
    if (candidate == null) {
      return false
    }
    if (encryptedPassword == null) {
      return false
    }
    return BCrypt.checkpw(candidate, encryptedPassword)
  }
}


