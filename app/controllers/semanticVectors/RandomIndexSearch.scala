package controllers.semanticVectors

/**
 * Created by Aldo on 05/08/14.
 */

import pitt.search.semanticvectors.FlagConfig
import play.api.Play
import scala.collection.JavaConversions._

object RandomIndexSearch{


}

class RandomIndexSearch {
  val pathIndex = Play.current.configuration.getString("randomindexing.index").get
  val nhits = 1000;

  def replaceFirstPoint(str: String):String = {
    val point = "bbPOINTbb"
    if(str.charAt(0)=='.') point + str.substring(1) else str
  }

  def replaceHyphen(str: String):String = {
    val hyphen = "bbHYPHENbb"
    var upto = 0
    val sstr = str.split("[A-Za-z]-[A-Za-z]")
    if(sstr.size > 1){
      (for(i <- 0 until sstr.size) yield {
        if(i == sstr.size-1){
          val from = upto + 1
          upto = from + sstr(i).length +1// +1 al prossimo
          hyphen + str.substring(from, upto)
        }else if(i>0){
          val from = upto + 1
          upto = from + sstr(i).length +2// +1 al prossimo
          hyphen + str.substring(from, upto)
        } else {
          upto += sstr(i).length+1
          str.substring(0, upto)
        }
      }).mkString
    }else sstr(0)
  }

  def replaceNumber(str: String):String = {
    val number = Array("bbZero", "bbOnebb", "bbTwobb", "bbThreebb", "bbFourbb", "bbFivebb", "bbSixbb", "bbSevenbb", "bbEightbb", "bbNinebb")
    var nStr = str
    for(i <- 0 to 9){
      var strI:Int = 0
      while(nStr.substring(strI).indexOf(i.toString)>=0){
        strI = strI + nStr.substring(strI).indexOf(i.toString)
        nStr = if(strI>0 && nStr.charAt(strI-1).isLetter) nStr.substring(0, strI) + nStr.substring(strI).replaceFirst(i.toString, number(i)) else nStr
        strI = strI + 1
      }
    }
    nStr
  }

  def search(str: String): List[String] = {
    val flagConfig = FlagConfig.parseFlagsFromString("-queryvectorfile " + pathIndex+"/termtermvectors.bin " + replaceNumber(replaceHyphen(replaceFirstPoint(str.replaceAll("@", "bbATbb")))))
    val results = Search.runSearch(flagConfig).toList

    if (results.nonEmpty) {
      results.map(r => {
        r.getObjectVector.getObject.toString
      })
    } else {
      Nil
    }
  }

}
