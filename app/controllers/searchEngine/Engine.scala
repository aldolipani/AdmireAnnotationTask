package controllers.searchEngine

import play.api.Play
import org.apache.lucene.util.Version
import org.apache.lucene.analysis.core.{WhitespaceAnalyzer, KeywordAnalyzer}
import org.apache.lucene.store.FSDirectory
import java.io.File
import org.apache.lucene.index.{IndexWriter, Term, DirectoryReader}
import org.apache.lucene.search.{TermQuery, BooleanQuery, TopScoreDocCollector, IndexSearcher}
import org.apache.lucene.queryparser.classic.QueryParser
import org.apache.lucene.queryparser.flexible.standard.QueryParserUtil
import org.apache.lucene.search.BooleanClause.Occur
import org.apache.lucene.analysis.standard.StandardAnalyzer
import org.apache.lucene.analysis.util.CharArraySet

/**
 * Created by Aldo on 04/08/14.
 */
object Engine {


  def replaceChars(str:String):String = {

    def replacePoint(str: String):String = {
      def replaceFirstPoint(str: String):String = {
        val point = "bbPOINTbb"
        if(str.size>1 && str.charAt(0)=='.' && str.charAt(1).isLetter) point + str.substring(1) else str
      }

      val point = "bbPOINTbb"
      var upto = 0
      val nstr = replaceFirstPoint(str)
      val sstr = nstr.split(" \\.[A-Za-z]")
      if(sstr.size>1){
        (for(i <- 0 until sstr.size) yield {
          if(i>0){
            val from = upto+2
            upto = from + sstr(i).length + 1
            " " + point + nstr.substring(from, upto)
          } else {
            upto += sstr(i).length
            nstr.substring(0, upto)
          }
        }).mkString
      }else nstr
    }

    def replaceHyphen(str: String):String = {
      val hyphen = "bbHYPHENbb"
      var upto = 0
      val sstr = str.split("[A-Za-z0-9]-[A-Za-z0-9]")
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
      }else if(sstr.size == 1 && sstr.head.length+3 == str.length){
        str.replace("-", hyphen)
      }else if(sstr.size == 0){
        str.replace("-", hyphen)
      }else str
    }

    def replaceNumber(str: String):String = {
      val number = Array("bbZerobb", "bbOnebb", "bbTwobb", "bbThreebb", "bbFourbb", "bbFivebb", "bbSixbb", "bbSevenbb", "bbEightbb", "bbNinebb")
      var nStr = str
      for(i <- 0 to 9){
        var strI:Int = 0
        while(nStr.substring(strI).indexOf(i.toString)>=0){
          strI = strI + nStr.substring(strI).indexOf(i.toString)
          nStr = if({val nd = nStr.substring(0, strI).reverse.find(c=> !c.isDigit && c!='.'); nd.nonEmpty && nd.get.isLetter} || {val nd = nStr.substring(strI).find(c=> !c.isDigit && c!='.'); nd.nonEmpty && nd.get.isLetter})
            nStr.substring(0, strI) + nStr.substring(strI).replaceFirst(i.toString, number(i))
          else nStr
          strI = strI + 1
        }
      }
      nStr
    }

    replaceNumber(replaceHyphen(replacePoint(str.replaceAll("@", "bbATbb"))))
  }


}

class Engine {
  val luceneVersion = Version.LUCENE_46
  val pathIndex = Play.current.configuration.getString("lucene.index").get
  val nPhrases = 5
  val nhits = 1000000

  def search(str: String): List[DocumentID] = {
    val nStr = Engine.replaceChars(str)
    val analyzer = new EnglishMinimalAnalyzer
    val index = FSDirectory.open(new File(pathIndex))
    val reader = DirectoryReader.open(index)
    val searcher = new IndexSearcher(reader)
    val query = new QueryParser(Version.LUCENE_46, "contents", analyzer).parse(nStr)
    val collector = TopScoreDocCollector.create(nhits, true)
    searcher.search(query, collector)
    val hits = collector.topDocs().scoreDocs
    val res = (for (i <- 0 until hits.length) yield {
      val docId = hits(i).doc
      val d = searcher.doc(docId)
      new DocumentID(d.get("id"))
    }).toList
    reader.close
    res
  }

  def getDocumentByID(mID:String, subID:Int): Document = getDocumentByID(mID + "#" + subID)

  def getDocumentByID(id: String): Document = {
    val analyzer = new KeywordAnalyzer
    val index = FSDirectory.open(new File(pathIndex))
    val reader = DirectoryReader.open(index)
    val searcher = new IndexSearcher(reader)
    val query = new QueryParser(luceneVersion, "id", analyzer).parse("\""+QueryParserUtil.escape(id)+"\"")
    val collector = TopScoreDocCollector.create(1, true)
    searcher.search(query, collector)
    val hits = collector.topDocs().scoreDocs
    val res = try {
      val docId = hits(0).doc
      val score = hits(0).score
      val d = searcher.doc(docId)
      new Document(id, d.get("text"))
    } catch {
      case e: Exception => new Document(id, "Error: document not found!")
    }
    reader.close
    res
  }

  def getSnippetByID(id: String): Snippet = {
    val ids = id.split("#")
    val mainID = ids(0)
    val subID = ids(1).toInt
    val nP = Math.floor(nPhrases/2).toInt
    val pre =
      (if(subID >= nP)
        (for(i <- (subID-nP) until subID) yield {getDocumentByID(mainID, i)}).toList else Nil)
    val post = (for(i <- subID+1 to (subID+nP)) yield {getDocumentByID(mainID, i)}).toList

    new Snippet(pre.map(_.content).mkString(" "), getDocumentByID(id).content, post.map(_.content).mkString(" "))
  }
}

class DocumentID(val id: String)

class Document(val id: String, val content: String) {
  def getSnippet: Snippet = (new Engine).getSnippetByID(id)
}

class Snippet(val pre: String, val center: String, val post: String) {
  override def toString: String = pre + " " + center + " " + post
}
