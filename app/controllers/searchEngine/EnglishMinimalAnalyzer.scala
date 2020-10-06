package controllers.searchEngine

import org.apache.lucene.analysis.{TokenFilter, TokenStream, Tokenizer, Analyzer}
import java.io.Reader
import org.apache.lucene.analysis.Analyzer.TokenStreamComponents
import org.apache.lucene.analysis.core.{StopFilter, StopAnalyzer}
import org.apache.lucene.util.Version
import org.apache.lucene.analysis.standard.StandardTokenizer
import org.apache.lucene.analysis.en.EnglishMinimalStemFilter
import org.apache.lucene.analysis.tokenattributes.CharTermAttribute

/**
 * Created by aldo on 6/6/14.
 */
class EnglishMinimalAnalyzer extends Analyzer{

  override def createComponents(s: String, r: Reader): TokenStreamComponents = {
    val source:Tokenizer = new StandardTokenizer(Version.LUCENE_46, r)

    var result:TokenStream = null
    result = new EnglishMinimalStemFilter(source)

    new TokenStreamComponents(source,result)
  }
}

