package com.sixtysevenbricks.text.languagedetection

import collection.mutable.HashMap
import java.io.File
import org.apache.commons.io.{FileUtils, FilenameUtils}
import scala.collection.jcl.Conversions._

/**
 * Work out the language of a piece of text by comparing its n-gram fingerprint
 * with a set of pre-prepared fingerprints for various languages.
 * <p>
 * This is inspired by the paper "Evaluation of Language Identification Methods"
 * by Simon Kranig (http://www.sfs.uni-tuebingen.de/iscl/Theses/kranig.pdf),
 * linked to from http://tnlessone.wordpress.com/2007/05/13/how-to-detect-which-language-a-text-is-written-in-or-when-science-meets-human/,
 * and the code and explanation at http://boxoffice.ch/pseudo/code_functions.php.
 *
 * @author Inigo Surguy
 * @created 6th June 2009
 */
class LanguageDetector(fingerprintDir: File, languagesToCheck: List[String]) {

  /** Identify a text as being in a specific language, returning the name of that language. */
  def identifyLanguage(text : String) : String = {
    val maxChars = 4

    val profile = createNgramProfile(text, maxChars).take(300)
    val languageScores = for (language <-languagesToCheck;
                              languagePrints = fingerprints(language).filter(_.length<=maxChars))
                                  yield (language, computeDistance(profile, languagePrints))
    languageScores.sort(tupleComparator).last._1
  }

  type Profile = Seq[String]

  val fingerprints = readFingerprints(fingerprintDir)

  /** Read language fingerprints from .lm files containing one line per ngram sorted in order of occurrence */
  private def readFingerprints(dir : File) = {
    val fingerprints = new HashMap[String, Profile]()
    for (f <- dir.listFiles.filter(_.getName.endsWith(".fp"))) {
      val languageName = FilenameUtils.getBaseName(f.getName)
      val lines = FileUtils.readLines(f).asInstanceOf[java.util.List[String]]
      fingerprints(languageName) = for (line <- lines) yield line
    }
    fingerprints
  }

  /** Remove non-alphabetic characters except whitespace */
  def removePunctuation(text: String) = text.replaceAll("[^\\p{L}\\s]","")

  /** Convert tabs and runs of space to single spaces, and trim leading and trailing space. */
  def normalizeSpace(text: String) = text.trim().replaceAll("[\\s\\t]+"," ")

  /** A Map whose empty values are initialized on access according to the passed-in function. Useful for counters. */ 
  class DefaultDict[K, V](defaultFn: (K)=>V) extends HashMap[K, V] {
    override def default(key: K): V = return defaultFn(key)
  }

  /** Provide a total occurrence for each of the distinct terms in a sequence. i.e. (red,fish,blue,fish) will return: red 1, fish 2, blue 1. */
  def countValues[T](terms: Seq[T]) = {
    var count = new DefaultDict[T, int](K => 0)
    for (term <- terms) { count(term) = count(term) + 1}
    count
  }

  /** Extract the n-grams (bigrams, trigrams, etc.) from a piece of text into a sequence. Spaces are represented as _. */
  def extractNgrams(text: String, maxChars : Int) : Seq[String] = {
    val normalizedText : String = "_"+removePunctuation(normalizeSpace(text.toLowerCase)).replace(" ","_")+"_"
    for {start <- (0 to normalizedText.length);
         length <- (1 to maxChars);
         if start+length <= normalizedText.length} yield normalizedText.substring(start, start+length)
  }

  /** Provide a sorted summary count of the n-grams in a piece of text. */
  def createNgramProfile(text: String, maxChars : Int) : Profile =
    sortNgramValues( countValues( extractNgrams(text, maxChars) ) )

  /** Compare a tuple based on its second value. */
  private def tupleComparator(a : Pair[String, Int], b : Pair[String, Int]) =
    (a,b) match { case ((k1,v1),(k2,v2)) => v1.compare(v2)>0; }

  /** Remove the unigrams, then sort the ngrams by their occurrence into a flat profile. */
  private def sortNgramValues(countedValues : HashMap[String, Int]) : Profile =
    countedValues.filterKeys( _.length>1 ).toList.sort( tupleComparator ).map( _ match { case (k,v) => k } )

  /** Work out the distance between two n-gram profiles. */
  private def computeDistance(ngrams:Profile, otherProfile:Profile) : Int = {
    val max = 30
    val termDistances = for (key <- ngrams;
                             rank = ngrams.indexOf(key);
                             otherRank = if (otherProfile.contains(key)) otherProfile.indexOf(key) else 999;
                             diff = Math.abs(rank - otherRank))
                        yield if (diff>max) max else diff
    termDistances.foldLeft(0)(_+_)
  }

}