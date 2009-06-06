package com.sixtysevenbricks.text.languagedetection



import java.io._
import xml.XML

/**
 * Generate n-gram fingerprint profiles to be used for language recognition.
 * <p>
 * Expects to act on the Wikipedia export XML format, as produced by
 * http://en.wikipedia.org/wiki/Special:Export and the fr and de equivalents.
 *
 * @author Inigo Surguy
 * @created Jun 6, 2009 8:11:02 PM
 */
object ProfileCreator {

  val detector = new LanguageDetector(new File("."),List())
  val sourcesDir = new File("../src/test/resources/sources/")
  val outputDir = new File("../src/main/resources/profile")

  // Remove links and categories (not normal text), special Wikipedia instructions, and headings (often repeated)
  val regexToRemove = List("\\{\\{.*?\\}\\}","\\[.*?]","==.*?==")

  def createProfile(file : File) = {
    println("Reading "+file.getName)
    val xml = XML.load(new InputStreamReader(new FileInputStream(file),"UTF-8"))
    val texts = for (t <- xml \\ "text") yield t.text
    var text = texts.foldLeft("")(_+_)
    regexToRemove.foreach( regex => text = text.replaceAll(regex,""))

    println("Creating ngrams")
    val ngrams = detector.createNgramProfile(text, 5).take(2000)
    println("Writing out ngrams")
    val language = file.getName.substring(0, file.getName.indexOf("_"))
    val out = new PrintStream(new FileOutputStream(new File(outputDir, language+".lm")))
    ngrams.foreach(out.println(_))
    out.close()
    ngrams
  }

  def createProfiles() =
    sourcesDir.listFiles().filter(_.getName.endsWith(".xml")).foreach(createProfile(_))

  def main(args : Array[String]) = ProfileCreator.createProfiles()

}