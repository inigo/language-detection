package com.sixtysevenbricks.text.languagedetection

import org.junit.Test
import org.junit.Assert._
import java.io.File

class LanguageDetectorTest {
  var detector = new LanguageDetector(getFile("/profile"), List("fr-FR", "de-DE","en-US"))
  def getFile(path : String) = new File(this.getClass.getResource(path).getFile.replace("%20"," "))

  @Test def removePunctuation() = assertEquals("Fish cakes",detector.removePunctuation("Fi!sh cakes.?123"))
  @Test def removePunctuationEmptyString() = assertEquals("",detector.removePunctuation(""))
  @Test def normalizeSpace() = assertEquals("Fish cakes",detector.normalizeSpace("  Fish\u0009 \u000A cakes \u000A  "))

  @Test def extractNgrams1() = assertEquals(List("_","f","i","s","h","_"),detector.extractNgrams("Fish",1).toList)
  @Test def extractNgrams2() = assertEquals(List("_","_f","f","fi","i","is","s","sh","h","h_","_"),
                                  detector.extractNgrams("Fish",2).toList)
  @Test def extractNgrams3() = assertEquals(List("_","_f","_fi","f","fi","fis","i","is","ish","s","sh","sh_","h","h_","_"),
                                  detector.extractNgrams("Fish",3).toList)

  @Test def countValue() = assertEquals(Map("fish"->2,"red"->1,"blue"->1),detector.countValues(List("red","fish","blue","fish")))
  @Test def createNgramProfile() = assertEquals(Set("fi","is","_f","sh","h_"),Set() ++ detector.createNgramProfile("Fish",2))
  @Test def createNgramProfileCount() = assertEquals(List("a_","_b","ba","fa","_f"),detector.createNgramProfile("Fa Ba Ba Ba",2))

  @Test def identifyEnglish() = assertEquals("en-US", detector.identifyLanguage("Hello world this is English"))
  @Test def identifyFrench() = assertEquals("fr-FR", detector.identifyLanguage("Calcul des structures pour leur "))
  @Test def identifyGerman() = assertEquals("de-DE", detector.identifyLanguage("Bemessung und Konstruktion von Stahlbauten"))

}
