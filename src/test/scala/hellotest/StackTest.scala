package hellotest

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers.*
import scala.io.Source
import java.io.{File, PrintWriter}

class StackTest extends AnyFlatSpec:

  "The argument parser" should "correctly parse valid arguments" in {
    val args = Array("--cloud-size", "15", "--length-at-least", "5", "--window-size", "500", "--min-frequency", "2")
    val config = Main.parseArguments(args)
    
    config must not be empty
    val cfg = config.get
    cfg.cloudSize must equal(15)
    cfg.minLength must equal(5)
    cfg.windowSize must equal(500)
    cfg.minFrequency must equal(2)
  }

  it should "handle missing or incorrect arguments by returning None" in {
    val args = Array("--invalid-arg", "value")
    val config = Main.parseArguments(args)
    
    config must be(empty) // Parsing should fail, returning None
  }

  "The WordCloud" should "add and track words correctly" in {
    val ignoreList = Set("the", "is", "in")
    val wordCloud = new WordCloud(10, 3, 5, ignoreList, 1)
    
    wordCloud.addWord("hello")
    wordCloud.addWord("world")
    wordCloud.addWord("scala")
    wordCloud.addWord("hello")
    
    val topWords = wordCloud.getTopWords
    topWords must contain ("hello" -> 2)
    topWords must contain ("world" -> 1)
    topWords must contain ("scala" -> 1)
  }

  // it should "ignore words in the ignore list" in {
  //   val ignoreList = Set("hello", "scala")
  //   val wordCloud = new WordCloud(10, 3, 5, ignoreList, 1)
    
  //   wordCloud.addWord("hello")
  //   wordCloud.addWord("world")
  //   wordCloud.addWord("scala")
    
  //   val topWords = wordCloud.getTopWords
  //   topWords must contain ("world" -> 1)
  //   topWords must not contain ("hello" -> 1)
  //   topWords must not contain ("scala" -> 1)
  // }

  // it should "respect the word length filter" in {
  //     val wordCloud = new WordCloud(10, 5, 5, Set.empty, 1) // Set minLength to 5
  //     wordCloud.addWord("short")  // This word should not be included
  //     wordCloud.addWord("longword") // This word should be included

  //     val topWords = wordCloud.getTopWords
  //     topWords must not contain ("short" -> 1) // This should pass
  //     topWords must contain ("longword" -> 1) // This should also pass
  // }


  it should "remove the oldest word when exceeding window size" in {
    val wordCloud = new WordCloud(10, 3, 3, Set.empty, 1)
    
    wordCloud.addWord("word1")
    wordCloud.addWord("word2")
    wordCloud.addWord("word3")
    wordCloud.addWord("word4") // This should push "word1" out of the window
    
    val topWords = wordCloud.getTopWords
    topWords must not contain ("word1" -> 1)
    topWords must contain ("word2" -> 1)
    topWords must contain ("word3" -> 1)
    topWords must contain ("word4" -> 1)
  }

  "The ignore list reader" should "correctly load words from a file" in {
    val ignoreFile = File.createTempFile("ignore", ".txt").nn
    val writer = new PrintWriter(ignoreFile.nn)
    writer.write("word1\nword2\nword3\n")
    writer.close()
    
    val ignoreList = Main.readIgnoreList(ignoreFile.getAbsolutePath.nn)
    
    ignoreList must contain("word1")
    ignoreList must contain("word2")
    ignoreList must contain("word3")
    
    ignoreFile.delete() // Cleanup temp file
  }

end StackTest
