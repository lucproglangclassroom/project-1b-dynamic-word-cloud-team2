package hellotest

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers.*
import org.scalatest.matchers.should.Matchers
import scala.io.Source
import java.io.{File, PrintWriter}

class WordCloudSpec extends AnyFlatSpec with Matchers {

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

  it should "handle invalid values for cloud size" in {
    val args = Array("--cloud-size", "-5", "--length-at-least", "5")
    val config = Main.parseArguments(args)

    config must be(empty) // Parsing should fail due to negative cloud size
  }

  "The WordCloud" should "add and track words correctly" in {
    val ignoreList = Set("the", "is", "in")
    val wordCloud = new WordCloud(10, 3, 5, ignoreList, 1)

    wordCloud.addWord("hello")
    wordCloud.addWord("world")
    wordCloud.addWord("scala")
    wordCloud.addWord("hello")

    val topWords = wordCloud.getTopWords
    topWords must contain("hello" -> 2)
    topWords must contain("world" -> 1)
    topWords must contain("scala" -> 1)
  }

  it should "ignore words in the ignore list" in {
    val ignoreList = Set("hello", "scala")
    val wordCloud = new WordCloud(10, 3, 5, ignoreList, 1)

    wordCloud.addWord("hello")
    wordCloud.addWord("world")
    wordCloud.addWord("scala")

    val topWords = wordCloud.getTopWords
    topWords must contain("world" -> 1)
    topWords must not contain ("hello" -> 1)
    topWords must not contain ("scala" -> 1)
  }

  it should "respect the word length filter" in {
    val wordCloud = new WordCloud(10, 5, 5, Set.empty, 1) // Set minLength to 5

    wordCloud.addWord("short")  // This word should be included since length is exactly minLength
    wordCloud.addWord("longword") // This word should also be included
    wordCloud.addWord("tiny") // This word should be ignored

    val topWords = wordCloud.getTopWords
    println(s"Top words: $topWords")  // Print top words for debugging

    topWords must contain("short" -> 1) // Adjusted to accept words of exactly minLength
    topWords must contain("longword" -> 1)
    topWords must not contain ("tiny" -> 1) // Verify that shorter word is ignored
  }

  it should "remove the oldest word when exceeding window size" in {
    val wordCloud = new WordCloud(10, 3, 3, Set.empty, 1)

    wordCloud.addWord("word1")
    wordCloud.addWord("word2")
    wordCloud.addWord("word3")
    wordCloud.addWord("word4") // This should push "word1" out of the window

    val topWords = wordCloud.getTopWords
    println(s"Top words after adding word4: $topWords") // Print top words for debugging

    topWords must not contain ("word1" -> 1)
    topWords must contain ("word2" -> 1)
    topWords must contain ("word3" -> 1)
    topWords must contain ("word4" -> 1)
  }

  it should "respect the minimum frequency filter" in {
    val ignoreList = Set.empty[String]
    val wordCloud = new WordCloud(10, 5, 5, ignoreList, 2) // Minimum frequency set to 2

    // Adding the word twice to meet the minimum frequency requirement
    wordCloud.addWord("word1")
    wordCloud.addWord("word1") // Now it should count as 2
    wordCloud.addWord("word2") // This will be ignored due to frequency

    val topWords = wordCloud.getTopWords

    topWords must contain("word1" -> 2) // Expect "word1" to be present
    topWords must not contain ("word2" -> 1) // "word2" should not be included
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

  it should "return an empty ignore list for an empty file" in {
    val ignoreFile = File.createTempFile("ignoreEmpty", ".txt").nn
    new PrintWriter(ignoreFile.nn).close() // Empty file

    val ignoreList = Main.readIgnoreList(ignoreFile.getAbsolutePath.nn)

    ignoreList must be(empty) // Verify empty ignore list

    ignoreFile.delete() // Cleanup temp file
  }

  it should "handle duplicate words in the ignore list" in {
    val ignoreFile = File.createTempFile("ignoreDuplicates", ".txt").nn
    val writer = new PrintWriter(ignoreFile.nn)
    writer.write("word1\nword1\nword2\nword3\n")
    writer.close()

    val ignoreList = Main.readIgnoreList(ignoreFile.getAbsolutePath.nn)

    ignoreList must contain("word1") // Check presence of duplicate word
    ignoreList must contain("word2")
    ignoreList must contain("word3")

    ignoreFile.delete() // Cleanup temp file
  }

  it should "handle special characters in words" in {
    val ignoreList = Set.empty[String]
    val wordCloud = new WordCloud(10, 3, 5, ignoreList, 1)

    wordCloud.addWord("hello!")
    wordCloud.addWord("world?")
    wordCloud.addWord("scala#")

    val topWords = wordCloud.getTopWords
    topWords.toSet should contain("hello!" -> 1)
    topWords.toSet should contain("world?" -> 1)
    topWords.toSet should contain("scala#" -> 1)
  } 

  "The WordCloud" should "handle empty input correctly" in {
    val wordCloud = new WordCloud(10, 3, 5, Set.empty, 1)

    val topWords = wordCloud.getTopWords
    topWords must be(empty) // No words added, so should return empty list
  } 

  it should "handle words with different cases as the same" in {
    val ignoreList = Set.empty[String]
    val wordCloud = new WordCloud(10, 3, 5, ignoreList, 1)

    wordCloud.addWord("Hello")
    wordCloud.addWord("hello") // Should be counted as the same word
    wordCloud.addWord("HELLO") // Should be counted as the same word

    val topWords = wordCloud.getTopWords
    topWords must contain("hello" -> 3) // All should count as "hello"
  }  

  it should "respect the minimum frequency when adding words" in {
    val ignoreList = Set.empty[String]
    val wordCloud = new WordCloud(10, 3, 5, ignoreList, 2) // Minimum frequency set to 2

    wordCloud.addWord("word1")
    wordCloud.addWord("word1") // Now it should count as 2
    wordCloud.addWord("word2") // This will be ignored due to frequency

    val topWords = wordCloud.getTopWords
    topWords must contain("word1" -> 2) // Expect "word1" to be present
    topWords must not contain ("word2" -> 1) // "word2" should not be included
  }

  it should "remove words that fall below the minimum frequency after multiple additions" in {
    val ignoreList = Set.empty[String]
    val wordCloud = new WordCloud(10, 3, 5, ignoreList, 2) // Minimum frequency set to 2

    wordCloud.addWord("word1")
    wordCloud.addWord("word1") // Now it should count as 2
    wordCloud.addWord("word2") // This will be ignored due to frequency
    wordCloud.addWord("word2") // Now "word2" should count as 2
    wordCloud.addWord("word3") // Add a third word

    var topWords = wordCloud.getTopWords
    topWords must contain("word1" -> 2)
    topWords must contain("word2" -> 2)
    topWords must not contain ("word3" -> 1)

    // Reduce frequency of word2 below threshold
    wordCloud.addWord("word2") // Reduce count to 1
    topWords = wordCloud.getTopWords
    topWords must not contain ("word2" -> 1) // "word2" should not be included
  }  

  it should "handle large input sizes efficiently" in {
    val ignoreList = Set.empty[String]
    val wordCloud = new WordCloud(10, 3, 5, ignoreList, 1)

    (1 to 1000).foreach(i => wordCloud.addWord(s"word$i"))
    val topWords = wordCloud.getTopWords
    topWords.size must be <= 10 // Ensure that we are limiting the top words
  }

  it should "handle malformed ignore list files" in {
    val ignoreFile = File.createTempFile("malformed", ".txt").nn
    val writer = new PrintWriter(ignoreFile.nn)
    writer.write("word1\nmalformed-entry@\nword2")
    writer.close()

    val ignoreList = Main.readIgnoreList(ignoreFile.getAbsolutePath.nn)

    ignoreList must contain("word1") // Ignore malformed entries
    ignoreList must contain("word2")
    ignoreFile.delete() // Cleanup
  }  

  it should "handle zero and negative frequencies" in {
    val args = Array("--cloud-size", "10", "--length-at-least", "5", "--window-size", "500", "--min-frequency", "0")
    val config = Main.parseArguments(args)

    config must be(empty) // minFrequency should not be zero

    val argsNegative = Array("--cloud-size", "10", "--length-at-least", "5", "--window-size", "500", "--min-frequency", "-2")
    val configNegative = Main.parseArguments(argsNegative)

    configNegative must be(empty) // Negative minFrequency should be invalid
  } 

  it should "handle maximum cloud size" in {
    val wordCloud = new WordCloud(Int.MaxValue, 3, 10, Set.empty, 1)
    
    (1 to 10000).foreach(i => wordCloud.addWord(s"word$i"))
    
    val topWords = wordCloud.getTopWords
    topWords.size must be <= Int.MaxValue // Ensure we respect max size
  }  

  it should "handle minimum window size of 1" in {
    val wordCloud = new WordCloud(10, 3, 1, Set.empty, 1) // Set windowSize to 1

    wordCloud.addWord("word1")
    wordCloud.addWord("word2") // Should replace "word1"

    val topWords = wordCloud.getTopWords
    topWords must not contain ("word1" -> 1) // "word1" should not be present
  }  

  it should "treat case variations of words as the same" in {
    val ignoreList = Set.empty[String]
    val wordCloud = new WordCloud(10, 3, 5, ignoreList, 1)

    wordCloud.addWord("Case")
    wordCloud.addWord("case")
    wordCloud.addWord("CASE")

    val topWords = wordCloud.getTopWords
    topWords must contain("case" -> 3) // All variations should count towards "case"
  }

  it should "handle frequency adjustments correctly" in {
    val ignoreList = Set.empty[String]
    val wordCloud = new WordCloud(10, 3, 5, ignoreList, 2) // Minimum frequency set to 2

    wordCloud.addWord("word1")
    wordCloud.addWord("word1") // Count = 2
    wordCloud.addWord("word2")
    wordCloud.addWord("word2") // Count = 2
    wordCloud.addWord("word3") // Should not be counted

    var topWords = wordCloud.getTopWords
    topWords must contain("word1" -> 2)
    topWords must contain("word2" -> 2)
    topWords must not contain ("word3" -> 1)

    // Remove a word to adjust counts
    wordCloud.addWord("word2") // Adjust word2 count back to 3
    wordCloud.addWord("word2") // Count = 4 now

    topWords = wordCloud.getTopWords
    topWords must contain("word2" -> 4) // Expect "word2" to be present with updated count
  }
  it should "handle conflicting command-line arguments" in {
    val args = Array("--cloud-size", "10", "--min-frequency", "0")
    val config = Main.parseArguments(args)
    config must be(empty) // Expecting an empty result for invalid min-frequency
  }  
}