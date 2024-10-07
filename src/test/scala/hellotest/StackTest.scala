package hellotest

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers.*
import scala.io.Source
import java.io.{File, PrintWriter}
import java.nio.file.{Files, Paths}

import org.scalatest.concurrent.TimeLimits
import scala.concurrent.duration._

import org.scalatest.matchers.should.Matchers

class StackTest extends AnyFlatSpec with Matchers with TimeLimits {

  
  "The WordCloud" should "handle heavy load without significant performance degradation" in {
    val ignoreList = Set.empty[String]
    val wordCloud = new WordCloud(100000, 3, 5, ignoreList, 1) // Adjust max size if necessary

    // Measure performance while adding a large number of words
    val largeWordList = (1 to 100000).map(i => s"word$i").toList

    // Use a time limit to ensure performance stays within acceptable bounds
    val maxDuration: FiniteDuration = 5.seconds // Define max duration
    val duration: FiniteDuration = measuringTime {
      largeWordList.foreach(wordCloud.addWord)
    }

    println(s"Time taken to add 100,000 words: ${duration.toMillis} ms")
    duration should be <= maxDuration // Ensure we do not exceed the max duration
    wordCloud.getTopWords.size must be <= 100000 // Ensure no overflow occurs
  }

  private def measuringTime[R](block: => R): FiniteDuration = {
    val start = System.nanoTime()
    block
    val end = System.nanoTime()
    Duration.fromNanos(end - start).asInstanceOf[FiniteDuration] // Cast to FiniteDuration
  }

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

  

  "Argument parser" should "parse valid arguments correctly" in {
    val args = Array("--cloud-size", "5", "--length-at-least", "4", "--window-size", "100", "--ignore-list", "ignore.txt")
    val config = Main.parseArguments(args).getOrElse(fail("Failed to parse arguments"))

    config.cloudSize shouldEqual 5
    config.minLength shouldEqual 4
    config.windowSize shouldEqual 100
    config.ignoreListFile shouldEqual Some("ignore.txt")
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

  it should "handle zero cloud size" in {
    val args = Array("--cloud-size", "0", "--length-at-least", "5")
    val config = Main.parseArguments(args)

    config must be(empty) // Expect failure for cloud size of 0
  }

  it should "ignore words in the ignore list when processing input" in {
    val ignoreList = Set("ignoreme")
    val wordCloud = new WordCloud(10, 3, 5, ignoreList, 1)

    wordCloud.addWord("ignoreme") // This should not be counted
    wordCloud.addWord("test")

    val topWords = wordCloud.getTopWords
    topWords must contain("test" -> 1) // "test" should be counted
    topWords must not contain ("ignoreme" -> 1) // "ignoreme" should not be counted
  }
  it should "correctly count the same word added multiple times" in {
      val wordCloud = new WordCloud(10, 3, 5, Set.empty, 1)

      wordCloud.addWord("test")
      wordCloud.addWord("test")
      wordCloud.addWord("test")

      val topWords = wordCloud.getTopWords
      topWords must contain("test" -> 3) // "test" should be counted as 3
  }
  it should "not exceed max size when adding words" in {
    val wordCloud = new WordCloud(3, 3, 5, Set.empty, 1)

    wordCloud.addWord("one")
    wordCloud.addWord("two")
    wordCloud.addWord("three")
    wordCloud.addWord("four") // Should evict one of the existing words

    val topWords = wordCloud.getTopWords
    topWords.size must be <= 3 // Ensure size does not exceed max size
  }
  it should "handle empty ignore list file" in {
      // Create a temporary empty ignore list file
      val tempIgnoreFile = Files.createTempFile("ignoreEmpty", ".txt")
      Files.write(tempIgnoreFile, "".getBytes)

      val args = Array("--cloud-size", "10", "--length-at-least", "3", "--window-size", "5", "--min-frequency", "1", "--ignore-list-file", tempIgnoreFile.toString)

      // Cleanup temporary file
      Files.delete(tempIgnoreFile)
  }  
  it should "handle very large values for window size" in {
    val args = Array("--cloud-size", "10", "--length-at-least", "3", "--window-size", "100000", "--min-frequency", "1")
    val config = Main.parseArguments(args)

    config must not be empty // Should handle large values gracefully
    val cfg = config.get
    cfg.windowSize must equal(100000)
  }  
  "Main.readIgnoreList" should "correctly read words from a file" in {
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
  
  it should "correctly parse minimum required arguments" in {
    val args = Array("--cloud-size", "15", "--length-at-least", "5")
    val config = Main.parseArguments(args)
    config must not be empty
    config.get.cloudSize must equal(15)
    config.get.minLength must equal(5)
  }

  it should "return None for arguments with invalid types" in {
    val args = Array("--cloud-size", "invalid")
    val config = Main.parseArguments(args)
    config must be(empty) // Parsing should fail
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

  it should "correctly count the same word added multiple times" in {
    val wordCloud = new WordCloud(10, 3, 5, Set.empty, 1)

    wordCloud.addWord("test")
    wordCloud.addWord("test")
    wordCloud.addWord("test")

    val topWords = wordCloud.getTopWords
    topWords must contain("test" -> 3) // "test" should be counted as 3
  }

  it should "return an empty ignore list for an invalid file path" in {
    val ignoreList = Main.readIgnoreList("invalid/path/to/file.txt")
    ignoreList must be(empty) // Expect empty list for invalid path
  }

  it should "handle negative values for non-negative parameters" in {
    val args = Array("--cloud-size", "-1", "--length-at-least", "5")
    val config = Main.parseArguments(args)

    config must be(empty) // Parsing should fail due to negative cloud size
  }

  it should "return None when required arguments are missing" in {
    val args = Array("--length-at-least", "5")
    val config = Main.parseArguments(args)

    config must be(empty) // Required argument --cloud-size is missing
  }

  it should "update the word cloud output based on the update frequency" in {
    val ignoreList = Set.empty[String]
    val wordCloud = new WordCloud(10, 5, 5, ignoreList, 1) // Set minFrequency to 1

    // Simulate input
    val wordsToAdd = List("hello", "world", "scala", "hello", "spark")
    
    // Add words and track updates
    wordsToAdd.foreach(wordCloud.addWord)

    val topWordsInitial = wordCloud.getTopWords
    println(s"Initial top words: $topWordsInitial")

    // Assuming `runWordCloud` manages printing output based on updateFrequency
    // Check that word cloud was updated correctly
    topWordsInitial must contain("hello" -> 2)
    topWordsInitial must contain("world" -> 1)
    topWordsInitial must contain("scala" -> 1)
    topWordsInitial must contain("spark" -> 1)
  }

  it should "handle a large number of words efficiently" in {
    val wordCloud = new WordCloud(100, 3, 5, Set.empty, 1) // Larger size

    // Simulate adding a large number of unique words
    (1 to 1000).foreach(i => wordCloud.addWord(s"word$i"))

    val topWords = wordCloud.getTopWords
    topWords.size must be <= 100 // Ensure we respect the max size
  }

  "process ignore list file with special characters" should "handle special characters correctly" in {
    // Create a temporary file
    val ignoreFile = File.createTempFile("ignoreSpecial", ".txt").nn

    try {
      // Write special characters to the file
      val writer = new PrintWriter(ignoreFile)
      writer.write("word1\nword@2\nword#3\n")
      writer.close()

      // Read the ignore list
      val ignoreList = Main.readIgnoreList(ignoreFile.getAbsolutePath.nn)

      // Validate the contents of the ignore list
      ignoreList should contain("word1")
      ignoreList should contain("word@2")
      ignoreList should contain("word#3")
    } finally {
            // Cleanup temp file and assert the result
        if (!ignoreFile.delete()) {
          fail("Failed to delete the temporary ignore file.") // Use fail to indicate an error in deletion
        }
    }
  }

}
