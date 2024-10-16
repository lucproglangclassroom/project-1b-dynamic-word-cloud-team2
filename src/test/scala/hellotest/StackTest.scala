package hellotest

import org.scalatest.funsuite.AnyFunSuite
import scala.collection.mutable.Map
import scala.language.unsafeNulls

class TestOutputCollector extends WordCloud.OutputSink {
  val frequencyMap: Map[String, Int] = Map()
  
  def doOutput(value: Seq[(String, Int)]) = {
    frequencyMap.clear()
    frequencyMap ++= value.toMap
  }
}

class StackTest extends AnyFunSuite {

  test("Words are counted in a case-insensitive manner") {
    val inputWords = Iterator("Apple", "Banana", "APPLE", "banana", "apple")
      .map(_.toLowerCase)
    val outputCollector = new TestOutputCollector()
    WordCloud.processing(
      words = inputWords,
      cloud_size = 2,
      length_at_least = 1,
      window_size = 5,
      every_K = 1,
      min_frequency = 1,
      outputSink = outputCollector
    )

    assert(outputCollector.frequencyMap("apple") == 3, "Expected frequency of 'apple' to be 3, but found " + outputCollector.frequencyMap("apple"))
    assert(outputCollector.frequencyMap("banana") == 2, "Expected frequency of 'banana' to be 2, but found " + outputCollector.frequencyMap("banana"))
  }

  test("Words shorter than the specified length are excluded") {
    val inputWords = Iterator("x", "y", "z", "xx", "yy", "zz", "xx", "yy")
    val outputCollector = new TestOutputCollector()
    WordCloud.processing(
      words = inputWords,
      cloud_size = 3,
      length_at_least = 2,
      window_size = 5,
      every_K = 1,
      min_frequency = 1,
      outputSink = outputCollector
    )

    assert(!outputCollector.frequencyMap.contains("x"), "'x' should not be in the output as its length is less than 2")
    assert(!outputCollector.frequencyMap.contains("y"), "'y' should not be in the output as its length is less than 2")
    assert(outputCollector.frequencyMap("xx") == 2, "Expected frequency of 'xx' to be 2, but found " + outputCollector.frequencyMap("xx"))
    assert(outputCollector.frequencyMap("yy") == 2, "Expected frequency of 'yy' to be 2, but found " + outputCollector.frequencyMap("yy"))
  }

  test("Window size maintains first input first output behavior") {
    val inputWords = Iterator("ff", "bb", "cc", "aa", "bb", "gg")
    val outputCollector = new TestOutputCollector()
    WordCloud.processing(
      words = inputWords,
      cloud_size = 4,
      length_at_least = 2,
      window_size = 5,
      every_K = 1,
      min_frequency = 1,
      outputSink = outputCollector
    )
    assert(outputCollector.frequencyMap.contains("gg"), "'gg' should be included in the output")
    assert(!outputCollector.frequencyMap.contains("ff"), "'ff' should be excluded as it should have been pushed out")
  }

  test("No words are included when length requirement is not satisfied") {
    val inputWords = Iterator("g", "h", "i", "j", "k", "l")
    val outputCollector = new TestOutputCollector()
    WordCloud.processing(
      words = inputWords,
      cloud_size = 4,
      length_at_least = 2,
      window_size = 5,
      every_K = 1,
      min_frequency = 1,
      outputSink = outputCollector
    )

    assert(outputCollector.frequencyMap.isEmpty, "Expected no words in output since all are less than the specified length")
  }

  test("No words are included when minimum frequency requirement is not satisfied") {
    val inputWords = Iterator("qq", "rr", "ss", "tt", "uu", "vv")
    val outputCollector = new TestOutputCollector()
    WordCloud.processing(
      words = inputWords,
      cloud_size = 4,
      length_at_least = 2,
      window_size = 5,
      every_K = 1,
      min_frequency = 5,
      outputSink = outputCollector
    )

    assert(outputCollector.frequencyMap.isEmpty, "Expected no words in output since none appear 5 times or more")
  }

  test("Words with special characters are processed correctly") {
    val inputWords = Iterator("hello!", "@world", "hello", "#scala", "world?", "hello")
      .flatMap(_.split("(?U)[^\\p{Alpha}0-9']+"))
      .map(_.toLowerCase)
    val outputCollector = new TestOutputCollector()
    WordCloud.processing(
      words = inputWords,
      cloud_size = 3,
      length_at_least = 1,
      window_size = 6,
      every_K = 1,
      min_frequency = 1,
      outputSink = outputCollector
    )

    assert(outputCollector.frequencyMap("hello") == 3, "Expected frequency of 'hello' to be 3, but found " + outputCollector.frequencyMap("hello"))
    assert(outputCollector.frequencyMap("world") == 2, "Expected frequency of 'world' to be 2, but found " + outputCollector.frequencyMap("world"))
    assert(outputCollector.frequencyMap("scala") == 1, "Expected frequency of 'scala' to be 1, but found " + outputCollector.frequencyMap("scala"))
  }

  test("Words with apostrophes are processed correctly") {
    val inputWords = Iterator("don't", "can't", "won't", "can't", "don't")
    val outputCollector = new TestOutputCollector()
    WordCloud.processing(
      words = inputWords,
      cloud_size = 3,
      length_at_least = 3,
      window_size = 5,
      every_K = 1,
      min_frequency = 1,
      outputSink = outputCollector
    )

    assert(outputCollector.frequencyMap("don't") == 2, "Expected frequency of 'don't' to be 2, but found " + outputCollector.frequencyMap("don't"))
    assert(outputCollector.frequencyMap("can't") == 2, "Expected frequency of 'can't' to be 2, but found " + outputCollector.frequencyMap("can't"))
    assert(outputCollector.frequencyMap("won't") == 1, "Expected frequency of 'won't' to be 1, but found " + outputCollector.frequencyMap("won't"))
  }

  test("WordCloud calculates the correct frequency of words") {
    val inputWords = Iterator("apple", "banana", "apple", "orange", "banana", "apple")
    val outputCollector = new TestOutputCollector
    WordCloud.processing(
      words = inputWords,
      cloud_size = 3,
      length_at_least = 3,
      window_size = 6,
      every_K = 1,
      min_frequency = 1,
      outputSink = outputCollector
    )

    assert(outputCollector.frequencyMap("apple") == 3, "Expected frequency of 'apple' to be 3, but found " + outputCollector.frequencyMap("apple"))
    assert(outputCollector.frequencyMap("banana") == 2, "Expected frequency of 'banana' to be 2, but found " + outputCollector.frequencyMap("banana"))
    assert(outputCollector.frequencyMap("orange") == 1, "Expected frequency of 'orange' to be 1, but found " + outputCollector.frequencyMap("orange"))
  }

  test("Output respects the cloud size limit") {
    val inputWords = Iterator("apple", "banana", "cherry", "date", "elderberry", "fig", "grape")
    val outputCollector = new TestOutputCollector()
    WordCloud.processing(
      words = inputWords,
      cloud_size = 3,
      length_at_least = 1,
      window_size = 10,
      every_K = 1,
      min_frequency = 1,
      outputSink = outputCollector
    )
    
    assert(outputCollector.frequencyMap.size <= 3, s"Expected output size to be limited to cloud size of 3, but found ${outputCollector.frequencyMap.size}")
  }

  test("Window size first input first output behavior at exact boundary") {
    val inputWords = Iterator("h", "i", "j", "k", "l", "m", "n")
    val outputCollector = new TestOutputCollector()
    WordCloud.processing(
      words = inputWords,
      cloud_size = 7,
      length_at_least = 1,
      window_size = 5,
      every_K = 1,
      min_frequency = 1,
      outputSink = outputCollector
    )

    assert(!outputCollector.frequencyMap.contains("h"), "'h' should be excluded since it should have been pushed out when the window reached its limit")
    assert(outputCollector.frequencyMap.contains("m"), "'m' should be included in the output")
    assert(outputCollector.frequencyMap.contains("n"), "'n' should be included in the output")
  }

  test("Processing handles empty input without errors") {
    val inputWords = Iterator.empty[String]
    val outputCollector = new TestOutputCollector
    WordCloud.processing(
      words = inputWords,
      cloud_size = 3,
      length_at_least = 1,
      window_size = 5,
      every_K = 1,
      min_frequency = 1,
      outputSink = outputCollector
    )

    assert(outputCollector.frequencyMap.isEmpty, "Expected no words in output for empty input")
  }
}
