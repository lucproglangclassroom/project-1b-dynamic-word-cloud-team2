package hellotest

import scala.io.Source
import scopt.OParser
import java.io.File
import scala.collection.mutable
import org.jfree.chart.{ChartFactory, ChartUtils}
import org.jfree.data.category.DefaultCategoryDataset
import javax.imageio.ImageIO
import java.awt.image.BufferedImage
import org.slf4j.LoggerFactory
import scala.runtime.stdLibPatches.Predef.nn

case class Config(
  cloudSize: Int = 10,
  minLength: Int = 6,
  windowSize: Int = 1000,
  ignoreListFile: Option[String] = None,
  minFrequency: Int = 1,
  updateFrequency: Int = 1 // New field for update frequency
)

object Main {
  val logger = LoggerFactory.getLogger(Main.getClass)

  def main(args: Array[String]): Unit = {
    System.setProperty("java.awt.headless", "true") // Enable headless mode for environments without GUI
    val config = parseArguments(args).getOrElse {
      sys.exit(1) // Exit if argument parsing fails
    }
 
    // Log the parameters
    logger.nn.debug(
      s"howMany=${config.cloudSize} minLength=${config.minLength} lastNWords=${config.windowSize} " +
        s"everyKSteps=${config.updateFrequency} minFrequency=${config.minFrequency}"
    )

    processInput(config)
  }

  def parseArguments(args: Array[String]): Option[Config] = {
    val builder = OParser.builder[Config]
    val parser = {
      import builder._
      OParser.sequence(
        programName("topwords"),
        opt[Int]('c', "cloud-size")
          .validate(x => if (x <= 0) failure("Cloud size must be positive.") else success)
          .action((x, c) => c.copy(cloudSize = x))
          .text("size of the word cloud (default: 10)"),
        opt[Int]('l', "length-at-least")
          .validate(x => if (x < 0) failure("Minimum length must be non-negative.") else success)
          .action((x, c) => c.copy(minLength = x))
          .text("minimum length of words to consider (default: 6)"),
        opt[Int]('w', "window-size")
          .action((x, c) => c.copy(windowSize = x))
          .text("size of the moving window (default: 1000)"),
        opt[String]('i', "ignore-list")
          .action((x, c) => c.copy(ignoreListFile = Some(x)))
          .text("file containing words to ignore"),
        opt[Int]('f', "min-frequency")
          .validate(x => if (x < 1) failure("Minimum frequency must be at least 1.") else success)
          .action((x, c) => c.copy(minFrequency = x))
          .text("minimum frequency for a word to be included in the cloud (default: 1)"),
        opt[Int]('u', "update-frequency")
          .action((x, c) => c.copy(updateFrequency = x))
          .text("number of steps before updating the word cloud (default: 1)"),
        help("help") // Help option
      )
    }

    OParser.parse(parser, args, Config()) match {
      case Some(config) => 
        println(s"Parsed configuration: $config") // Debug output to check parsed values
        Some(config)
      case _ => None // Invalid arguments result in None
    }
  }



  def processInput(config: Config): Unit = {
    val ignoreList = config.ignoreListFile.map(readIgnoreList).getOrElse(Set.empty)
    val wordCloud = new WordCloud(config.cloudSize, config.minLength, config.windowSize, ignoreList, config.minFrequency)
    val lines = Source.stdin.getLines()

    var stepCount = 0 // Step counter

    while (lines.hasNext) {
      val line = lines.next()
      val splitWordsOpt = Option(line.split("(?U)[^\\p{Alpha}0-9']+"))

      splitWordsOpt match {
        case Some(wordsArray) =>
          val length = wordsArray.nn.length
          var index = 0

          while (index < length) {
            Option(wordsArray.nn(index)) match {
              case Some(word: String) if word.nonEmpty =>
                val lowerWord = word.toLowerCase
                wordCloud.addWord(lowerWord.nn)
              case _ => // Ignore null or empty words
            }
            index += 1
          }

          stepCount += 1 // Increment step count

          // Update word cloud output if stepCount reaches updateFrequency
          if (stepCount >= config.updateFrequency && wordCloud.isReady) {
            println(wordCloud.getTopWords.map { case (word, count) => s"$word: $count" }.mkString(" "))
            visualizeWordCloud(wordCloud.getTopWords) // Generate and save the word cloud visualization
            stepCount = 0 // Reset step count
          }

        case _ =>
          println("No words to process.")
      }
    }
  }

  def readIgnoreList(filePath: String): Set[String] = {
    Source.fromFile(new File(filePath)).getLines()
      .flatMap(line => Option(line).map(_.toLowerCase.nn))
      .toSet
  }

  def visualizeWordCloud(words: List[(String, Int)]): Unit = {
    val dataset = new DefaultCategoryDataset()

    words.foreach { case (word, count) =>
      dataset.addValue(count, "Frequency", word)
    }

    val chart = ChartFactory.createBarChart(
      "Word Frequency",
      "Words",
      "Frequency",
      dataset
    )

    // Create a BufferedImage and draw the chart on it
    val bufferedImage = chart.nn.createBufferedImage(800, 600)
    try {
      val success = ImageIO.write(bufferedImage, "png", new File("word_cloud.png"))
      if (!success) {
        println("Failed to save the word cloud image.")
      }
    } catch {
      case e: Exception => e.printStackTrace()
    }
  }
}

class WordCloud(maxSize: Int, minLength: Int, windowSize: Int, ignoreList: Set[String], minFrequency: Int) {
  private var wordCounts = Map.empty[String, Int]
  private var wordOrder = List.empty[String]

  def addWord(word: String): Unit = {
    // Use Option to handle the potential null input
    Option(word).foreach { w =>
      val normalizedWord = w.toLowerCase // Normalize the word to lowercase
      if (!ignoreList.contains(normalizedWord.nn) && normalizedWord.nn.length >= minLength) {
        // Ensure normalizedWord is treated as a String
        val count = wordCounts.getOrElse(normalizedWord.nn, 0) + 1
        wordCounts = wordCounts.updated(normalizedWord.nn, count)

        // Maintain the word order and remove the oldest word if necessary
        wordOrder = (normalizedWord.nn :: wordOrder).distinct
        if (wordOrder.size > windowSize) {
          val oldestWord = wordOrder.last
          wordCounts -= oldestWord
          wordOrder = wordOrder.init // Remove last element
        }
      }
    }
  }

  def getTopWords: List[(String, Int)] = {
    wordCounts.filter(_._2 >= minFrequency).toList.sortBy(-_._2).take(maxSize)
  }

  def isReady: Boolean = wordOrder.size >= windowSize // Use wordOrder instead of wordQueue
}
