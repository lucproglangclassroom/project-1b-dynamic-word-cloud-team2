package newhellotest

import scala.io.Source
import scopt.OParser
import java.io.File
import org.jfree.chart.{ChartFactory, ChartUtils}
import org.jfree.data.category.DefaultCategoryDataset
import javax.imageio.ImageIO
import java.awt.image.BufferedImage
import org.slf4j.LoggerFactory
import scala.runtime.stdLibPatches.Predef.nn
import sun.misc.Signal
import sun.misc.SignalHandler

case class WordCloudConfig(
  cloudSize: Int = 10,
  minLength: Int = 6,
  windowSize: Int = 1000,
  ignoreListFile: Option[String] = None,
  minFrequency: Int = 1,
  updateFrequency: Int = 1
)

trait WordCloudProcessor {
  def addWord(word: String): WordCloudProcessor
  def isReady: Boolean
  def getTopWords: List[(String, Int)]
}

case class DynamicWordCloud(
  cloudSize: Int,
  minLength: Int,
  windowSize: Int,
  ignoreList: Set[String],
  minFrequency: Int,
  wordCounts: Map[String, Int] = Map.empty,
  wordQueue: List[String] = List.empty
) extends WordCloudProcessor {

  override def addWord(word: String): DynamicWordCloud = {
    if (ignoreList.contains(word) || word.length < minLength) {
      this
    } else {
      val updatedQueue = (word :: wordQueue).take(windowSize)
      val updatedCounts = updatedQueue.foldLeft(wordCounts) { (counts, w) =>
        counts.updated(w, counts.getOrElse(w, 0) + 1)
      }

      val newCounts = if (updatedQueue.size > windowSize) {
        val oldestWord = updatedQueue.last
        val newCount = updatedCounts.getOrElse(oldestWord, 1) - 1
        if (newCount > 0) {
          updatedCounts.updated(oldestWord, newCount)
        } else {
          updatedCounts - oldestWord
        }
      } else {
        updatedCounts
      }

      this.copy(wordCounts = newCounts, wordQueue = updatedQueue)
    }
  }

  override def isReady: Boolean = wordQueue.size >= windowSize

  override def getTopWords: List[(String, Int)] = {
    wordCounts.filter { case (_, count) => count >= minFrequency }
      .toList
      .sortBy(-_._2)
      .take(cloudSize)
  }
}

trait WordCloudVisualizer {
  def visualizeWordCloud(words: List[(String, Int)], outputFile: String): Unit
}

trait WordCloudRunner {
  def runWordCloud(config: WordCloudConfig): Unit
}

object newMain extends WordCloudVisualizer with WordCloudRunner {
  val logger = LoggerFactory.getLogger(newMain.getClass)

  def main(args: Array[String]): Unit = {
    // Handle SIGPIPE to prevent termination on broken pipe
    Signal.handle(new Signal("PIPE"), new SignalHandler {
      override def handle(signal: Signal): Unit = {
        // Do nothing or log a message if needed
        logger.nn.warn("Received SIGPIPE, ignoring.")
      }
    })

    System.setProperty("java.awt.headless", "true")
    val config = parseArguments(args).getOrElse {
      sys.exit(1)
    }

    logger.nn.debug(
      s"cloudSize=${config.cloudSize} minLength=${config.minLength} windowSize=${config.windowSize} " +
        s"updateFrequency=${config.updateFrequency} minFrequency=${config.minFrequency}"
    )

    runWordCloud(config)
  }

  def parseArguments(args: Array[String]): Option[WordCloudConfig] = {
    val builder = OParser.builder[WordCloudConfig]
    val parser = {
      import builder._
      OParser.sequence(
        programName("topwords"),
        opt[Int]('c', "cloud-size")
          .action((x, c) => c.copy(cloudSize = x))
          .text("size of the word cloud (default: 10)"),
        opt[Int]('l', "length-at-least")
          .action((x, c) => c.copy(minLength = x))
          .text("minimum length of words to consider (default: 6)"),
        opt[Int]('w', "window-size")
          .action((x, c) => c.copy(windowSize = x))
          .text("size of the moving window (default: 1000)"),
        opt[String]('i', "ignore-list")
          .action((x, c) => c.copy(ignoreListFile = Some(x)))
          .text("file containing words to ignore"),
        opt[Int]('f', "min-frequency")
          .action((x, c) => c.copy(minFrequency = x))
          .text("minimum frequency for a word to be included in the cloud (default: 1)"),
        opt[Int]('u', "update-frequency")
          .action((x, c) => c.copy(updateFrequency = x))
          .text("number of steps before updating the word cloud (default: 1)")
      )
    }

    OParser.parse(parser, args, WordCloudConfig()) match {
      case Some(config) if validateConfig(config) => Some(config)
      case Some(config) =>
        logger.nn.error("Invalid configuration values.")
        None
      case _: None.type => None
    }
  }

  private def validateConfig(config: WordCloudConfig): Boolean = {
    config.cloudSize > 0 &&
    config.minLength >= 0 &&
    config.windowSize > 0 &&
    config.minFrequency >= 0 &&
    config.updateFrequency > 0
  }

  override def runWordCloud(config: WordCloudConfig): Unit = {
    val ignoreList = config.ignoreListFile.map(readIgnoreList).getOrElse(Set.empty)
    val initialWordCloud = DynamicWordCloud(config.cloudSize, config.minLength, config.windowSize, ignoreList, config.minFrequency)
    val lines = Source.stdin.getLines()

    var stepCount = 0
    var currentWordCloud: WordCloudProcessor = initialWordCloud

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
                currentWordCloud = currentWordCloud.addWord(lowerWord.nn)
              case _ => // Ignore null or empty words
            }
            index += 1
          }

          stepCount += 1

          if (stepCount >= config.updateFrequency && currentWordCloud.isReady) {
            println(currentWordCloud.getTopWords.map { case (word, count) => s"$word: $count" }.mkString(" "))
            visualizeWordCloud(currentWordCloud.getTopWords, "word_cloud_newMain.png")
            stepCount = 0
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

  override def visualizeWordCloud(words: List[(String, Int)], outputFile: String): Unit = {
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

    val bufferedImage = chart.nn.createBufferedImage(800, 600)
    try {
      val success = ImageIO.write(bufferedImage, "png", new File(outputFile))
      if (!success) {
        println("Failed to save the chart image.")
      }
    } catch {
      case e: Exception =>
        println(s"Error while saving the chart image: ${e.getMessage}")
    }
  }
}
