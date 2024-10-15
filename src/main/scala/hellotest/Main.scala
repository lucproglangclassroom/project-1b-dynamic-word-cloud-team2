package hellotest

import org.apache.commons.collections4.queue.CircularFifoQueue
import scala.language.unsafeNulls
import scala.collection.mutable
import sun.misc.{Signal, SignalHandler}
import mainargs.{main, arg, ParserForMethods, Flag}
import org.log4s._
import org.jfree.data.category.DefaultCategoryDataset
import org.jfree.chart.ChartFactory
import javax.imageio.ImageIO
import java.io.File
import java.awt.{Color, Font, Graphics2D}
import java.awt.image.BufferedImage
import java.util.Random

object Main:

  // Default values for arguments
  def argValidation(cloud_size: Int, length_at_least: Int, window_size: Int, min_frequency: Int, every_K: Int): Unit = {
    if (cloud_size < 1 || length_at_least < 1 || window_size < 1 || min_frequency < 1 || every_K < 1) {
      throw new NumberFormatException("Arguments should be natural numbers")
    }
  }

  def main(args: Array[String]): Unit = {
    try {
      ParserForMethods(this).runOrExit(args.toIndexedSeq)
    } catch {
      case e: NumberFormatException =>
        System.err.println(e.getMessage)
        System.exit(4)
    }
  }

  @main
  def run(
      @arg(short = 'c', doc = "size of the sliding word cloud") cloud_size: Int = 10,
      @arg(short = 'l', doc = "minimum word length to be considered") length_at_least: Int = 6,
      @arg(short = 'w', doc = "size of the sliding FIFO queue") window_size: Int = 1000,
      @arg(short = 'k', doc = "number of steps between word cloud updates") every_K: Int = 10,
      @arg(short = 'f', doc = "minimum frequency for a word to be included in the cloud") min_frequency: Int = 3,
      @arg(short = 'i', doc = "path to ignore file") ignore_file: Option[String] = None,
      @arg(short = 't', doc = "path to input text file") input_file: Option[String] = None) = {

    try {
      Signal.handle(new Signal("PIPE"), _ => {
        System.err.println("SIGPIPE detected. Terminating.")
        System.exit(0)
      })
    } catch {
      case e: IllegalArgumentException =>
        System.err.println("Signal handling not supported on this platform.")
    }

    val ignore: Set[String] = try {
      ignore_file match {
        case Some(path) => scala.io.Source.fromFile(path).getLines().map(_.toLowerCase).toSet
        case None => Set.empty[String]
      }
    } catch {
      case e: Exception =>
        System.err.println(s"Error reading ignore file: ${e.getMessage}")
        Set.empty[String]
    }

    val lines = input_file match {
      case Some(path) => scala.io.Source.fromFile(path).getLines()
      case None => scala.io.Source.stdin.getLines()
    }
    val words =
      lines.flatMap(l => l.split("(?U)[^\\p{Alpha}0-9']+")).map(_.toLowerCase).filter(word => !ignore.contains(word))

    WordCloud.processing(
      words = words,
      cloud_size = cloud_size,
      length_at_least = length_at_least,
      window_size = window_size,
      every_K = every_K,
      min_frequency = min_frequency,
      outputSink = WordCloud.myOutputSink
    )

    val logger = org.log4s.getLogger
    logger.debug(
      f"Cloud Size = $cloud_size Length At Leasts = $length_at_least Window Size = $window_size Every K = $every_K Min Frequency = $min_frequency"
    )
  }

end Main

object WordCloud {

  def processing(words: Iterator[String], cloud_size: Int, length_at_least: Int, window_size: Int, every_K: Int, min_frequency: Int, outputSink: OutputSink): Unit = {
    words.filter(_.length >= length_at_least).scanLeft((0, List.empty[String])) { case ((steps, queue), word) =>
      val newQueue = (queue :+ word).takeRight(window_size)
      val newSteps = steps + 1

      if (newQueue.size >= window_size && newSteps >= every_K) {
        fullQueue(newQueue, cloud_size, min_frequency, outputSink)
        (0, newQueue)
      } else {
        (newSteps, newQueue)
      }

    }.foreach { _ => () }
  }

  trait OutputSink {
    def doOutput(value: Seq[(String, Int)]): Unit
  }

  object myOutputSink extends OutputSink {
    def doOutput(value: Seq[(String, Int)]) = {
      try {
        val out = value.map { case (word, count) => s"$word: $count" }.mkString(" ")
        println(out)
        visualizeWordCloud(value)
        visualizeWordCloudBarChart(value)
      } catch {
        case _: java.io.IOException =>
          System.err.println("Broken pipe error. Exiting")
          System.exit(0)
      }
    }
  }

  def fullQueue(queue: List[String], cloud_size: Int, min_frequency: Int, output: OutputSink): Unit = {
    val frequency = queue.groupBy(identity).view.mapValues(_.size).toMap
    val sortedFrequency: Seq[(String, Int)] = frequency.toSeq.sortBy(-_._2).filter { case (_, count) => count >= min_frequency }.take(cloud_size)
    output.doOutput(sortedFrequency)
  }

  def visualizeWordCloud(words: Seq[(String, Int)]): Unit = {
    val imageWidth = 800
    val imageHeight = 600
    val bufferedImage = new BufferedImage(imageWidth, imageHeight, BufferedImage.TYPE_INT_ARGB)
    val graphics = bufferedImage.createGraphics()

    graphics.setColor(new Color(230, 240, 255))
    graphics.fillRect(0, 0, imageWidth, imageHeight)
    graphics.setColor(Color.BLACK)
    graphics.drawRect(0, 0, imageWidth - 1, imageHeight - 1)

    val maxFontSize = 48
    val minFontSize = 12
    val maxFrequency = words.headOption.map(_._2).getOrElse(1)
    val random = new Random()

    words.foreach { case (word, count) =>
      val fontSize = minFontSize + ((maxFontSize - minFontSize) * (count.toFloat / maxFrequency)).toInt
      graphics.setFont(new Font("SansSerif", Font.BOLD, fontSize))
      val wordColor = new Color(random.nextInt(256), random.nextInt(256), random.nextInt(256))
      graphics.setColor(wordColor)
      val x = random.nextInt(imageWidth - fontSize * word.length)
      val y = random.nextInt(imageHeight - fontSize)
      graphics.drawString(word, x, y)
    }

    graphics.dispose()

    try {
      val success = ImageIO.write(bufferedImage, "png", new File("word_cloud.png"))
      if (!success) println("Failed to save the word cloud image.")
    } catch {
      case e: Exception => e.printStackTrace()
    }
  }

  def visualizeWordCloudBarChart(words: Seq[(String, Int)]): Unit = {
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

    val bufferedImage = chart.createBufferedImage(800, 600)
    try {
      val success = ImageIO.write(bufferedImage, "png", new File("word_cloud_frequency_bar_chart.png"))
      if (!success) println("Failed to save the bar chart image.")
    } catch {
      case e: Exception => e.printStackTrace()
    }
  }
}
