package newhellotest

import org.apache.commons.collections4.queue.CircularFifoQueue
import scala.language.unsafeNulls
import scala.collection.mutable
import sun.misc.{Signal, SignalHandler}
import mainargs.{main, arg, ParserForMethods}
import org.log4s._
import java.awt._
import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import scala.util.Random
import java.io.File
import scala.collection.immutable.List // Import the correct List
import scala.jdk.CollectionConverters._

object newMain:

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
    @arg(short = 'i', doc = "path to ignore file") ignore_file: Option[String] = None
  ) = {

    // Handle SIGPIPE signal by exiting
    Signal.handle(new Signal("PIPE"), new SignalHandler {
      override def handle(sig: Signal): Unit = {
        System.err.println("SIGPIPE detected. Terminating.")
        System.exit(0)
      }
    })

    // Ignore file
    val ignore: Set[String] = if (ignore_file.isDefined) {
      scala.io.Source.fromFile(ignore_file.get).getLines().map(_.toLowerCase).toSet
    } else {
      Set.empty[String]
    }

    // Set up input Scanner
    val lines = scala.io.Source.stdin.getLines
    val words =
      lines.flatMap(l => l.split("(?U)[^\\p{Alpha}0-9']+")).map(_.toLowerCase).filter(word => !ignore.contains(word))

    // Call WordCloud with given words and arguments
    WordCloud.processing(words, cloud_size, length_at_least, window_size, every_K, min_frequency, WordCloud.myOutputSink)

    val logger = org.log4s.getLogger
    logger.debug(f"Cloud Size = $cloud_size Length At Least = $length_at_least Window Size = $window_size Every K = $every_K Min Frequency = $min_frequency")
  }

end newMain

object WordCloud {

def processing(words: Iterator[String], cloud_size: Int, length_at_least: Int, window_size: Int, every_K: Int, min_frequency: Int, outputSink: OutputSink): Unit = {

  // Function to process words recursively
  def processWords(remainingWords: Iterator[String], queue: List[String], steps: Int): Unit = {
    if (remainingWords.hasNext) {
      val word = remainingWords.next()

      // Only process the word if it meets the length requirement
      if (word.length >= length_at_least) {
        // Update the queue (add the new word and maintain the size)
        val updatedQueue = (word :: queue).takeRight(window_size)

        // Increment steps
        val newSteps = steps + 1

        // Check if the queue is full and steps are sufficient
        if (updatedQueue.size == window_size && newSteps >= every_K) {
          // Convert List[String] to CircularFifoQueue[String]
          val circularQueue = new CircularFifoQueue[String](updatedQueue.asJava)
          fullQueue(circularQueue, cloud_size, min_frequency, outputSink) // Pass CircularFifoQueue to fullQueue
          
          // Reset steps after processing
          processWords(remainingWords, updatedQueue, 0)
        } else {
          // Continue processing with updated queue and steps
          processWords(remainingWords, updatedQueue, newSteps)
        }
      } else {
        // Continue processing without modifying queue or steps
        processWords(remainingWords, queue, steps)
      }
    }
  }

  // Start processing words with an empty queue and zero steps
  processWords(words, List.empty[String], 0)
}

  // Separate I/O and logic by creating OutputSink
  trait OutputSink {
    def doOutput(value: Seq[(String, Int)]): Unit
  }

  // Create OutputSink instance that prints the output of fullQueue. This separates I/O from logic
  object myOutputSink extends OutputSink {
    def doOutput(value: Seq[(String, Int)]) = {
      try {
        val out = value.map { case (word, count) => s"$word: $count" }.mkString(" ")
        println(out)
        visualizeWordCloud(value)            // Call the word cloud visualization
        visualizeWordCloudBarChart(value)    // Call the bar chart visualization
      } catch {
        case _: java.io.IOException =>
          System.err.println("Broken pipe error. Exiting")
          System.exit(0)
      }
    }
  }

  // Function to process full queue
  def fullQueue(queue: CircularFifoQueue[String], cloud_size: Int, min_frequency: Int, output: OutputSink): Unit = {

    // Create a variable 'frequency', which is a mutable map of a string and integer
    val frequency = mutable.Map[String, Int]()

    // For each word in the current queue: if the string is not in 'frequency', set the word frequency to 0. Add 1 to the frequency.
    queue.forEach { word =>
      frequency(word) = frequency.getOrElse(word, 0) + 1
    }

    // Sort by descending frequency and take the first c pairs
    val sortedFrequency: Seq[(String, Int)] = frequency.toSeq.sortBy(-_._2).filter { case (_, count) => count >= min_frequency }.take(cloud_size)

    output.doOutput(sortedFrequency)
  }

  // Method to create a classic NLP-style word cloud with border, background, and random word colors
  def visualizeWordCloud(words: Seq[(String, Int)]): Unit = {
    val imageWidth = 800
    val imageHeight = 600
    val bufferedImage = new BufferedImage(imageWidth, imageHeight, BufferedImage.TYPE_INT_ARGB)
    val graphics = bufferedImage.createGraphics()

    // Background color
    graphics.setColor(new Color(230, 240, 255)) // Light blue background
    graphics.fillRect(0, 0, imageWidth, imageHeight)

    // Border color and drawing
    graphics.setColor(Color.BLACK)
    graphics.drawRect(0, 0, imageWidth - 1, imageHeight - 1) // Draw a border around the image

    val maxFontSize = 48
    val minFontSize = 12
    val maxFrequency = words.headOption.map(_._2).getOrElse(1)
    val random = new Random()

    words.foreach { case (word, count) =>
      // Calculate font size based on word frequency
      val fontSize = minFontSize + ((maxFontSize - minFontSize) * (count.toFloat / maxFrequency)).toInt
      graphics.setFont(new Font("SansSerif", Font.BOLD, fontSize))

      // Set a random color for each word
      val wordColor = new Color(random.nextInt(256), random.nextInt(256), random.nextInt(256))
      graphics.setColor(wordColor)

      // Position words randomly within the image boundaries
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

  // Method to create a bar chart of word frequencies
  def visualizeWordCloudBarChart(words: Seq[(String, Int)]): Unit = {
    val dataset = new org.jfree.data.category.DefaultCategoryDataset()

    words.foreach { case (word, count) =>
      dataset.addValue(count, "Frequency", word)
    }

    val chart = org.jfree.chart.ChartFactory.createBarChart(
      "Word Frequency",
      "Words",
      "Frequency",
      dataset
    )

    // Create a BufferedImage and draw the chart on it
    val bufferedImage = chart.createBufferedImage(800, 600)
    try {
      val success = ImageIO.write(bufferedImage, "png", new File("word_cloud_frequency_bar_chart.png"))
      if (!success) {
        println("Failed to save the bar chart image.")
      }
    } catch {
      case e: Exception => e.printStackTrace()
    }
  }
}
