package newhellotest

import org.apache.commons.collections4.queue.CircularFifoQueue
import scala.language.unsafeNulls
import scala.collection.mutable
import sun.misc.{Signal, SignalHandler} 
import mainargs.{main, arg, ParserForMethods, Flag}
import org.log4s._


object newMain:

  // Default values for arguments


  def argValidation(cloud_size: Int, length_at_least: Int, window_size: Int, min_frequency: Int, every_K: Int): Unit = {
  if (cloud_size<1 || length_at_least<1 || window_size<1 || min_frequency<1 ||every_K<1) {
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
    @arg(short = 'l', doc = "minimum word length to be considere") length_at_least: Int = 6,
    @arg(short = 'w', doc = "size of the sliding FIFO queue") window_size: Int = 1000,
    @arg(short = 'k', doc = "number of steps between word cloud updates") every_K: Int = 10,
    @arg(short = 'f', doc = "minimum frequency for a word to be included in the cloud") min_frequency: Int = 3,
    @arg(short = 'i', doc = "path to ignore file") ignore_file: Option[String] = None) = {

  


    // Handle SIGPIPE signal by exiting 
    Signal.handle(new Signal("PIPE"), new SignalHandler {
      override def handle(sig: Signal): Unit = {
        System.err.println("SIGPIPE detected. Terminating.")
        System.exit(0)
      }
    })


    //Ignore file
    val ignore: Set[String] = if (ignore_file.isDefined) {
      scala.io.Source.fromFile(ignore_file.get).getLines().map(_.toLowerCase).toSet
    } else {
      Set.empty[String]
    }


    // Set up input Scanner
    val lines = scala.io.Source.stdin.getLines
    val words = 
      lines.flatMap(l => l.split("(?U)[^\\p{Alpha}0-9']+")).map(_.toLowerCase).filter(word => !ignore.contains(word)) //.map(_.toLowerCase) satisfies EC for case-insensitivity

    // Call WordCloud with given words and arguements
    WordCloud.processing(words=words, cloud_size=cloud_size, length_at_least=length_at_least, window_size=window_size, every_K=every_K, min_frequency=min_frequency, outputSink=WordCloud.myOutputSink)

    val logger = org.log4s.getLogger
    logger.debug(f"Cloud Size = $cloud_size Length At Leasts = $length_at_least Window Size = $window_size Every K = $every_K Min Frequency = $min_frequency")

    }

end newMain


object WordCloud {

  def processing(words: Iterator[String], cloud_size: Int, length_at_least: Int, window_size: Int, every_K: Int, min_frequency: Int, outputSink: OutputSink): Unit = {

    val queue = new CircularFifoQueue[String](window_size)
    var steps = 0 // Initialize to count steps
    words.filter(_.length >= length_at_least).foreach {word =>

      // Add the word to the queue
      queue.add(word)
      steps += 1 // Increment steps by 1 after word added to queue
      
      // If the queue is full after adding the word AND steps >= k, call the fullQueue function on the queue. Additionally, reset steps.
      if ((queue.isAtFullCapacity) & (steps >= every_K))  {
        steps = 0
        fullQueue(queue,cloud_size, min_frequency, outputSink)
      }
    }
  }

  // Separate I/O and logic by creating OutputSink
  trait OutputSink{
    def doOutput(value: Seq[(String,Int)]): Unit
  }

  // Create OutputSink instance that prints the output of fullQueue. This separates I/O from logic
  object myOutputSink extends OutputSink {
    def doOutput(value: Seq[(String, Int)]) = {
      try {
        val out = value.map {case (word,count) => s"$word: $count" }.mkString(" ")
        println(out)
      } catch {
        case _: java.io.IOException =>
          System.err.println("Broken pipe error. Exiting")
          System.exit(0)
      }
    }
  }

  // Function to process full queue
  def fullQueue(queue: CircularFifoQueue[String], cloud_size: Int, min_frequency: Int, output:OutputSink): Unit = {

    // Create a variable 'frequency', which is a mutable map of a string and integer
    val frequency = mutable.Map[String, Int]() 

    // For each word in the current queue: if the string is not in 'frequency', set the word frequency to 0. Add 1 to the frequency.
    queue.forEach {word => 
      frequency(word) = frequency.getOrElse(word,0) + 1
    }

    // Sort by descending frequency and take the first c pairs
    val sortedfrequency: Seq[(String, Int)] = frequency.toSeq.sortBy(-_._2).filter{case (_,count) => count >=min_frequency}.take(cloud_size)

    output.doOutput(sortedfrequency)

  }

}