package reductions

import org.scalameter._
import common._

object LineOfSightRunner {
  
  val standardConfig = config(
    Key.exec.minWarmupRuns -> 10, //40,
    Key.exec.maxWarmupRuns -> 20, //80,
    Key.exec.benchRuns -> 20, //100,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]) {
    val length = 10000000
    val threshold =  100 // 10000
    val input = (0 until length).map(_ % 100 * 1.0f).toArray
    val output = new Array[Float](length + 1)
    val seqtime = standardConfig measure {
      LineOfSight.lineOfSight(input, output)
    }
    println(s"sequential time: $seqtime ms")

    val partime = standardConfig measure {
      LineOfSight.parLineOfSight(input, output, threshold )
    }
    println(s"parallel time: $partime ms")
    println(s"speedup: ${seqtime / partime}")
  }
}

object LineOfSight {

  def max(a: Float, b: Float): Float = if (a > b) a else b

  def lineOfSight(input: Array[Float], output: Array[Float]): Unit = {
    input.zipWithIndex.foreach{
      case (xs, 0) => output(0) = 0
      case (xs, i) => output(i) = Math.max(xs / i, output(i-1))
    }
  }

  sealed abstract class Tree {
    def maxPrevious: Float
//    override def toString(): String
  }

  case class Node(left: Tree, right: Tree) extends Tree {
    val maxPrevious = max(left.maxPrevious, right.maxPrevious)
//     override def toString(): String = " (" + left + ")|(" + right + ")> ["+maxPrevious+"]  "

  }

  case class Leaf(from: Int, until: Int, maxPrevious: Float) extends Tree {
//     override def toString(): String = maxPrevious.toString
  }

  /** Traverses the specified part of the array and returns the maximum angle.
   */
  def upsweepSequential(input: Array[Float], from: Int, until: Int): Float = {
    (from until until).
      foldLeft(0f)(
        (curMax, i) => List(input(i) / i, curMax).max
      )

// Imperative, but better performant?
//    var i = from
//    var max:Float = 0
//    while (i < until){
//      max = Math.max(max, input(i) / i)
//      i = i + 1
//    }
//    max
  }

  /** Traverses the part of the array starting at `from` and until `end`, and
   *  returns the reduction tree for that part of the array.
   *
   *  The reduction tree is a `Leaf` if the length of the specified part of the
   *  array is smaller or equal to `threshold`, and a `Node` otherwise.
   *  If the specified part of the array is longer than `threshold`, then the
   *  work is divided and done recursively in parallel.
   */
  def upsweep(input: Array[Float], from: Int, end: Int,
    threshold: Int): Tree = {
//    println(from + " - " + end )
    //Leaf(from, end, upsweepSequential(input, from, end))

    if(end - from <= threshold) Leaf(from, end, upsweepSequential(input, from, end))
    else {
      val mid = from + (end - from) / 2
      val (left, right) = parallel(
        upsweep(input, from, mid, threshold),
        upsweep(input, mid, end, threshold))
      Node (left, right)
    }
  }

  /** Traverses the part of the `input` array starting at `from` and until
   *  `until`, and computes the maximum angle for each entry of the output array,
   *  given the `startingAngle`.
   */
  def downsweepSequential(input: Array[Float], output: Array[Float],
    startingAngle: Float, from: Int, until: Int): Unit = {
    def calculateOutput(i: Int, end: Int, max:Float): Unit = {
      if(i < end){
        output(i) = List(input(i) / i, max).max
        calculateOutput(i+1, end, output(i))
      }
    }

    calculateOutput(from, until, startingAngle)

    // More convoluted then recursive solution IMHO  / performance diff?
    //    (from until until).foreach{ i =>
    //      if(i == 0) output(0) = 0
    //      else if(i == from) output(i) = Math.max(input(i) / i, startingAngle)
    //      else output(i) = Math.max(input(i) / i, output(i-1))
    //    }
  }

  /** Pushes the maximum angle in the prefix of the array to each leaf of the
   *  reduction `tree` in parallel, and then calls `downsweepTraverse` to write
   *  the `output` angles.
   */
  def downsweep(input: Array[Float], output: Array[Float], startingAngle: Float,
    tree: Tree): Unit = {
    tree match {
      case Leaf(from, until, _) => downsweepSequential(input, output, startingAngle, from, until)
      case Node(left, right) => {
        parallel(
          downsweep(input, output, startingAngle, left),    // 0
          downsweep(input, output, left.maxPrevious max startingAngle,right))  // 6
      }
    }
  }

  /*
                   6
            /                 \
           6                    7
         /     \            /       \
        6      4          7         2
       /        \         /         \
    2 3 6     1 2 4     3 7 2     1 1 2
   */


  /** Compute the line-of-sight in parallel. */
  def parLineOfSight(input: Array[Float], output: Array[Float],
    threshold: Int): Unit = {
//    println("up")
    val tree = upsweep(input, 0, input.length, threshold)
//    println("down")
//    println(input.toList)
//    println(tree)
    downsweep(input, output, 0f, tree)
  }
}
