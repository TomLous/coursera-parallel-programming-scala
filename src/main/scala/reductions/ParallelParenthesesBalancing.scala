package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer (new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
    */
  def balance(chars: Array[Char]): Boolean = {
    def checkOpenClose(chars: Array[Char], openCount: Int): Boolean = {
      if (chars.isEmpty) openCount == 0
      else if (openCount < 0) false
      else if (chars.head == '(') checkOpenClose(chars.tail, openCount + 1)
      else if (chars.head == ')') checkOpenClose(chars.tail, openCount - 1)
      else checkOpenClose(chars.tail, openCount)

    }

    checkOpenClose(chars, 0)
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
    */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int): (Int, Int) = {
      // God I hate this imperative coding for parallelism's sake!
      var i = idx
      var begin = 0
      var end = 0
      var switched = false

      while (i < until){
        if(begin < 0){
          switched = true
        }else{
          switched = false
        }

        if(chars(i) == '('){
          if(switched){
            end = end + 1
          }else{
            begin = begin + 1
          }

         // if(switched) end = end + 1 else begin = begin + 1
        }
        if(chars(i) == ')')
        {
          if (switched) {
            end = end - 1
          }else{
            begin = begin - 1
          }
          //if(switched) end = end - 1 else begin = begin - 1
        }

        i = i + 1
      }

      (begin,  end)

    }

    /*
                      (0,0 )
                ((test)(c+x)-4)((((4)444)---)xx(x)x)
                               / \
                  (0,3)  =>         (-3, 0)
            ((test)(c+x)-4)(((    (4)444)---)xx(x)x)

       (2,0)    (-2,3)                (-1,0)   (-2, 0)
         /             \                /      \
    ((test)(c     +x)-4)((()(       (4)444)--      -)xx(x)x)





    */



    def reduce(from: Int, until: Int): (Int, Int) = {
      if(until - from <= threshold) traverse(from, until)
      else {
        val mid = from + (until - from) / 2
        val (pair1, pair2) = parallel(reduce(from, mid), reduce(mid, until))

        // This is my best piece of code ever
        if(pair1._1 < 0 && pair2._1 > 0) (pair1._1 , pair2._1 + pair1._2 + pair2._2)
        else if(pair2._1 < 0 && pair1._2 > 0) (pair1._1 + pair2._1 + pair1._2 ,  + pair2._2)
        else (pair1._1 + pair2._1, pair1._2 + pair2._2)
      }
    }

    val res = reduce(0, chars.length)
    res._1 + res._2 == 0 && res._1 >= 0
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
