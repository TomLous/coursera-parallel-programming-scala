import common._

import scalashop._


object test {
//  val src = new Img(15, 15)
//
//  for (x <- 0 until 5; y <- 0 until 5)
//    src(x, y) = rgba(x, y, x + y, math.abs(x - y))

//  boxBlurKernel(src, 0, 0, 2)

//  val from = 10
//  val end = 15
//  for(w <- from until end) yield w

  val r =  Range(0,5) by Math.max(1/24,1)
  val tasks = for(
    z <- r
  ) yield {
    val b = task{
      r.sum
    }
  }
//  b.join()


  val w = 4
  val h = 3
  val src = new Img(w, h)
  val dst = new Img(w, h)
  src(0, 0) = 0; src(1, 0) = 1; src(2, 0) = 2; src(3, 0) = 9
  src(0, 1) = 3; src(1, 1) = 4; src(2, 1) = 5; src(3, 1) = 10
  src(0, 2) = 6; src(1, 2) = 7; src(2, 2) = 8; src(3, 2) = 11

  blur(src, dst, 0, 4, 2)

  def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int) = {

    val pixels = {
      for (
        i <- -radius to radius;
        j <- -radius to radius
      ) yield (scalashop.clamp(x + i, 0, src.width - 1), scalashop.clamp(y + j, 0, src.height - 1))
    }.distinct.map({
      case (x, y) =>
        val pixel = src(x, y)
        (red(pixel), green(pixel), blue(pixel), alpha(pixel))
    })

    rgba(
      pixels.map(_._1).sum / pixels.length,
      pixels.map(_._2).sum / pixels.length,
      pixels.map(_._3).sum / pixels.length,
      pixels.map(_._4).sum / pixels.length
    )


  }

  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit = {
    // TODO implement this method using the `boxBlurKernel` method
    for(
      x <- from until end;
      y <- 0 until src.height;
      if x >= 0 && x < src.width
    ) yield {

      println(x +", " + y + " = "+ src(x,y) + " => " + boxBlurKernel(src, x, y, radius))
      dst.update(x, y,  boxBlurKernel(src, x, y, radius))
    }
    dst
  }


}