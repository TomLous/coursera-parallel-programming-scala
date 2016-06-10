

import scalashop._


object test {
  val src = new Img(15, 15)

  for (x <- 0 until 5; y <- 0 until 5)
    src(x, y) = rgba(x, y, x + y, math.abs(x - y))

  boxBlurKernel(src, 0, 0, 2)

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


    //    pixels.map[(Int,Int)]((x,y)=>{
    //
    //    })
//
//    val sum = pixels.unzip match {case(r,g,b,a) => {
//      (a)
//        }}
//
// sum

    //pixels.foldLeft((0,0,0,0),0){ case (sum, next) => ((sum._1 + next._1, sum._2 + next._2, sum._3 + next._3, sum._4 + next._2), 1) }

    //


  }


}