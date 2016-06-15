object test{


  traverse(0,8,0,0,"((test)(c+x)-4)((((4)444)---)xx(x)x)".toCharArray)
  traverse(8,18,0,0,"((test)(c+x)-4)((((4)444)---)xx(x)x)".toCharArray)
  traverse(18,26,0,0,"((test)(c+x)-4)((((4)444)---)xx(x)x)".toCharArray)
  traverse(26,36,0,0,"((test)(c+x)-4)((((4)444)---)xx(x)x)".toCharArray)


  def traverse(idx: Int, until: Int, startOpen: Int, startClosed: Int, chars: Array[Char]): (Int, Int) = {

    println("\n------")

    var i = idx
    var begin = 0
    var end = 0
    var switched = false

    while (i < until) {
      print(chars(i))
      if (chars(i) == '(') {
        if (begin < 0) {
          switched = true
        }

        if (switched) end = end + 1 else begin = begin + 1
      }
      if (chars(i) == ')') {
        if (begin > 0) {
          switched = true

        }

        if (switched) end = end - 1 else begin = begin - 1
      }

      i = i + 1
    }

    println("\n------")

    (begin, end)
  }
}