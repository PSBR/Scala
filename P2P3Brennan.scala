object P2P3Brennan {
  //create a new list alternating the elements of 2 different lists
  def alternatingList(x: List[Int], y: List[Int]) :List[Int]= x match {
    case first::rest => first::alternatingList(y, rest)
    case _ => y
  }

  //find the last instance of an element in a list
  def findLast(xs: List[Int], x: Int) :Int = {
      def inner(index: Int, ls: List[Int], count: Int) :Int = {
        ls match{
          case Nil => index
          case y::ys => if (y==x) inner(count,ys,count+1)
                      else inner(index,ys,count+1)

        }
      }
      inner(-1,xs,0)
  }

  def main(args: Array[String]): Unit ={
    def arr = List(1,2,3,4)
    def arr2 = List(7,8,9,10,11,10)

    println(alternatingList(arr,arr2))

    println(findLast(arr,3))
    println(findLast(arr2,10))
    println(findLast(arr2,2))
  }
}
