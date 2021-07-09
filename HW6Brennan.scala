
object HW6Brennan {

  //Applys a function on 2 integers coming from 2 different lists and returning the result of each function call in a new list
  def apply_f(xs: List[Int], ys: List[Int], f: (Int,Int) => Int): List[Int] = {
    if(xs.isEmpty || ys.isEmpty) Nil
    else f(xs.head,ys.head)::apply_f(xs.tail,ys.tail,f)
  }

  //curried version of the first function
  def curried_applyf(f: (Int,Int)=>Int): (List[Int],List[Int]) => List[Int] ={
    (xs: List[Int], ys: List[Int]) =>
      if(xs.isEmpty || ys.isEmpty) Nil
      else f(xs.head,ys.head)::apply_f(xs.tail,ys.tail,f)
  }

  //map function which takes in a list and applies a function to each element returning the results in a new list
  def my_map(xs: List[Int], f: (Int) => Int): List[Int] = {
    xs match {
      case Nil => Nil
      case y::ys => f(y)::my_map(ys,f)
    }
  }

  //curried version
  def curried_mymap(f:Int => Int): List[Int] => List[Int] = {
    (xs: List[Int]) =>
     if(xs.isEmpty) Nil
     else f(xs.head)::curried_mymap(f)(xs.tail)
  }

  //takes in 2 functions and applys them to a descending list of #'s (ex: apply_combine(square,4,add) -> 30 = 16+9+4+1)
  def apply_combine(f:Int=>Int, x: Int, g: (Int,Int)=>Int): Int = {
    if (x==1) f(x)
    else g(f(x), apply_combine(f,x-1,g))
  }

  //curried
  def apply_combine2(f:Int=>Int): Int => (((Int,Int)=>Int)=>Int) =
    (x: Int) => {
      (g: (Int,Int) => Int) => {
        apply_combine(f,x,g)
      }
    }

  def apply_combine3(f:Int=>Int): Int => ((Int,Int)=>Int) => Int = {
    def input(x:Int): ((Int,Int)=>Int)=> Int = {
      def combine_func(g:(Int,Int)=>Int): Int = {
        if (x==1) f(x)
        else g(f(x), apply_combine(f,x-1,g))
      }
      combine_func
    }
    input
  }


  def main(args: Array[String]): Unit ={
    println("Problem 2: ")
    println(apply_f(List(3,8,1,5),List(12,6,23,1,8,4),(x,y)=>x+y))

    println("Problem 3: ")
    println(curried_applyf((x,y)=>x+y)(List(3,8,1,5),List(12,6,23,1,8,4)))

    println("Problem 4: ")
    println(curried_mymap((x)=>x*x)(List[1,2,3,4])

    println("Problem 5: ")
    println(apply_combine2(x=>x*x)(4)((x,y)=>x+y))
    println(apply_combine3(x=>x*x)(4)((x,y)=>x+y))



  }
}
