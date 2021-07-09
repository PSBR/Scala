class HW9Brennan {
  def forLoop(i: Int, f: Int=>Boolean, t: Int=>Int) (x: => Unit): Unit ={
    if(f(i)){
      x
      forLoop(t(i),f,t)(x)
    }
  }

  def main(args: Array[String]): Unit ={
      println(forLoop(0, x=>x<10, y=>y+1)(println("Hello World")))
  }
}
