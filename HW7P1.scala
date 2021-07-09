object HW7P1 {
  //Implementation of a BigInt class in scala for #'s sometimes too big for the integer variable to handle

    class Biguint (num: List[Int]){
      //passing a list into the class initializes it by putting it into numlist
          val numList = num

      //No argument constructor
      def this() = this(0::Nil)

      //String converter
      def this (s: String) = this ({
        def convert(s: String): List[Int] = {
          if (s.isEmpty) Nil
          else
            convert(s.tail) ::: List((s.head - '0'))
        }
        convert(s)
      })

      //Overloading addition function
      def +(that: Biguint): Biguint = {
        def adder(num1: List[Int], num2: List[Int], carry: Int): List[Int] = {
          //Base cases (if one or more of the lists are empty)
          if (num1.isEmpty && num2.isEmpty) Nil
          else if (num1.isEmpty) num2
          else if (num2.isEmpty) num1

            //Neither lists are empty so time to add
          else{
            //add the values at each point of the list and apply the carry
            val addOn = num1.head + num2.head + carry
            if (addOn >= 10) {
              //addOn%10 gives new lead number without it being double digit
              addOn%10::adder(num1.tail,num2.tail,1)
            } else {
              addOn::adder(num1.tail,num2.tail,0)
            }
          }
        }
        //Return new list of the 2 added together
        new Biguint(adder(this.numList, that.numList,0))
      }

      def printBig(): Unit ={
        //println(this.numList.reverse)
        for (element <- numList.reverse){
          print(element)
        }
        println()
      }
    }

  def main (args: Array[String]): Unit ={
    val big = new Biguint(List(2,7,4,1))
    big.printBig()

    val big2 = new Biguint("1472")
    big2.printBig()

    val big3 = big + big2
    big3.printBig()
  }

}
