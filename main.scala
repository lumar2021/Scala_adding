import scala.math.sqrt

object Main {

  def main(args: Array [String]): Unit =
  {
    //proof example
    val cond = "4 3 condnumero"
    print(solveRPN(cond))
  }
       

    def solveRPN(eqn: String): Double = {
        val items = eqn.split(" ")
        val accumulator = List[Double]()
        items.foldLeft(accumulator)(foldingFunction).head
    }
   
    def foldingFunction (stack: List[Double], a: String): List[Double] = stack match {
        case List() => a.toDouble :: stack   
        case List(_) => a.toDouble :: stack
        case x::y::ys => a match {
            case "*" => x * y :: ys
            case "+" => x + y :: ys
            case "-" => y - x :: ys
            case "/" => y / x :: ys
            case "neg1" => -x :: ys
            case "neg1+" => -x+10 :: ys
            case "neg1*" => -x * 10 :: ys
            case "raiz2" =>sqrt(x) :: ys
            case "condnumero"  => addInt(x)::ys
            case s: String => s.toDouble :: stack
        }
    }
      
  def addInt( a:Double) : Double = {
      var sum:Double = 0
    if(a == 3.0){
       sum = 100
    }
    else if (a==5)
    {
      sum = 25
    }
    else 
    {
      sum = 0
    }
    return sum
   }


}
