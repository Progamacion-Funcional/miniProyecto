import scala.math.E

def AproximacionSimple(a:Int, b: Int,funcion: Double => Double) =
  val x=((a+b)/2)
  (b-a)*(funcion(a)+4*funcion(x)+funcion(b))/6

val Integral1 = (x : Double) => -Math.pow(x,2) + (8*x) -12
val Integral2 = (x:Double)=> 3*Math.pow(x,2)
val Integral3 = (x:Double) => x + 2*Math.pow(x,2)-Math.pow(x,3) + 5*Math.pow(x,4)
val Integral4 = (x:Double) => (2*x) + 1 / Math.pow(x,2) + x
val Integral5 = (x:Double) => Math.pow(E,x)
val Integral6 = (x: Double) => 1/Math.sqrt(x-1)
val Integral7 = (x:Double)=> 1/(1+Math.pow(x,2))

@main
def main(): Unit =
  println(AproximacionSimple(3,5,Integral1))
  println(AproximacionSimple(0,2,Integral2))
  println(AproximacionSimple(-1, 1,Integral3))
  println(AproximacionSimple(1, 2,Integral4))
  println(AproximacionSimple(0, 1,Integral5))
  println(AproximacionSimple(2, 3,Integral6))
  println(AproximacionSimple(0,1,Integral7))



