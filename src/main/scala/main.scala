val Par=8
def AproximacionSimple(a:Int, b: Int,funcion: Double => Double) =
  val x=((a+b)/2)
  (b-a)*((funcion(a))+(4*funcion(x))+(funcion(b)))/6
def AproximacionCompuesta(a:Double, b:Double,n:Int,funcion:Double => Double) : Double =
  val h = (b-a)/n
  val xj = (j:Double) => a + (j*h)
  (h/3) * (1 to (n/2)).map(j =>funcion(xj(2*j-2))+4*funcion(xj(2*j-1))+funcion(xj(2*j))).sum

def AproximacionExtendida(a:Double, b:Double, funcion:Double => Double) : Double = {
  val n = 2 * (b-a)
  val h = (b-a)/n
  (h/3)*(funcion(a)+(4*(1 to (n-1).toInt).map(i => funcion(a+(i*h))).sum) + (2*(1 to (n-2).toInt).map(j => funcion(a+(j*h))).sum) + funcion(b))
}

def calError (valEsperado : Double, Aproximacion : Double) = (Math.abs(valEsperado-Aproximacion))

val Integral1 = (x : Double) => -Math.pow(x,2) + (8*x) -12
val Integral2 = (x:Double)=> 3*Math.pow(x,2)
val Integral3 = (x:Double) => x + 2*Math.pow(x,2)-Math.pow(x,3) + 5*Math.pow(x,4)
val Integral4 = (x:Double) => ((2*x) + 1) / (Math.pow(x,2) + x)
val Integral5 = (x:Double) => Math.pow(Math.E,x)
val Integral6 = (x: Double) => 1/Math.sqrt(x-1)
val Integral7 = (x:Double)=> 1/(1+Math.pow(x,2))

@main
def main(): Unit =
  println(calError(7.33, AproximacionSimple(3, 5, Integral1)))
  println(calError(8, AproximacionSimple(0, 2, Integral2)))
  println(calError(3.333, AproximacionSimple(-1, 1, Integral3)))
  println(calError(1.09861, AproximacionSimple(1, 2, Integral4)))
  println(calError(1.71828, AproximacionSimple(0, 1, Integral5)))
  println(calError(0.828427, AproximacionSimple(2, 3, Integral6)))
  println(calError(0.7785398, AproximacionSimple(0, 1, Integral7)))
  println("--------------------------------------")
  println("Errores de Aproximacion Compuesta")
  println(calError(7.33, AproximacionCompuesta(3, 5, Par, Integral1)))
  println(calError(8, AproximacionCompuesta(0, 2, Par, Integral2)))
  println(calError(3.333, AproximacionCompuesta(-1, 1, Par, Integral3)))
  println(calError(1.09861, AproximacionCompuesta(1, 2, Par, Integral4)))
  println(calError(1.71828, AproximacionCompuesta(0, 1, Par, Integral5)))
  println(calError(0.828427, AproximacionCompuesta(2, 3, Par, Integral6)))
  println(calError(0.7785398, AproximacionCompuesta(0, 1, Par, Integral7)))
  println("--------------------------------------")
  println("Errores de Aproximacion Extendida")
  println(calError(7.33, AproximacionExtendida(3, 5, Integral1)))
  println(calError(8, AproximacionExtendida(0, 2, Integral2)))
  println(calError(3.333, AproximacionExtendida(-1, 1, Integral3)))
  println(calError(1.09861, AproximacionExtendida(1, 2, Integral4)))
  println(calError(1.71828, AproximacionExtendida(0, 1, Integral5)))
  println(calError(0.828427, AproximacionExtendida(2, 3, Integral6)))
  println(calError(0.7785398, AproximacionExtendida(0, 1, Integral7)))




