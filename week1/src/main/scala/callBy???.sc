def square1(x: Double): Double =
  println(s"call sqaure1 with $x")
  x * x

def square2(x: => Double): Double =
  println(s"call sqaure2 with $x")
  x * x

def oneDef: Double =
  println("create 1")
  1

val oneVal: Double =
  println("create 1")
  1

square1(1)
square1(oneDef)
square2(1)
square2(oneDef)

oneDef
oneVal

oneDef == oneVal

val a = oneDef
def b = oneDef

