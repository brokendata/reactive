import calculator._

val x = Var(10)
val y = Var(10)
val z = Signal{ x() + y()}

z()
x() = 20
z()