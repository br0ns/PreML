val a = (,42) 42
val b = (42,) 42
val c = (,) 42 42
val d = (42,,42) 42
fun e x = (,x,)
val f = e 41 42 43

val xs = map (42,) [1,2,3]
