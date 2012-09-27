type 'a lazy = unit -> 'a
fun lazy t = t
fun force t = t ()
fun delay t = L(F (t()))
fun eager x = L(x)

datatype 'a stream = Stream of ('a * 'a stream lazy)

val nat =
    let
      fun loop n = L (Stream (n, loop (n + 1)))
    in
      loop 1
    end
