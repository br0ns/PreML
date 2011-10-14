structure Source :> Source =
struct

open Substring

type t = substring
type pos = int
type span = pos * pos

val fromString = full
val toString = string

fun span (s, (l, r)) = string $ slice (s, l, SOME (r - l))

val read = getc
end
