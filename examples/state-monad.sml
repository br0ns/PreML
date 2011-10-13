structure State =
struct
fun >>= (m, k) s = let val (a, s') = m s in k a s' end
fun return a s = (a, s)
fun get s = (s, s)
fun put s _ = ((), s)
fun evalState m s = let val (a, _) = m s in a end
fun execState m s = let val (_, s') = m s in s' end
end

(* Example from http://hackage.haskell.org/packages/archive/mtl/1.1.0.2/doc/html/Control-Monad-State-Lazy.html#3 *)
datatype 'a tree = L | B of 'a tree * 'a * 'a tree
type ('a, 'b) table = ('a * 'b) list

open State

fun lookup x =
    do table <- get
     ; case List.find (fn (y, _) => x = y) table of
         SOME (_, i) => return i
       | NONE => do i := length table
                  ; put ((x, i) :: table)
                  ; return i
                 end
    end

fun numberTree L = return L
  | numberTree (B (l, x, r)) =
    do i <- lookup x
     ; l' <- numberTree l
     ; r' <- numberTree r
     ; return (B (l', i, r'))
    end

val t = B (B (L, "foo", L), "bar", B (L, "foo", L))
val test01 = evalState (numberTree t) [] = B (B (L, 1, L), 0, B (L, 1, L))
