signature Monad =
sig
type 'a t
val >>= : 'a t * ('a -> 'b t) -> 'b t
val return : 'a -> 'a t
end

functor StateT (M : Monad) =
struct
type 'a inner = 'a M.t
type ('a, 's) t = 's -> ('a * 's) inner
fun >>= (m, k) s =
    do with M
     ; (a, s') <- m s
     ; k a s'
    end
fun lift m s =
    do with M
     ; a <- m
     ; return (a, s)
    end
fun return a s = M.return (a, s)
fun get s = M.return (s, s)
fun put s _ = M.return ((), s)
fun evalStateT m s =
    do with M
     ; (a, _) <- m s
     ; return a
    end
fun execStateT m s =
    do with M
     ; (_, s') <- m s
     ; return s'
    end
end

structure Option =
struct open Option
type 'a t = 'a option
fun >>= (SOME x, k) = k x
  | >>= _ = NONE
val return = SOME
end


(* Example from http://hackage.haskell.org/packages/archive/mtl/1.1.0.2/doc/html/Control-Monad-State-Lazy.html#3 *)
datatype 'a tree = L | B of 'a tree * 'a * 'a tree
type ('a, 'b) table = ('a * 'b) list

open StateT (Option)

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
  | numberTree (B (l, "", r)) = lift NONE
  | numberTree (B (l, x, r)) =
    do i <- lookup x
     ; l' <- numberTree l
     ; r' <- numberTree r
     ; return (B (l', i, r'))
    end

val t01 = B (B (L, "foo", L), "bar", B (L, "foo", L))
val test01 = evalStateT (numberTree t01) [] =
             SOME (B (B (L, 1, L), 0, B (L, 1, L)))

val t02 = B (B (L, "foo", L), "", B (L, "foo", L))
val test02 = evalStateT (numberTree t02) [] =
             NONE
