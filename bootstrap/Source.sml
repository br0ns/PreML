(* TODO: Remove this when the new MyLib is ready *)
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
fun >>= (m, k) s = let infix 0 >>= val op>>= = M.>>= val return = M.return in ( 

                  m s ) >>= (fn  (a, s') => 
       k a s' ) end 

fun lift m s = let infix 0 >>= val op>>= = M.>>= val return = M.return in ( 

            m ) >>= (fn  a => 
       return (a, s) ) end 

fun return a s = M.return (a, s)
fun get s = M.return (s, s)
fun put s _ = M.return ((), s)
fun modify f s = M.return ((), f s)
fun evalStateT m s = let infix 0 >>= val op>>= = M.>>= val return = M.return in ( 

                 m s ) >>= (fn  (a, _) => 
       return a ) end 

fun execStateT m s = let infix 0 >>= val op>>= = M.>>= val return = M.return in ( 

                  m s ) >>= (fn  (_, s') => 
       return s' ) end 

end

structure Option =
struct open Option
type 'a t = 'a option
fun >>= (SOME x, k) = k x
  | >>= _ = NONE
val return = SOME
end

structure Reader = StateT (Option)

structure Source :> Source =
struct

open Substring

type t = substring
type pos = int
type span = pos * pos

val fromString = full
val fromFile = full o TextIO.readFile o Path.toString
val toString = string

fun span (s, (l, r)) = string $ slice (s, l, SOME (r - l))

val read = getc

fun position s p =
    let open Reader
      fun loop 0 r c = return {row = r, column = c}
        | loop n r c = let infix 0 >>= in let val 
             cont =  loop (n - 1) in ( 
                  read ) >>= (fn  x => 
             case x of
               #"\n" => cont (r + 1) 0
             | _     => cont r (c + 1) ) end end 

    in
      case evalStateT (loop p 1 0) s of
        SOME r => r
      | NONE   => raise Fail "/home/br0ns/code/sml/preml/bootstrap/Source.sml(80:26): PreML.Source.position: Reached end of file." 

    end
end