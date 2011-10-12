
structure Option =
struct open Option
fun >>= (SOME x, k) = k x
  | >>= _ = NONE
val return = SOME
end

fun checkedAdd (x, y) = do with Option
                         ; a <- x
                         ; b <- y
                         ; return (a + b)
                        end
