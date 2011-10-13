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

val test01 = checkedAdd (SOME 7, SOME 35) = SOME 42
val test02 = checkedAdd (SOME 7, NONE) = NONE
