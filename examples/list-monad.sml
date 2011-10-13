structure List =
struct open List
fun >>= (xs, k) = List.concat (map k xs)
fun return x = [x]
end

fun allPairs (xs, ys) =
    do with List
     ; x <- xs
     ; y <- ys
     ; return (x, y)
    end

val test01 = allPairs ([1, 2], [3, 4]) = [(1, 3), (1, 4), (2, 3), (2, 4)]
