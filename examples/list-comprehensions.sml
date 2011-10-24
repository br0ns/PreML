val xs = [1, 2, 3]
val ys = [~1, 5, 6]
val zs = [x + y | x <- xs, y <- ys, y > 0]
