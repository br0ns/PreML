open (filter, concat) List

val foo = filter (fn x => x <> 0) o concat o map (fn x => [x, ~x])
