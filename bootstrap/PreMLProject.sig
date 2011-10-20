signature PreMLProject =
sig
  val run : (Path.t -> Path.t) -> Path.t -> Path.t -> int * Path.t list
end
