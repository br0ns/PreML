signature PreMLProject =
sig
  val run : (Path.t -> Path.t) -> Path.t -> Path.t -> int
  val walk : (Path.t -> unit) -> Path.t -> int
end
