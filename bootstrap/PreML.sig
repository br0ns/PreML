signature PreML =
sig
  (* {pathIn -> pahtOut -> ...} *)
  val run : Path.t -> Path.t -> int
end
