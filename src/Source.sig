signature Source =
sig
  type t
  type pos = int
  type span = pos * pos

  val fromString : string -> t
  val fromFile : Path.t -> t
  val toString : t -> string
  val position : t -> pos -> {row: int, column: int}

  val span : t * span -> string

  (* Reader instance *)
  val read : (char, t) StringCvt.reader
end
