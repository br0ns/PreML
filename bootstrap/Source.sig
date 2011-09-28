signature Source =
sig
  type t
  type pos = int
  type span = pos * pos

  val fromString : string -> t
  val toString : t -> string

  val span : t * span -> string

  (* Reader instance *)
  val read : (char, t) StringCvt.reader
end
