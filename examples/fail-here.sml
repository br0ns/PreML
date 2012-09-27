val x = raise FailHere "foo"

(* On my system (using MosML 2.10) I see this:
 * - sml19387iLv -> 1 -> sml19387iLv.preml
 * [opening file "/tmp/sml19387iLv.preml"]
 * ! Uncaught exception:
 * ! Fail  "/tmp/sml19387iLv(1:31): foo"
 * [closing file "/tmp/sml19387iLv.preml"]
 *)
