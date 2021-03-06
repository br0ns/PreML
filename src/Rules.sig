signature Rules =
sig
  type 'x t = 'x Rewrite.rule

  val doBlock : 'x t
  val extendNew : 'x t
  val extendExisting : 'x t
  val openFunctor : 'x t
  val failHere : Path.t -> Source.t -> 'x t
  val includeFile : Path.t -> 'x t
  val openFiltered : 'x t
  val listComp : 'x t
  val partTuples : 'x t
  val lazy : 'x t
end
