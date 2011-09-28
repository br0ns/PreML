signature Rules =
sig
  type 'x t = 'x Rewrite.rule

  val doBlock : 'x t
  val extendNew : 'x t
  val extendExisting : 'x t
  val openFunctor : 'x t
end
