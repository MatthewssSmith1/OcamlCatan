module type Board = sig
  type board

  val add_road : Player.player -> int -> int -> board -> board

  val add_settlement : Player.player -> int -> int -> board -> board

  val upgrade_city : Player.player -> int -> int -> board -> board
end
