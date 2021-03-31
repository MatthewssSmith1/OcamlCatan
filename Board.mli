module type Board = sig
  type board

  val add_road : (*Game_State.tTeam -> *) int -> int -> board -> board

  val add_settlement : (*team ->*) int -> int -> board -> board

  val upgrade_city : (*team ->*) int -> int -> board -> board
end
