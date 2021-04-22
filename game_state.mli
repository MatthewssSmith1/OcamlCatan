type t

val make_new_game : t

val game_to_board : t -> Board.t

(** Requires that a player of the given color has not been added before *)
val add_player : t -> Types.color -> t

val game_to_players : t -> Player.t list

(*For giving resources at the start of each turn*)
val distribute_resources : t -> int -> t

(*val make_move : t -> Input.move -> t

  val next_move : t -> Input.move -> t *)
