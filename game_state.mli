type t

val make_new_game : unit -> t

val game_to_board : t -> Board.t

val next_turn : t -> t

val current_turn : t -> Player.t

(** Requires that a player of the given color has not been added before *)
val add_player : t -> Types.color -> t

val game_to_players : t -> Player.t list

(**For giving resources at the start of each turn*)
val distribute_resources : t -> int -> t

(**Player color, hex, dir, raises failure if not possible*)
val build_road : t -> int -> int -> t

val build_settlement : t -> int -> int -> t

val upgrade_city : t -> int -> int -> t

val buy_dev_card : t -> t

val accept_trade :
  t -> Types.color -> Types.resource list -> Types.resource list -> t

val make_move : t -> Types.turnCommand -> t
