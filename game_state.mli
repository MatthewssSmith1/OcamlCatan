type t

val make_new_game : t

val game_to_board : t -> Board.t

(** Requires that a player of the given color has not been added before *)
val add_player : t -> Types.color -> t

val game_to_players : t -> Player.t list

(*For giving resources at the start of each turn*)
val distribute_resources : t -> int -> t

(*Player color, hex, dir, raises failure if not possible*)
(* val build_road : t -> Types.color -> int -> int -> t

   val build_settlement : t -> Types.color -> int -> int -> t

   val upgrade_city : t -> Types.color -> int -> int -> t

   val buy_dev_card : t -> Types.color -> t

   val open_trade : t -> Types.trade_offer -> t

   val close_trade : t -> Types.trade_offer -> t

   val accept_trade : t -> Types.trade_offer -> Types.color -> t *)

(*val make_move : t -> Input.move -> t

  val next_move : t -> Input.move -> t *)
