type t

val make_new_game : t

(** Requires that a player of the given color has not been added before *)
val add_player : t -> Types.color -> t

val distribute_resources : t -> int -> t

val make_move : t -> Input.move -> t

val next_move : t -> Input.move -> t
