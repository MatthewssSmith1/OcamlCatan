type t

val make_new_game : t

val add_player : t -> Types.color -> t

val distribute_resources : t -> int -> t

val make_move : t -> Input.move -> t

val next_move : t -> Input.move -> t
