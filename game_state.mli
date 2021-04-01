type t

val distribute_resources : t -> int -> t

val make_move : t -> Input.move -> t

val next_move : t -> Input.move -> t
