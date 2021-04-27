type t

val add_resource : Types.resource -> int -> t -> t

val remove_resource : Types.resource -> int -> t -> t

val check_resource : Types.resource -> t -> int

val add_dev : Types.devCard -> int -> t -> t

val remove_dev : Types.devCard -> int -> t -> t

(*Only counts currently playable cards*)
val check_dev : Types.devCard -> t -> int

val add_port : Types.port -> t -> t

val place_road : t -> t

val place_settlement : t -> t

val place_city : t -> t

val buy_dev : t -> Types.devCard -> t

val get_color : t -> Types.color

val make_player : Types.color -> t

val to_string : t -> string

val can_add_road : t -> bool

val can_add_settlement : t -> bool

val can_add_city : t -> bool

val can_add_dev : t -> bool

val end_turn : t -> t
