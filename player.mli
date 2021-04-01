type t

exception Not_enough_resources

exception Not_enough_pieces

val add_resource : Types.resource -> int -> t -> t

val remove_resource : Types.resource -> int -> t -> t

val check_resource : Types.resource -> t -> int

val add_port : Types.port -> t -> t

val place_road : t -> t

val place_settlement : t -> t

val place_city : t -> t

val make_player : t
