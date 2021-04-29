type t

val add_resource : Types.resource -> int -> t -> t

val remove_resource : Types.resource -> int -> t -> t

val add_resource_list : Types.resource list -> t -> t

val remove_resource_list : Types.resource list -> t -> t

val check_resource : Types.resource -> t -> int

val add_dev : Types.devCard -> int -> t -> t

val remove_dev : Types.devCard -> int -> t -> t

(*Only counts currently playable cards*)
val check_dev : Types.devCard -> t -> int

val add_port : Types.port -> t -> t

(*bool = whether or not the piece is free*)
val place_road : t -> bool -> t

val place_settlement : t -> bool -> t

val place_city : t -> bool -> t

val buy_dev : t -> Types.devCard -> t

val get_color : t -> Types.color

val make_player : Types.color -> t

val to_string : t -> string

val can_add_road : t -> bool

val can_add_settlement : t -> bool

val can_add_city : t -> bool

val can_add_dev : t -> bool

val end_turn : t -> t

val resources_of : t -> (Types.resource * int) list

val devs_of : t -> (Types.devCard * int) list

val num_resources : t -> int

val num_devs : t -> int

(*number of _ played, used to determine whether or not at start of game*)
val num_roads : t -> int

val num_settlements : t -> int

val pieces_left : t -> int * int * int

val victory_points : t -> int
