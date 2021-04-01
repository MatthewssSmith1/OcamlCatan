type t

val add_road : Player.t -> int -> int -> t -> t

val add_settlement : Player.t -> int -> int -> t -> t

val upgrade_city : Player.t -> int -> int -> t -> t

val make_board : t

val make_random_board : t

val hex_info : t -> int -> Types.hex

val hex_to_vertices : t -> int -> Types.vertex list

val hex_to_edges : t -> int -> Types.edge list
