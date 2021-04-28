type t

val add_road : Player.t -> int -> int -> t -> t

val add_settlement : Player.t -> int -> int -> t -> t

val add_settlement_start : Player.t -> int -> int -> t -> t

val upgrade_city : Player.t -> int -> int -> t -> t

(*Check for adj roads/settlements/cities*)
val can_add_road : Player.t -> int -> int -> t -> bool

(*Check for adj roads and no adj settlements/cities*)
val can_add_settlement : Player.t -> int -> int -> t -> bool

val move_robber : int -> t -> t

val make_board : unit -> t

val make_random_board : unit -> t

val hex_info : t -> int -> Types.hex

val hex_to_vertices : t -> int -> Types.vertex list

val hex_to_edges : t -> int -> Types.edge list

val hex_coords : int -> int * int

val int_to_hex_list : t -> int -> int list

val vert_to_adj_verts : t -> int -> int -> Types.vertex list

val vert_to_adj_vert_coords : t -> int -> int -> (int * int) list

val vert_to_adj_edges : t -> int -> int -> Types.edge list

val vert_to_adj_edge_coords : t -> int -> int -> (int * int) list

val vert_to_adj_hexes : t -> int -> int -> Types.hex list

val vert_to_adj_hex_coords : t -> int -> int -> (int * int) list

val edge_to_adj_verts : t -> int -> int -> Types.vertex list

val edge_to_adj_vert_coords : t -> int -> int -> (int * int) list

val edge_to_adj_edges : t -> int -> int -> Types.edge list

val edge_to_adj_edge_coords : t -> int -> int -> (int * int) list

val get_robber : t -> int

(* val verticies_of_player : t -> Types.color -> (int * int) list val
   longest_road : t -> Types.color * int *)
