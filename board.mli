type t

(** Adds a road at the specified location (hex, dir) of the specified
    player*)
val add_road : Player.t -> int -> int -> t -> t

(** Adds a settlement at the specified location (hex, dir) of the
    specified player*)
val add_settlement : Player.t -> int -> int -> t -> t

(** Adds a settlement at the specified location (hex, dir) of the
    specified player without the requirement of being adjacent to a road*)
val add_settlement_start : Player.t -> int -> int -> t -> t

(** Upgrades a settlement to a city at the specified location (hex, dir)
    of the specified player*)
val upgrade_city : Player.t -> int -> int -> t -> t

(** Makes a basic board. Only for testing*)
val make_board : unit -> t

(** Makes a random board*)
val make_random_board : unit -> t

(** Returns the info of the specified hex*)
val hex_info : t -> int -> Types.hex

(** Returns a list of the verticies around a specified a hex*)
val hex_to_vertices : t -> int -> Types.vertex list

(** Returns a list of the edges around a specified a hex*)
val hex_to_edges : t -> int -> Types.edge list

(** Returns thec oordinates of a specified a hex*)
val hex_coords : int -> int * int

(** Returns a list of hexes with the specified number*)
val int_to_hex_list : t -> int -> int list

(** Returns a list of the verticies adjacent to the specified vertex*)
val vert_to_adj_verts : t -> int -> int -> Types.vertex list

(** Returns a list of the coordinates of the verticies adjacent to the
    specified vertex*)
val vert_to_adj_vert_coords : t -> int -> int -> (int * int) list

(** Returns a list of the edges adjacent to the specified vertex*)
val vert_to_adj_edges : t -> int -> int -> Types.edge list

(** Returns a list of the coordinates of the edges adjacent to the
    specified vertex*)
val vert_to_adj_edge_coords : t -> int -> int -> (int * int) list

(** Returns a list of the hexes adjacent to the specified vertex*)
val vert_to_adj_hexes : t -> int -> int -> Types.hex list

(** Returns a list of the coordinates of the hexes adjacent to the
    specified vertex*)
val vert_to_adj_hex_coords : t -> int -> int -> (int * int) list

(** Returns a list of the verticies adjacent to the specified edge*)
val edge_to_adj_verts : t -> int -> int -> Types.vertex list

(** Returns a list of the coordinates of the verticies adjacent to the
    specified edge*)
val edge_to_adj_vert_coords : t -> int -> int -> (int * int) list

(** Returns a list of the edges adjacent to the specified edge*)
val edge_to_adj_edges : t -> int -> int -> Types.edge list

(** Returns a list of the coordinates of the edges adjacent to the
    specified edge*)
val edge_to_adj_edge_coords : t -> int -> int -> (int * int) list

(** Returns the current location of the robber*)
val get_robber : t -> int

(** Returns an otherwise identical new board where the robber is at the
    specified location*)
val move_robber : t -> int -> t

(** Returns a port option at the specified vertex*)
val get_port : int -> int -> t -> Types.port option

(* val verticies_of_player : t -> Types.color -> (int * int) list val
   longest_road : t -> Types.color * int *)
