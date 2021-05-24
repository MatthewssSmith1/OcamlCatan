(** Represents a single board state containing the information for the
    hexes, edges, and vertices as well as the location of the robber and
    types of ports.*)

(** represents a possible state of the board, see Catan rule book for
    valid states*)
type t

(** returns a new board state where a road belonging to the given player
    is placed per Catan rules at the specified hex and direction, fails
    if it cannot place the road*)
val add_road : Player.t -> int -> int -> t -> t

(** returns a new board state where a settlement belonging to the given
    player is placed per Catan rules at the specified hex and direction,
    fails if it cannot place the settlement*)
val add_settlement : Player.t -> int -> int -> t -> t

(** identical functionality to [add_settlement] except new settlement
    does not need to be connected to a player's roads*)
val add_settlement_start : Player.t -> int -> int -> t -> t

(** returns a new board state where an existing settlement belong to the
    given player is upgraded to a city per Catan rules at the specified
    hex and direction, fails if it cannot upgrade a settlement*)
val upgrade_city : Player.t -> int -> int -> t -> t

(** returns a new standard board state*)
val make_board : unit -> t

(** returns a new board state generated randomly*)
val make_random_board : unit -> t

(** returns [Some a] where [a] is the [Types.hex] representaion of the
    given hex on the board if it exists, otherwise returns [None] *)
val hex_info : t -> int -> Types.hex

(** given a board and hex number returns a list containing the
    information of surrounding vertices*)
val hex_to_vertices : t -> int -> Types.vertex list

(** given a board and hex number returns a list containing the
    information of surrounding edges*)
val hex_to_edges : t -> int -> Types.edge list

(** given a hex number returns its coordinates in the internal hex array*)
val hex_coords : int -> int * int

(** given a board and resource number returns the list of hex numbers
    not covered by the robber that match that resource number*)
val int_to_hex_list : t -> int -> int list

(** given a board, hex number, and direction for a vertex returns a list
    containing the information of surrouding vertices*)
val vert_to_adj_verts : t -> int -> int -> Types.vertex list

(** given a board and vertex coordinates returns a list containing the
    coordinates of surrounding vertices*)
val vert_to_adj_vert_coords : t -> int -> int -> (int * int) list

(** given a board, hex number, and direction for a vertex returns a list
    containing the information of surrounding edges*)
val vert_to_adj_edges : t -> int -> int -> Types.edge list

(** given a board and vertex coordinates returns a list containing the
    coordinates of surrounding edges*)
val vert_to_adj_edge_coords : t -> int -> int -> (int * int) list

(** given a board, hex number, and direction for a vertex returns a list
    containing the information of surrounding hexes*)
val vert_to_adj_hexes : t -> int -> int -> Types.hex list

(** given a board and vertex coordinates returns a list containing the
    coordinates of surrounding hexes*)
val vert_to_adj_hex_coords : t -> int -> int -> (int * int) list

(** given a board, hex number, and direction for an edge returns a list
    containing the information of surrounding vertices*)
val edge_to_adj_verts : t -> int -> int -> Types.vertex list

(** given a board and edge coordinates returns a list containing the
    coordinates of surrounding vertices*)
val edge_to_adj_vert_coords : t -> int -> int -> (int * int) list

(** given a board, hex number, and direction for an edge returns a list
    containing the information of surrounding edges*)
val edge_to_adj_edges : t -> int -> int -> Types.edge list

(** given a board and edge coordinates returns a list containing the
    coordinates of surrounding edges*)
val edge_to_adj_edge_coords : t -> int -> int -> (int * int) list

(** given a board returns the current hex number of the robber*)
val get_robber : t -> int

(** returns a new board state where the robber has been moved to the
    specified hex number, fails if hex number is out of bounds*)
val move_robber : t -> int -> t

(** given a hex number and direction returns [Some a] if the specified
    board has a port of type [a] there, otherwise returns [None]*)
val get_port : int -> int -> t -> Types.port option
