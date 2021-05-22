val print_game : Game_state.t -> unit

val initialize : unit -> unit

val is_window_open : unit -> bool

val next_board_click : unit -> Types.boardClick

(** returns the hex index clicked *)
val next_hex_click : unit -> int

(** returns (hex_index, edge_dir) *)
val next_edge_click : unit -> int * int

(** returns (hex_index, vert_dir) *)
val next_vert_click : unit -> int * int
