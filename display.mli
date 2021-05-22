val print_game : Game_state.t -> unit

val initialize : unit -> unit

val is_window_open : unit -> bool

val next_board_click : unit -> Types.boardClick

val next_hex_click : unit -> int

val next_edge_click : unit -> int * int

val next_vert_click : unit -> int * int
