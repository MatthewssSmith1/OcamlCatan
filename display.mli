(** Contains all the functionality for graphically showing information
    about the current game state to the user and taking in mouse inputs
    from the screen.*)

(** Prints to the terminal a represntation of a game state*)
val print_game : Game_state.t -> unit

(** Intializes and opens up a new UI window*)
val initialize : unit -> unit

(** Returns true if the game window is open*)
val is_window_open : unit -> bool

(** Returns information about the location of the board clicked by the
    user*)
val next_board_click : unit -> Types.boardClick

(** returns the hex index clicked *)
val next_hex_click : unit -> int

(** returns (hex_index, edge_dir) *)
val next_edge_click : unit -> int * int

(** returns (hex_index, vert_dir) *)
val next_vert_click : unit -> int * int
