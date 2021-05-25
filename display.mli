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

(** returns the index of the next hex that is clicked *)
val next_hex_click : unit -> int

(** returns coords in the form (hex_index, edge_dir) for the next edge
    that is clicked *)
val next_edge_click : unit -> int * int

(** returns coords in the form (hex_index, vert_dir) for the next vertex
    that is clicked *)
val next_vert_click : unit -> int * int
