(** Represents a single game state containing the information for the
    board and players*)

(** Represents a possible state of the game, see Catan rule book for
    valid states*)
type t

(** Makes a new game with a random board and no players*)
val make_new_game : unit -> t

(** Returns the game board*)
val game_to_board : t -> Board.t

(** Ends the current player's turn*)
val end_turn : t -> t

(** Returns the player whose turn it is*)
val current_turn : t -> Player.t

(** Adds a new player to the game. Requires that a player of the given
    color has not been added before *)
val add_player : t -> Types.color -> t

(** Returns a list of the players, in turn order, such that the head is
    the current turn, the second element is the next player, etc.*)
val game_to_players : t -> Player.t list

(** Distributes resources to the players as if the specified number was
    rolled. Intended for the start of each turn*)
val distribute_resources : t -> int -> t

(** The current player builds a road at the specified location. Current
    state, hex, dir, free. Raises failure if not possible*)
val build_road : t -> int -> int -> bool -> t

(** The current player builds a settlement at the specified location.
    Current state, hex, dir, free. Raises failure if not possible*)
val build_settlement : t -> int -> int -> bool -> t

(** The current player builds a city at the specified location. Current
    state, hex, dir, free. Raises failure if not possible*)
val upgrade_city : t -> int -> int -> bool -> t

(** The current player buys a development card from the deck. Raises
    failure if not possible*)
val buy_dev_card : t -> t

(** The current player trades the offer for the request with the
    specified other player*)
val accept_trade :
  t -> Types.color -> Types.resource list -> Types.resource list -> t

(** The current player trades the offer for the request with the bank*)
val bank_trade : t -> Types.resource list -> Types.resource list -> t

(** The current player moves the robber to the specified location and
    attempts to steal a resource from the specified player.*)
val move_robber : t -> int -> Types.color -> t

(** A move is made according to the specified turn command*)
val make_move : t -> Types.turnCommand -> t
