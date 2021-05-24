(** Represents a single player containing the information for the color,
    resources, development cards, access to ports, structures, and
    victory points corresponding to the player*)

(** Represents a possible player. See Catan rule book for valid players*)
type t

(** Returns whether or not a player has the resources and structures to
    build a road*)
val can_add_road : t -> bool

(** Returns whether or not a player has the resources and structures to
    build a settlement*)
val can_add_settlement : t -> bool

(** Returns whether or not a player has the resources and structures to
    build a city*)
val can_add_city : t -> bool

(** Returns whether or not a player has the resources to buy a
    development card*)
val can_add_dev : t -> bool

(** Addes the specified amount of the specified resource to the player*)
val add_resource : Types.resource -> int -> t -> t

(** Removes the specified amount of the specified resource to the
    player. Fails if impossible*)
val remove_resource : Types.resource -> int -> t -> t

(** Addes the specified resources to the player*)
val add_resource_list : Types.resource list -> t -> t

(** Removes the specified resources to the player. Fails if impossible*)
val remove_resource_list : Types.resource list -> t -> t

(** Returns the amount of the specified resource the player has*)
val check_resource : Types.resource -> t -> int

(** Addes the specified amount of the specified development card to the
    player*)
val add_dev : Types.devCard -> int -> t -> t

(** Removes the specified amount of the specified development card to
    the player. Fails if impossible*)
val remove_dev : Types.devCard -> int -> t -> t

(** Returns the amount of the specified development card the player
    currently has. Only counts currently playable cards, ie not counting
    non-victory point cards gained this turn*)
val check_dev : Types.devCard -> t -> int

(** Adds the specified port to the player*)
val add_port : Types.port -> t -> t

(** Places a road corresponding to the player, specifying whether or not
    the road is free. Fails if impossible.*)
val place_road : t -> bool -> t

(** Places a settlement corresponding to the player, specifying whether
    or not the road is free. Fails if impossible.*)
val place_settlement : t -> bool -> t

(** Places a city corresponding to the player, specifying whether or not
    the road is free. Fails if impossible.*)
val place_city : t -> bool -> t

(** Buys a development card. Fails if impossible.*)
val buy_dev : t -> Types.devCard -> t

(** Returns the color of the player*)
val get_color : t -> Types.color

(** Makes a new player with the specified color*)
val make_player : Types.color -> t

(** Returns a string of the player*)
val to_string : t -> string

(** Ends the player's turn*)
val end_turn : t -> t

(** Returns a list of the player's resources*)
val resources_of : t -> (Types.resource * int) list

(** Returns a list of the player's development cards*)
val devs_of : t -> (Types.devCard * int) list

(** Returns how many resources the player has*)
val num_resources : t -> int

(** Returns how many development cards the player has*)
val num_devs : t -> int

(** Returns the number of roads the player has placed*)
val num_roads : t -> int

(** Returns the number of settlements the player has placed*)
val num_settlements : t -> int

(** Returns the number of cities the player has placed*)
val num_cities : t -> int

(** Returns the number of roads, settlements, and cities the player has
    left*)
val pieces_left : t -> int * int * int

(** Returns the number of victory points the player currently has*)
val victory_points : t -> int

(** Returns a random resource the player currently has. Fails if no
    resources*)
val random_resource : t -> Types.resource

(** Returns whether or not the player currently has access to the
    specified port*)
val has_port : t -> Types.port -> bool

(** Adds the specified port to the player*)
val add_port : t -> Types.port -> t
