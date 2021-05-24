(** Contains all the various types used by other modules as well as some
    helper functions to better use them.*)

(** corresponds to the resource produced by a hex*)
type resource =
  | Wood
  | Sheep
  | Wheat
  | Brick
  | Ore

(** represents a hex as either a desert or a resource producing tile of
    its resource number and produced resource*)
type hex =
  | Desert
  | Other of (int * resource)

(** represents a trading port type as 3-1 generic or 2-1 specific*)
type port =
  | ThreeToOne
  | TwoToOne of resource

(** colors corresponding to the players*)
type color =
  | Red
  | Orange
  | Blue
  | White

(** represents a vertex as either being empty or a settlement or city
    belonging to a color*)
type vertex =
  | Empty
  | Settlement of color
  | City of color

(** represents an edge as either empty or containing a road of a color*)
type edge =
  | Empty
  | Road of color

(** represents each of the dev cards in the game*)
type devCard =
  | Knight
  | RoadBuilding
  | YearOfPlenty
  | Monopoly
  | VictoryPoint

(** represents a trade offer as the offering player, the resources being
    offered, and resources requested*)
type trade_offer = {
  team_from : color;
  offer : resource list;
  request : resource list;
}
(** represents the possible commands a user can input on their turn*)
type turnCommand =
  | BuildRoad of (int * int) (*hex index, dir*)
  | BuildSettlement of (int * int) (*hex index, dir*)
  | UpgradeCity of (int * int) (*hex index, dir*)
  | OfferTrade of (color * resource list * resource list)
  (*trading partner, resources offered, resources wnated*)
  | BankTrade of (resource list * resource list)
  | BuyDevCard
  | UseDevCard of devCard
  | EndTurn

(** represents the possible places a user can click on the board*)
type boardClick =
  | CHex of int
  | CEdge of (int * int)
  | CVert of (int * int)

(** converts a board click into a readable string*)
val string_of_board_click : boardClick -> string

(** converts a resources to a readable string*)
val resource_to_string : resource -> string

(** converts a color to a readable string*)
val color_to_string : color -> string

(** converts a dev card to a readable string*)
val dev_card_to_string : devCard -> string

(** converts a hex to a readable string*)
val hex_to_string : hex -> string

(** given a hex returns its resource number*)
val number_of_hex : hex -> int
