type resource =
  | Wood
  | Sheep
  | Wheat
  | Brick
  | Ore

type hex =
  | Desert
  | Other of (int * resource)

type port =
  | ThreeToOne
  | TwoToOne of resource

type color =
  | Red
  | Orange
  | Blue
  | White

let colors = [ Red; Orange; Blue; White ]

type vertex =
  | Empty
  | Settlement of color
  | City of color

type edge =
  | Empty
  | Road of color

type devCard =
  | Knight
  | RoadBuilding
  | YearOfPlenty
  | Monopoly
  | VictoryPoint

type trade_offer = {
  team_from : color;
  offer : resource list;
  request : resource list;
}

type turnCommand =
  | BuildRoad of (int * int) (*hex index, dir*)
  | BuildSettlement of (int * int) (*hex index, dir*)
  | UpgradeCity of (int * int) (*hex index, dir*)
  | OfferTrade of (color * resource list * resource list)
  (*trading partner, resources offered, resources wanted*)
  | BankTrade of (resource list * resource list)
  | BuyDevCard
  | UseDevCard of devCard
  | EndTurn

type boardClick =
  | CHex of int
  | CEdge of (int * int)
  | CVert of (int * int)

let string_of_board_click = function
  | CHex i_hex -> ("hex " ^ (string_of_int i_hex))
  | CEdge (i_hex, i_edge) -> ("hex " ^ (string_of_int i_hex) ^ " > edge " ^ (string_of_int i_edge))
  | CVert (i_hex, i_vert) -> ("hex " ^ (string_of_int i_hex) ^ " > vert " ^ (string_of_int i_vert))

let resource_to_string resource =
  match resource with
  | Wood -> "Wood"
  | Sheep -> "Sheep"
  | Wheat -> "Wheat"
  | Brick -> "Brick"
  | Ore -> "Ore"

let color_to_string color =
  match color with
  | Red -> "Red"
  | Orange -> "Orange"
  | Blue -> "Blue"
  | White -> "White"

let dev_card_to_string card =
  match card with
  | Knight -> "Knight"
  | RoadBuilding -> "Road Building"
  | YearOfPlenty -> "Year of Plenty"
  | Monopoly -> "Monopoly"
  | VictoryPoint -> "Victory Point"

let hex_to_string hex =
  match hex with
  | Desert -> "Desert"
  | Other (x, y) -> string_of_int x ^ " " ^ resource_to_string y

let number_of_hex = function Desert -> 7 | Other (x, _) -> x
