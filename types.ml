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
  | Green
  | Blue
  | White

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
  | Green -> "Green"
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
