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

val resource_to_string : resource -> string

val color_to_string : color -> string

val dev_card_to_string : devCard -> string