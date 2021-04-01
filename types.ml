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

type vertex =
  | Empty
  | Settlement of Player.t
  | City of Player.t

type edge =
  | Empty
  | Road of Player.t

type devCard =
  | Knight
  | RoadBuilding
  | YearOfPlenty
  | Monopoly
  | VictoryPoint

type trade_offer = {
  team_from : Player.t;
  offer : resource list;
  request : resource list;
}