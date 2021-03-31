type t

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

type team =
  | Red
  | Orange
  | Blue
  | White

type vertex =
  | Empty
  | Settlement of team
  | City of team

type edge =
  | Empty
  | Road of team

type devCard =
  | Knight
  | RoadBuilding
  | YearOfPlenty
  | Monopoly
  | VictoryPoint

type trade_offer = {
  team_from : team;
  offer : resource list;
  request : resource list;
}

val next_move : t -> Input.move -> t
