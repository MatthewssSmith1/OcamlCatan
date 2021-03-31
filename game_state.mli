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

type tDevCard =
  | Knight
  | RoadBuilding
  | YearOfPlenty
  | Monopoly
  | VictoryPoint
