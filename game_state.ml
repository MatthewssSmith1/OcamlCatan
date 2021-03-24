type tResource =
  | Wood
  | Sheep
  | Wheat
  | Brick
  | Ore

type tHex =
  | Desert
  | Other of (int * tResource)

(** https://catan.fandom.com/wiki/Harbor *)
type tPort =
  | ThreeToOne
  | TwoToOne of tResource

type tTeam =
  | Red
  | Orange
  | Blue
  | White

type tVertex =
  | Empty
  | Settlement of tTeam
  | City of tTeam

type tEdge =
  | Empty
  | Road of tTeam

(** https://catan.fandom.com/wiki/Development_card *)
type tDevCard =
  | Knight
  | RoadBuilding
  | YearOfPlenty
  | Monopoly
  | VictoryPoint

type inventory = {
  resource_cards: tResource list;
  dev_cards: tDevCard list;
  roads_left: int;
  settlements_left: int;
  cities_left: int;
}

(** this is a fantastic resource for how me way want to represent
    hex coords https://www.redblobgames.com/grids/hexagons/ *)
type vec2 = {x: int; y: int;}

type trade_offer = {
  team_from: tTeam;
  offer: tResource list;
  request: tResource list;
}

type edge_coord = vec2
type vertex_coord = vec2

type game_state = {
  hexes: (vec2 * tHex) list;
  ports: (int * tPort) list;
  edges: (edge_coord * tEdge) list;
  vericies: (vertex_coord * tVertex) list;
  inventories: (tTeam * inventory) list;
  trades: trade_offer list;
}

type tMove =
  | DevCard of tDevCard
  | Trade of trade_offer
  | BuildRoad of (tTeam * edge_coord)
  | BuildSettlement of (tTeam * vertex_coord)
  | BuildCity of (tTeam * vertex_coord)