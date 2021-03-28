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
  vertices: (vertex_coord * tVertex) list;
  inventories: (tTeam * inventory) list;
  trades: trade_offer list;
}

type tMove =
  | DevCard of tDevCard
  | Trade of trade_offer
  | BuildRoad of (tTeam * edge_coord)
  | BuildSettlement of (tTeam * vertex_coord)
  | BuildCity of (tTeam * vertex_coord)

let to_vec2_list (input : (int * int) list) =
  List.map (fun (a, b) -> {x = a; y = b}) input

let rec list_merger list_1 list_2 =
  match list_1, list_2 with
  | [], [] -> []
  | h1 :: t1, h2 :: t2 -> (h1, h2) :: list_merger t1 t2
  | _, _ -> failwith "Dimensions not equal"

let make_new_game (hex_list : tHex list) (ports : (int * tPort) list) =
  let hex_vec = to_vec2_list [(0, -6); (3, -6); (6, -6); (-3, -3); (0, -3);
  (3, -3); (6, -3); (-6, 0); (-3, 0); (0, 0); (3, 0); (6, 0);
  (-6, 3); (-3,3); (0, 3); (3, 3); (-6, 6); (-3, 6); (0, 6)] in
  let hexes = list_merger hex_vec hex_list in
  let vert_vec = to_vec2_list [(1, -8); (4, -8); (7, -8); (-1, -7);
  (2, -7); (5, -7); (8, -7); (-2, -5); (1, -5); (4, -5); (7, -5); (-4, -4);
  (-1, -4); (2, -4); (5, -4); (8, -4); (-5, -2); (-2, -2); (1, -2); (4, -2);
  (7, -2); (-7, -1); (-4, -1); (-1, -1); (2, -1); (5, -1); (8, -1); (-8, 1);
  (-5, 1); (-2, 1); (1, 1); (4, 1); (7, 1); (-7, 2); (-4, 2); (-1, 2); (2, 2);
  (5, 2); (-8, 4); (-5, 4); (-2, 4); (1, 4); (4, 4); (-7, 5); (-4, 5); (-1, 5);
  (2, 5); (-8, 7); (-5, 7); (-2, 7); (1, 7); (-7, 8); (-4, 8); (-1, 8)] in
  let verticies = List.map (fun x -> (x, (Empty : tVertex))) vert_vec in
  {hexes = hexes; ports = ports; edges = []; vertices = verticies;
  inventories = []; trades = []}