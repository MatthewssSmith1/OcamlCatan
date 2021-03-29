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

(* All coordinates are done in axial coordinates,
with x being down and right and y being right and (0, 0) in the middle*)
(* The coordinates of the centers of the hexes*)
let hex_vec = to_vec2_list [(0, -12); (6, -12); (12, -12); (-6, -6);
  (0, -6); (6, -6); (12, -6); (-12, 0); (-6, 0); (0, 0); (6, 0); (12, 0);
  (-12, 6); (-6,6); (0, 6); (6, 6); (-12, 12); (-6, 12); (0, 12)]

(* The coordinates of the centers of the edges*)
let edge_vec = to_vec2_list [(0, -15); (3, -15); (6, -15); (9, -15);
  (12, -15); (-3, -12); (3, -12); (9, 12); (15, 12); (-6, -9); (-3, -9);
  (0, -9); (3, -9); (6, -9); (9, -9); (12, -9); (15, -9); (-9, -6);
  (-3, -6); (3, -6); (9, -6); (15, -6); (-12, -3); (-9, -3); (-6, -3);
  (-3, -3); (0, -3); (3, -3); (6, -3); (9, -3); (12, -3); (15, -3);
  (-15, 0); (-9, 0); (-3, 0); (3, 0); (9, 0); (15, 0); (-15, 3); (-12, 3);
  (-9, 3); (-6, 3); (-3, 3); (0, 3); (3, 3); (6, 3); (9, 3); (12, 3);
  (-15, 6); (-9, 6); (-3, 6); (3, 6); (9, 6); (-15, 9); (-12, 9); (-9, 9);
  (-6, 9); (-3, 9); (0, 9); (3, 9); (6, 9); (-15, 12); (-9, 12); (-3, 12);
  (3, 12); (-15, 15); (-12, 15); (-9, 15); (-6, 15); (-3, 15); (0, 15);]

(* The coordinates of the verticies*)
let vert_vec = to_vec2_list [(2, -16); (8, -16); (14, -16); (-2, -14);
  (4, -14); (10, -14); (16, -14); (-4, -10); (2, -10); (8, -10); (14, -10);
  (-8, -8); (-2, -8); (4, -8); (10, -8); (16, -8); (-10, -4); (-4, -4);
  (2, -4); (8, -4); (14, -4); (-14, -2); (-8, -2); (-2, -2); (4, -2);
  (10, -2); (16, -2); (-16, 2); (-10, 2); (-4, 2); (2, 2); (8, 2); (14, 2);
  (-14, 4); (-8, 4); (-2, 4); (4, 4); (10, 4); (-16, 8); (-10, 8); (-4, 8);
  (2, 8); (8, 8); (-14, 10); (-8, 10); (-2, 10); (4, 10); (-16, 14);
  (-10, 14); (-4, 14); (2, 14); (-14, 16); (-8, 16); (-2, 16)]

(*Creates a new empty game based on the inputs*)
let make_new_game (hex_list : tHex list) (ports : (int * tPort) list) =
  let hexes = list_merger hex_vec hex_list in
  let edges = List.map (fun x -> (x, (Empty : tEdge))) edge_vec in
  let verticies = List.map (fun x -> (x, (Empty : tVertex))) vert_vec in
  {hexes = hexes; ports = ports; edges = edges; vertices = verticies;
  inventories = []; trades = []}

let rec thing_finder list input =
  match list with
  | [] -> None
  | h :: t ->
    match h with
    | (a, b) -> if input = a then Some b else thing_finder t input

(* If input vec is a hex, then return adjacent verticies.
If input is a vertex, then return adjacent verticies. *)
let find_adj (state : game_state) (input : vec2) =
  let x = input.x in
  let y = input.y in
  let vecs = to_vec2_list [(x+2, y-4); (x-2, y-2); (x+4, y-2);
  (x-4, y+2); (x+2, y+2); (x-2, y+4)] in
  let rec option_remover list =
    match list with
    | [] -> []
    | h :: t ->
      match h with
      | None -> option_remover t
      | Some x -> x :: (option_remover t) in
  option_remover (List.map (thing_finder state.vertices) vecs)