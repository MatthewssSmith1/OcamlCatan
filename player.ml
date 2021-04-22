type t = {
  wood : int;
  sheep : int;
  wheat : int;
  brick : int;
  ore : int;
  knight : int;
  roadBuilding : int;
  yearOfPlenty : int;
  monopoly : int;
  victoryPoint : int;
  settlements : int;
  cities : int;
  roads : int;
  ports : Types.port list;
  color : Types.color;
}

exception Not_enough_resources

exception Not_enough_pieces

let add_resource resource amount player =
  match resource with
  | Types.Wood -> { player with wood = player.wood + amount }
  | Types.Sheep -> { player with sheep = player.sheep + amount }
  | Types.Wheat -> { player with wheat = player.wheat + amount }
  | Types.Brick -> { player with brick = player.brick + amount }
  | Types.Ore -> { player with ore = player.ore + amount }

let check_resource resource player =
  match resource with
  | Types.Wood -> player.wood
  | Types.Sheep -> player.sheep
  | Types.Wheat -> player.wheat
  | Types.Brick -> player.brick
  | Types.Ore -> player.ore

let remove_resource resource amount player =
  if check_resource resource player < amount then
    raise Not_enough_resources
  else
    match resource with
    | Types.Wood -> { player with wood = player.wood - amount }
    | Types.Sheep -> { player with sheep = player.sheep - amount }
    | Types.Wheat -> { player with wheat = player.wheat - amount }
    | Types.Brick -> { player with brick = player.brick - amount }
    | Types.Ore -> { player with ore = player.ore - amount }

let add_port port player = { player with ports = port :: player.ports }

let place_road player =
  if player.roads < 1 then raise Not_enough_pieces
  else { player with roads = player.roads - 1 }

let place_settlement player =
  if player.settlements < 1 then raise Not_enough_pieces
  else { player with settlements = player.settlements - 1 }

let place_city player =
  if player.cities < 1 then raise Not_enough_pieces
  else
    {
      player with
      cities = player.cities - 1;
      settlements = player.settlements + 1;
    }

let get_color player = player.color

let make_player some_color =
  {
    wood = 0;
    sheep = 0;
    wheat = 0;
    brick = 0;
    ore = 0;
    knight = 0;
    roadBuilding = 0;
    yearOfPlenty = 0;
    monopoly = 0;
    victoryPoint = 0;
    roads = 15;
    settlements = 5;
    cities = 4;
    ports = [];
    color = some_color;
  }

let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"

let to_string t =
  "Color: "
  ^ Types.color_to_string t.color
  (*Resources*)
  ^ "Wood: "
  ^ string_of_int t.wood ^ "Sheep: " ^ string_of_int t.sheep ^ "Wheat: "
  ^ string_of_int t.wheat ^ "Brick: " ^ string_of_int t.brick ^ "Ore: "
  ^ string_of_int t.ore ^ "\n" (*Dev Cards*) ^ "Knights: "
  ^ string_of_int t.knight ^ "Road Buildings: "
  ^ string_of_int t.roadBuilding
  ^ "Year of Plenties: "
  ^ string_of_int t.yearOfPlenty
  ^ "Monopolies: "
  ^ string_of_int t.monopoly
  ^ "Victory Points: "
  ^ string_of_int t.victoryPoint
  ^ "\n" (*Remaining*) ^ "Remaining Roads: "
  ^ string_of_int t.roads ^ "Remaining Settlements: "
  ^ string_of_int t.settlements
  ^ "Remaining Cities: " ^ string_of_int t.cities
