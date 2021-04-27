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
  newKnight : int;
  newRoadBuilding : int;
  newYearOfPlenty : int;
  newMonopoly : int;
  settlements : int;
  cities : int;
  roads : int;
  ports : Types.port list;
  color : Types.color;
}

let can_add_road player =
  player.roads >= 1 && player.brick >= 1 && player.wood >= 1

let can_add_settlement player =
  player.settlements >= 1 && player.brick >= 1 && player.wood >= 1
  && player.sheep >= 1 && player.wheat >= 1

let can_add_city player =
  player.cities >= 1 && player.ore >= 3 && player.wheat >= 2

let can_add_dev player =
  player.sheep >= 1 && player.ore >= 1 && player.wheat >= 1

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

let rec add_resource_list list player =
  match list with
  | [] -> player
  | h :: t -> add_resource_list t (add_resource h 1 player)

let remove_resource resource amount player =
  if check_resource resource player < amount then
    failwith "Not Enough Resources"
  else
    match resource with
    | Types.Wood -> { player with wood = player.wood - amount }
    | Types.Sheep -> { player with sheep = player.sheep - amount }
    | Types.Wheat -> { player with wheat = player.wheat - amount }
    | Types.Brick -> { player with brick = player.brick - amount }
    | Types.Ore -> { player with ore = player.ore - amount }

let rec remove_resource_list list player =
  match list with
  | [] -> player
  | h :: t -> remove_resource_list t (remove_resource h 1 player)

let add_dev dev amount player =
  match dev with
  | Types.Knight -> { player with knight = player.knight + amount }
  | Types.RoadBuilding ->
      { player with newRoadBuilding = player.newRoadBuilding + amount }
  | Types.YearOfPlenty ->
      { player with newYearOfPlenty = player.newYearOfPlenty + amount }
  | Types.Monopoly ->
      { player with newMonopoly = player.newMonopoly + amount }
  | Types.VictoryPoint ->
      { player with victoryPoint = player.victoryPoint + amount }

let check_dev dev player =
  match dev with
  | Types.Knight -> player.knight
  | Types.RoadBuilding -> player.roadBuilding
  | Types.YearOfPlenty -> player.yearOfPlenty
  | Types.Monopoly -> player.monopoly
  | Types.VictoryPoint -> player.victoryPoint

let remove_dev dev amount player =
  if check_dev dev player < amount then
    failwith "Not Enough Develpment Cards"
  else
    match dev with
    | Types.Knight -> { player with knight = player.knight - amount }
    | Types.RoadBuilding ->
        { player with roadBuilding = player.roadBuilding - amount }
    | Types.YearOfPlenty ->
        { player with yearOfPlenty = player.yearOfPlenty - amount }
    | Types.Monopoly ->
        { player with monopoly = player.monopoly - amount }
    | Types.VictoryPoint ->
        { player with victoryPoint = player.victoryPoint - amount }

let add_port port player = { player with ports = port :: player.ports }

let place_road player =
  if player.roads <= 13 then
    if not (player.roads >= 1) then failwith "Not Enough Pieces"
    else if not (player.brick >= 1 && player.wood >= 1) then
      failwith "Not Enough Resources"
    else
      {
        player with
        roads = player.roads - 1;
        brick = player.brick - 1;
        wood = player.wood - 1;
      }
  else player

let place_settlement player =
  if player.settlements <= 3 then
    if not (player.settlements >= 1) then failwith "Not Enough Pieces"
    else if
      not
        (player.brick >= 1 && player.wood >= 1 && player.sheep >= 1
       && player.wheat >= 1)
    then failwith "Not Enough Resources"
    else
      {
        player with
        settlements = player.settlements - 1;
        brick = player.brick - 1;
        wood = player.wood - 1;
        sheep = player.sheep - 1;
        wheat = player.wheat - 1;
      }
  else player

let place_city player =
  if not (player.cities >= 1) then failwith "Not Enough Pieces"
  else if not (player.ore >= 3 && player.wheat >= 2) then
    failwith "Not Enough Resources"
  else
    {
      player with
      cities = player.cities - 1;
      settlements = player.settlements + 1;
      ore = player.ore - 3;
      wheat = player.wheat - 2;
    }

let buy_dev player dev =
  if not (player.sheep >= 1 && player.ore >= 1 && player.wheat >= 1)
  then failwith "Not Enough Resources"
  else
    add_dev dev 1
      {
        player with
        sheep = player.sheep - 1;
        ore = player.ore - 1;
        wheat = player.wheat - 1;
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
    newKnight = 0;
    newRoadBuilding = 0;
    newYearOfPlenty = 0;
    newMonopoly = 0;
    victoryPoint = 0;
    roads = 15;
    settlements = 5;
    cities = 4;
    ports = [];
    color = some_color;
  }

let end_turn player =
  {
    player with
    knight = player.knight + player.newKnight;
    roadBuilding = player.roadBuilding + player.newRoadBuilding;
    yearOfPlenty = player.yearOfPlenty + player.newYearOfPlenty;
    monopoly = player.monopoly + player.newMonopoly;
    newKnight = 0;
    newRoadBuilding = 0;
    newYearOfPlenty = 0;
    newMonopoly = 0;
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
  ^ string_of_int t.ore ^ "\n" (*Playable Dev Cards*) ^ "Knights: "
  ^ string_of_int t.knight ^ "Road Buildings: "
  ^ string_of_int t.roadBuilding
  ^ "Year of Plenties: "
  ^ string_of_int t.yearOfPlenty
  ^ "Monopolies: "
  ^ string_of_int t.monopoly
  ^ "Victory Points: "
  ^ string_of_int t.victoryPoint
  ^ "\n" (*Unplayable Dev Cards*) ^ "New Knights: "
  ^ string_of_int t.newKnight
  ^ "New Road Buildings: "
  ^ string_of_int t.newRoadBuilding
  ^ "New Year of Plenties: "
  ^ string_of_int t.newYearOfPlenty
  ^ "New Monopolies: "
  ^ string_of_int t.newMonopoly
  ^ "\n" (*Remaining*) ^ "Remaining Roads: "
  ^ string_of_int t.roads ^ "Remaining Settlements: "
  ^ string_of_int t.settlements
  ^ "Remaining Cities: " ^ string_of_int t.cities

let num_resources player =
  player.wood + player.sheep + player.wheat + player.brick + player.ore

let num_devs player =
  player.knight + player.roadBuilding + player.yearOfPlenty
  + player.monopoly + player.victoryPoint + player.newKnight
  + player.newRoadBuilding + player.newYearOfPlenty + player.newMonopoly
