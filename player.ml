type t = {
  wood : int;
  sheep : int;
  wheat : int;
  brick : int;
  ore : int;
  devs : Game_state.devCard list;
  settlements : int;
  cities : int;
  roads : int;
  ports : Game_state.port list;
  color : Game_state.color;
}

exception Not_enough_resources

exception Not_enough_pieces

let add_resource resource amount player = 
  match resource with
  | Game_state.Wood -> {player with wood = player.wood + amount}
  | Game_state.Sheep -> {player with sheep = player.sheep + amount}
  | Game_state.Wheat -> {player with wheat = player.wheat + amount}
  | Game_state.Brick -> {player with brick = player.brick + amount}
  | Game_state.Ore -> {player with ore = player.ore + amount}


let check_resource resource player = 
  match resource with
  | Game_state.Wood -> player.wood
  | Game_state.Sheep -> player.sheep
  | Game_state.Wheat -> player.wheat
  | Game_state.Brick -> player.brick
  | Game_state.Ore -> player.ore

let remove_resource resource amount player = 
  if check_resource resource player < amount
  then raise Not_enough_resources
  else match resource with
  | Game_state.Wood -> {player with wood = player.wood - amount}
  | Game_state.Sheep -> {player with sheep = player.sheep - amount}
  | Game_state.Wheat -> {player with wheat = player.wheat - amount}
  | Game_state.Brick -> {player with brick = player.brick - amount}
  | Game_state.Ore -> {player with ore = player.ore - amount}

let add_port port player = 
  {player with ports = port :: player.ports}

let place_road player =
  if player.roads < 1
  then raise Not_enough_pieces
  else {player with roads = player.roads - 1}

let place_settlement player =
  if player.settlements < 1
    then raise Not_enough_pieces
    else {player with settlements = player.settlements - 1}
let place_city player =
  if player.cities < 1
    then raise Not_enough_pieces
    else {player with cities = player.cities - 1;
    settlements = player.settlements + 1}
let make_player color =
  {
    wood = 0;
    sheep = 0;
    wheat = 0;
    brick = 0;
    ore = 0;
    devs = [];
    roads = 15;
    settlements = 5;
    cities = 4;
    ports = [];
    color = color;
  }
