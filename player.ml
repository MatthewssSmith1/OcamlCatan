type t = {
  wood : int;
  sheep : int;
  wheat : int;
  brick : int;
  ore : int;
  devs : Types.devCard list;
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
    color;
  }
