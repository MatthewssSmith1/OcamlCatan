type t = {
  hexes : Game_state.hex option array array;
  vertices : Game_state.vertex option array array;
  edges : Game_state.edge option array array;
}

let hex_coords = function
  | 0 -> (0, 1)
  | 1 -> (0, 2)
  | 2 -> (0, 3)
  | 3 -> (1, 1)
  | 4 -> (1, 2)
  | 5 -> (1, 3)
  | 6 -> (1, 4)
  | 7 -> (2, 0)
  | 8 -> (2, 1)
  | 9 -> (2, 2)
  | 10 -> (2, 3)
  | 11 -> (2, 4)
  | 12 -> (3, 1)
  | 13 -> (3, 2)
  | 14 -> (3, 3)
  | 15 -> (3, 4)
  | 16 -> (4, 1)
  | 17 -> (4, 2)
  | 18 -> (4, 3)
  | _ -> failwith "out of bounds"

let edge_from_hex (x, y) dir =
  let offset = -(x mod 2) in
  match dir with
  | 0 -> (2 * x, (2 * y) + 1 + offset)
  | 1 -> ((2 * x) + 1, (2 * y) + 2 + offset)
  | 2 -> ((2 * x) + 2, (2 * y) + 1 + offset)
  | 3 -> ((2 * x) + 2, (2 * y) + offset)
  | 4 -> ((2 * x) + 1, (2 * y) + offset)
  | 5 -> (2 * x, (2 * y) + offset)
  | _ -> failwith "dir not in [0,5]"

let vertex_from_hex (x, y) dir =
  let offset = -(x mod 2) in
  match dir with
  | 0 -> (x, (2 * y) + 1 + offset)
  | 1 -> (x, (2 * y) + 2 + offset)
  | 2 -> (x + 1, (2 * y) + 2 + offset)
  | 3 -> (x + 1, (2 * y) + 1 + offset)
  | 4 -> (x + 1, (2 * y) + offset)
  | 5 -> (x, (2 * y) + offset)
  | _ -> failwith "dir not in [0,5]"

let add_road player hex dir board =
  let coords = hex_coords hex in
  let a, b = edge_from_hex coords dir in
  match board.edges.(a).(b) with
  | Some (Road _) -> failwith "road already exists"
  | None -> failwith "out of bounds"
  | Some Empty ->
      board.edges.(a).(b) = Some (Road (*player*) Red);
      board

let add_settlement player hex dir board =
  let coords = hex_coords hex in
  let a, b = vertex_from_hex coords dir in
  match board.vertices.(a).(b) with
  | None -> failwith "out of bounds"
  | Some Empty ->
      board.vertices.(a).(b) = Some (Settlement (*player*) Red);
      board
  | _ -> failwith "already exists"

let upgrade_city player hex dir board =
  let coords = hex_coords hex in
  let a, b = vertex_from_hex coords dir in
  match board.vertices.(a).(b) with
  | None -> failwith "out of bounds"
  | Some Empty -> failwith "can't build city on empty"
  | Some (Settlement c) ->
      if c <> player then failwith "can't build city on wrong color"
      else board.vertices.(a).(b) = Some (Settlement c);
      board
  | Some (City _) -> failwith "can't build city on city"
