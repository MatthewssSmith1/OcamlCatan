type t = {
  hexes : Types.hex option array array;
  vertices : Types.vertex option array array;
  edges : Types.edge option array array;
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
      board.edges.(a).(b) <- Some (Road (Player.get_color player));
      board

let add_settlement player hex dir board =
  let coords = hex_coords hex in
  let a, b = vertex_from_hex coords dir in
  match board.vertices.(a).(b) with
  | None -> failwith "out of bounds"
  | Some Empty ->
      board.vertices.(a).(b) <-
        Some (Settlement (Player.get_color player));
      board
  | _ -> failwith "already exists"

let upgrade_city player hex dir board =
  let coords = hex_coords hex in
  let a, b = vertex_from_hex coords dir in
  match board.vertices.(a).(b) with
  | None -> failwith "out of bounds"
  | Some Empty -> failwith "can't build city on empty"
  | Some (Settlement c) ->
      if c <> Player.get_color player then
        failwith "can't build city on wrong color"
      else board.vertices.(a).(b) <- Some (Settlement c);
      board
  | Some (City _) -> failwith "can't build city on city"

let make_board_from_array tiles =
  let board =
    {
      hexes = Array.make 5 (Array.make 5 None);
      vertices = Array.make 6 (Array.make 11 None);
      edges = Array.make 11 (Array.make 11 None);
    }
  in
  for i = 0 to 18 do
    let x, y = hex_coords i in
    board.hexes.(x).(y) <- Some tiles.(i);
    for j = 0 to 5 do
      let a, b = vertex_from_hex (x, y) j in
      board.vertices.(a).(b) <- Some Empty;
      let c, d = edge_from_hex (x, y) j in
      board.edges.(c).(d) <- Some Empty
    done
  done;
  board

let basic =
  Types.
    [|
      Other (10, Ore);
      Other (2, Sheep);
      Other (9, Wood);
      Other (12, Wheat);
      Other (6, Brick);
      Other (4, Sheep);
      Other (10, Brick);
      Other (9, Wheat);
      Other (11, Wood);
      Desert;
      Other (3, Wood);
      Other (8, Ore);
      Other (8, Wood);
      Other (3, Ore);
      Other (4, Wheat);
      Other (5, Sheep);
      Other (5, Brick);
      Other (6, Wheat);
      Other (11, Sheep);
    |]

let make_board () = make_board_from_array basic

let make_random_board () =
  let shuffle a =
    let n = Array.length a in
    let a = Array.copy a in
    for i = n - 1 downto 1 do
      let k = Random.int (i + 1) in
      let x = a.(k) in
      a.(k) <- a.(i);
      a.(i) <- x
    done;
    a
  in
  make_board_from_array (shuffle basic)

let hex_info board n =
  let x, y = hex_coords n in
  match board.hexes.(x).(y) with
  | Some a -> a
  | None -> failwith "out of bounds"

let hex_to_vertices board n =
  let coords = hex_coords n in
  let rec hex_to_vertices_helper x acc =
    if x = 6 then acc
    else
      let a, b = vertex_from_hex coords x in
      let vertex =
        match board.vertices.(a).(b) with
        | Some v -> v
        | None -> failwith " out of bounds"
      in
      vertex :: hex_to_vertices_helper (x + 1) acc
  in
  hex_to_vertices_helper 0 []

let hex_to_edges board n =
  let coords = hex_coords n in
  let rec hex_to_edges_helper x acc =
    if x = 6 then acc
    else
      let a, b = edge_from_hex coords x in
      let edge =
        match board.edges.(a).(b) with
        | Some e -> e
        | _ -> failwith "out of bounds"
      in
      edge :: hex_to_edges_helper (x + 1) acc
  in
  hex_to_edges_helper 0 []
