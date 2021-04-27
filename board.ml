type t = {
  hexes : Types.hex option array array;
  vertices : Types.vertex option array array;
  edges : Types.edge option array array;
  robber : int;
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

let hex_info board n =
  let x, y = hex_coords n in
  (* match board.hexes.(x).(y) with *)
  match Array.get (Array.get board.hexes x) y with
  | Some a -> a
  | None -> failwith "out of bounds"

let hex_from_hex (x, y) dir =
  let offset = -(x mod 2) in
  match dir with
  | 0 -> (2 * x, (2 * y) + 1 + offset)
  | 1 -> ((2 * x) + 1, (2 * y) + 2 + offset)
  | 2 -> ((2 * x) + 2, (2 * y) + 1 + offset)
  | 3 -> ((2 * x) + 2, (2 * y) + offset)
  | 4 -> ((2 * x) + 1, (2 * y) + offset)
  | 5 -> (2 * x, (2 * y) + offset)
  | _ -> failwith "dir not in [0,5]"

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

let has_road player hex dir board =
  let coords = hex_coords hex in
  let a, b = edge_from_hex coords dir in
  match board.edges.(a).(b) with
  | Some (Road color) ->
      if Player.get_color player = color then true else false
  | None -> false
  | Some Empty -> false

let has_settlement player hex dir board =
  let coords = hex_coords hex in
  let a, b = vertex_from_hex coords dir in
  match board.vertices.(a).(b) with
  | Some (Settlement color) ->
      if Player.get_color player = color then true else false
  | Some (City color) ->
      if Player.get_color player = color then true else false
  | None -> false
  | Some Empty -> false

let can_add_road player hex dir board = failwith "Unimplemented"

let can_add_settlement player hex dir board = failwith "Unimplemented"

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

let move_robber hex state = { state with robber = hex }

let find_desert board =
  let rec desert_helper board counter =
    if counter > 18 then failwith "Desert not found"
    else if hex_info board counter = Types.Desert then counter
    else desert_helper board (counter + 1)
  in
  desert_helper board 0

let make_board_from_array tiles =
  let board =
    {
      (* hexes = Array.make 5 (Array.make 5 None); *)
      hexes = Array.make_matrix 5 5 None;
      (* vertices = Array.make 6 (Array.make 11 None); *)
      vertices = Array.make_matrix 6 11 None;
      (* edges = Array.make 11 (Array.make 11 None); *)
      edges = Array.make_matrix 11 11 None;
      robber = -1;
    }
  in
  for i = 0 to 18 do
    let x, y = hex_coords i in
    board.hexes.(x).(y) <- Some tiles.(i);
    (* print_endline (string_of_int x ^ ", " ^ string_of_int y ^ ", " ^
       Types.hex_to_string tiles.(i)); *)
    for j = 0 to 5 do
      let a, b = vertex_from_hex (x, y) j in
      board.vertices.(a).(b) <- Some Empty;
      let c, d = edge_from_hex (x, y) j in
      board.edges.(c).(d) <- Some Empty
    done
  done;
  { board with robber = find_desert board }

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
  Random.init (Int.of_float (Unix.time ()));
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

let int_to_hex_list board input =
  let rec helper board input counter =
    if counter > 18 then []
    else
      match hex_info board counter with
      | Desert -> helper board input (counter + 1)
      | Other (x, y) ->
          if x = input && counter != board.robber then
            counter :: helper board input (counter + 1)
          else helper board input (counter + 1)
  in
  helper board input 0

let vert_to_adj_edge_coords board x y =
  let offset = if x mod 2 = y mod 2 then 1 else -1 in
  let unchecked_coords =
    [ (x * 2, y - 1); (x * 2, y); ((x * 2) + offset, y) ]
  in
  List.filter
    (fun (a, b) -> board.edges.(a).(b) <> None)
    unchecked_coords

let vert_to_adj_edges board hex dir =
  let x, y = vertex_from_hex (hex_coords hex) dir in
  let coords = vert_to_adj_edge_coords board x y in
  List.map
    (fun (a, b) ->
      match board.edges.(a).(b) with
      | Some i -> i
      | _ -> failwith "should not occur")
    coords

let vert_to_adj_hex_coords board x y =
  let xOffset = x mod 2 in
  let yOffset = y mod 2 in
  let secX = if xOffset + yOffset = 1 then x - 1 else x in
  let secY = if yOffset = 1 then (y / 2) + 1 else (y / 2) - 2 in
  let unchecked_coords = [ (x, y / 2); (secX, secY); (x - 1, y / 2) ] in
  List.filter
    (fun (a, b) ->
      try board.hexes.(a).(b) <> None with Invalid_argument _ -> false)
    unchecked_coords

let vert_to_adj_hexes board hex dir =
  let x, y = vertex_from_hex (hex_coords hex) dir in
  let coords = vert_to_adj_hex_coords board x y in
  List.map
    (fun (a, b) ->
      match board.hexes.(a).(b) with
      | Some i -> i
      | _ -> failwith "should not occur")
    coords

let edge_to_adj_vert_coords board x y =
  let offset = if x mod 2 = 0 then 0 else 1 in
  let unchecked_coords =
    [ ((x - offset) / 2, y); ((x + offset) / 2, y + (1 - offset)) ]
  in
  List.filter
    (fun (a, b) -> board.vertices.(a).(b) <> None)
    unchecked_coords

let edge_to_adj_verts board hex dir =
  let x, y = edge_from_hex (hex_coords hex) dir in
  let coords = edge_to_adj_vert_coords board x y in
  List.map
    (fun (a, b) ->
      match board.vertices.(a).(b) with
      | Some i -> i
      | _ -> failwith "should not occur")
    coords

let edge_to_adj_edge_coords board x y =
  let adj_vertices = edge_to_adj_vert_coords board x y in
  let all_edges =
    List.map
      (fun (a, b) -> vert_to_adj_edge_coords board a b)
      adj_vertices
  in
  List.flatten all_edges |> List.sort_uniq compare
  |> List.filter (fun p -> p <> (x, y))

let edge_to_adj_edges board hex dir =
  let x, y = edge_from_hex (hex_coords hex) dir in
  let coords = edge_to_adj_edge_coords board x y in
  List.map
    (fun (a, b) ->
      match board.edges.(a).(b) with
      | Some i -> i
      | _ -> failwith "should not occur")
    coords

let vert_to_adj_vert_coords board x y =
  let adj_edges = vert_to_adj_edge_coords board x y in
  let all_vertices =
    List.map (fun (a, b) -> edge_to_adj_vert_coords board a b) adj_edges
  in
  List.flatten all_vertices
  |> List.sort_uniq compare
  |> List.filter (fun p -> p <> (x, y))

let vert_to_adj_verts board hex dir =
  let x, y = vertex_from_hex (hex_coords hex) dir in
  let coords = vert_to_adj_vert_coords board x y in
  List.map
    (fun (a, b) ->
      match board.vertices.(a).(b) with
      | Some i -> i
      | _ -> failwith "should not occur")
    coords
