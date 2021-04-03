open ANSITerminal

(** ANSI functions:
    https://github.com/Chris00/ANSITerminal/blob/master/src/ANSITerminal.mli *)

let numbers =
  [
    "①";
    "②";
    "③";
    "④";
    "⑤";
    "⑥";
    "⑦";
    "⑧";
    "⑨";
    "⑩";
    "⑪";
    "⑫";
  ]

type pixel = {
  ansi_style : ANSITerminal.style list;
  content : string;
}

let print_pixel pixel =
  ANSITerminal.print_string pixel.ansi_style pixel.content

(** print_raster [r] prints [r] (a raster, or rectangular set of pixels
    in the form of a 2D array) to the terminal *)
let rec print_raster =
  Array.iter (fun row ->
      Array.iter print_pixel row;
      print_endline "")

let team_color_to_ansi = function
  | Types.Red -> on_red
  | Types.Blue -> on_blue
  | Types.Green -> on_cyan
  | Types.White -> on_white

let hex_to_ansi_color = function
  | Types.Desert -> on_yellow
  | Types.Other (_, Types.Brick) -> on_red
  | Types.Other (_, Types.Wood) -> on_green
  | Types.Other (_, Types.Wheat) -> on_yellow
  | Types.Other (_, Types.Ore) -> on_magenta
  | Types.Other (_, Types.Sheep) -> on_cyan

let hex_to_pixel_coords hex_coords =
  let y = (fst hex_coords * 7) + 1 in
  (* offsets every other row by half a hex horizontally*)
  let row_shift = if y mod 2 == 0 then -1 else 3 in
  let x = (snd hex_coords * 8) + row_shift in
  (x, y)

let edge_dir_offsets =
  [
    [ (4, -1); (4, 0); (5, 0); (5, 1); (6, 1); (6, 2) ];
    [ (7, 3); (7, 4); (7, 5) ];
    [ (6, 6); (6, 7); (5, 7); (5, 8); (4, 8); (4, 9) ];
    [ (2, 9); (2, 8); (1, 8); (1, 7); (0, 7); (0, 6) ];
    [ (-1, 5); (-1, 4); (-1, 3) ];
    [ (0, 2); (0, 1); (1, 1); (1, 0); (2, 0); (2, -1) ];
  ]

let draw_road raster (edge : Types.edge) dir coords =
  (* used for debugging, draw all road around a certain hex *)
  (* let (edge : Types.edge) = match coords with 1, 3 -> Types.Road
     Types.Red | _ -> edge in *)
  match edge with
  | Empty -> ()
  | Road team_color ->
      print_endline
        (string_of_int (fst coords) ^ ", " ^ string_of_int (snd coords));

      let pixel_coords = hex_to_pixel_coords coords in
      let draw_offset offset =
        try
          raster.(snd pixel_coords + snd offset).(fst pixel_coords
                                                  + fst offset) <-
            {
              ansi_style = [ team_color_to_ansi team_color ];
              content = "  ";
            }
        with _ -> ()
      in
      List.iter draw_offset (List.nth edge_dir_offsets dir)

let vertex_dir_offsets =
  [ (3, -1); (7, 2); (7, 6); (3, 9); (-1, 6); (-1, 2) ]

let draw_vertex raster (vertex : Types.vertex) dir coords =
  let color =
    (* used for debugging, draw all verticies around a certain hex *)
    (* match coords with | 1, 3 -> on_cyan | _ -> *)
    match vertex with
    | Empty -> on_white
    | Settlement team_color -> team_color_to_ansi team_color
    | City team_color -> team_color_to_ansi team_color
  in
  if color != on_white then
    let _ =
      print_endline
        ("vertex"
        ^ string_of_int (fst coords)
        ^ ", "
        ^ string_of_int (snd coords))
    in
    let offset = List.nth vertex_dir_offsets dir in
    let pixel_coords = hex_to_pixel_coords coords in
    let x = fst pixel_coords + fst offset in
    let y = snd pixel_coords + snd offset in
    try raster.(y).(x) <- { ansi_style = [ color ]; content = "  " }
    with _ -> ()
  else ()

let draw_hex raster style number coords =
  (* checks if the pixel is in one of the corners, cuts off corners of
     rectangle to make a hex*)
  let is_in_bounds i j =
    i + j > 2 && i + j < 12 && i - j < 4 && i - j > -6
  in
  for i = 0 to 6 do
    for j = 0 to 8 do
      if is_in_bounds i j then
        let content =
          " "
          ^
          if (i, j) = (3, 4) then List.nth numbers (number - 1) else " "
        in
        let x = fst coords + i in
        let y = snd coords + j in
        try raster.(y).(x) <- { ansi_style = [ style; black ]; content }
        with _ -> ()
    done
  done

let draw_board raster (board : Board.t) =
  for i = 0 to 18 do
    (*print_endline (match Board.hex_info board i with | Types.Desert ->
      "desert" | Other (h, k) -> Types.resource_to_string k);*)
    let coords = Board.hex_coords i in
    let hex_info = Board.hex_info board i in
    let color = hex_to_ansi_color hex_info in
    let number =
      match hex_info with Types.Desert -> 7 | Other (x, _) -> x
    in
    coords |> hex_to_pixel_coords |> draw_hex raster color number;
    List.iteri
      (fun i vertex -> draw_vertex raster vertex i coords)
      (Board.hex_to_vertices board i);
    List.iteri
      (fun i e -> draw_road raster e i coords)
      (Board.hex_to_edges board i)
  done

let print_board (board : Board.t) =
  let r =
    Array.make_matrix 39 45
      { ansi_style = [ on_white ]; content = "  " }
  in
  draw_board r board;
  print_raster r
