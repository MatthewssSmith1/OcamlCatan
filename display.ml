open ANSITerminal

(** ANSI functions:
    https://github.com/Chris00/ANSITerminal/blob/master/src/ANSITerminal.mli *)
let bg_colors =
  [
    on_white;
    on_black;
    on_red;
    on_green;
    on_yellow;
    on_blue;
    on_magenta;
    on_cyan;
  ]

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
  match edge with
  | Empty -> ()
  | Road color ->
      let offsets = List.nth edge_dir_offsets dir in
      List.iter
        (fun offset ->
          raster.(snd coords + snd offset).(fst coords + fst offset) <-
            {
              ansi_style = [ team_color_to_ansi color ];
              content = "  ";
            })
        offsets

let vertex_dir_offsets =
  [ (3, -1); (7, 2); (7, 6); (3, 9); (-1, 6); (-1, 2) ]

let draw_vertex raster (vertex : Types.vertex) dir coords =
  match vertex with
  | Empty -> ()
  | _ ->
      let offset = List.nth vertex_dir_offsets dir in
      raster.(snd coords + snd offset).(fst coords + fst offset) <-
        { ansi_style = [ on_cyan ]; content = "  " }

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
          " " ^ if (i, j) = (3, 4) then List.nth numbers number else " "
        in
        let x = fst coords + i in
        let y = snd coords + j in
        try raster.(y).(x) <- { ansi_style = [ style; black ]; content }
        with _ -> ()
    done
  done

let hex_to_pixel_coords hex_coords =
  let y = (fst hex_coords * 7) + 1 in
  (* offsets every other row by half a hex horizontally*)
  let row_shift = if y mod 2 == 0 then -1 else 3 in
  let x = (snd hex_coords * 8) + row_shift in
  (x, y)

let draw_board raster (board : Board.t) =
  for i = 0 to 18 do
    let coords = Board.hex_coords i in
    let color = hex_to_ansi_color (Board.hex_info board i) in
    let number = match Board.hex_info board i with
      | Types.Desert -> 7
      | Other (x,_) -> x in
    coords |> hex_to_pixel_coords
    |> draw_hex raster color number;
    (* List.iteri
      (fun i v -> draw_vertex raster v i coords)
      (Board.hex_to_vertices board i);
    List.iteri
      (fun i e -> draw_road raster e i coords)
      (Board.hex_to_edges board i) *)
  done

let print_board (board : Board.t) =
  let r =
    Array.make_matrix 39 45
      { ansi_style = [ on_white ]; content = "  " }
  in
  draw_board r board;
  print_raster r
