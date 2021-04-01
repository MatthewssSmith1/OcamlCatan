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

let draw_road raster style dir coords = ()

let vertex_dir_offsets =
  [ (3, -1); (7, 2); (7, 6); (3, 9); (-1, 6); (-1, 2) ]

let draw_vertex raster style dir coords =
  let offset = List.nth vertex_dir_offsets dir in
  raster.(snd coords + snd offset).(fst coords + fst offset) <-
    { ansi_style = [ style ]; content = "  " }

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
        try raster.(y).(x) <- { ansi_style = [ style; white ]; content }
        with _ -> ()
    done
  done

(** placed here just for testing purposes*)
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

let hex_to_pixel_coords hex_coords =
  let y = (fst hex_coords * 7) + 1 in
  (* offsets every other row by half a hex horizontally*)
  let row_shift = if y mod 2 == 0 then -1 else 3 in
  let x = (snd hex_coords * 8) + row_shift in
  (x, y)

let draw_board raster =
  for i = 0 to 18 do
    let color =
      match List.nth_opt bg_colors (1 + Random.int 7) with
      | None -> ANSITerminal.default
      | Some c -> c
    in
    let coords = hex_coords i in
    coords |> hex_to_pixel_coords
    |> draw_hex raster color (Random.int 12)
  done

let print_board =
  let r =
    Array.make_matrix 39 45
      { ansi_style = [ on_white ]; content = "  " }
  in
  draw_board r;
  print_raster r
