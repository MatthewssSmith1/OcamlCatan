let print_pixel index =
  let color_arr =
    [
      ANSITerminal.white;
      ANSITerminal.black;
      ANSITerminal.red;
      ANSITerminal.green;
      ANSITerminal.yellow;
      ANSITerminal.blue;
      ANSITerminal.magenta;
      ANSITerminal.cyan;
    ]
  in
  let color =
    match List.nth_opt color_arr index with
    | None -> ANSITerminal.white
    | Some c -> c
  in
  ANSITerminal.print_string [ color ] "██"

(** print_raster [r] prints [r] (a raster, or rectangular set of pixels
    in the form of a 2D array) to the terminal *)
let rec print_raster =
  Array.iter (fun row ->
      Array.iter print_pixel row;
      print_endline "")

let draw_hex raster (color : int) coords =
  let is_in_bounds i j =
    i + j > 2 && i + j < 12 && i - j < 4 && i - j > -6
  in
  for i = 0 to 6 do
    for j = 0 to 8 do
      if is_in_bounds i j then
        try raster.(snd coords + j).(fst coords + i) <- color
        with _ -> ()
    done
  done

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

let hex_to_pixel_coords coords =
  let y = (fst coords * 7) + 1 in
  let row_shift = if y mod 2 == 0 then -1 else 3 in
  let x = (snd coords * 8) + row_shift in
  (x, y)

let draw_board raster =
  for i = 0 to 18 do
    i |> hex_coords |> hex_to_pixel_coords |> draw_hex raster (2 + Random.int 6)
  done

let make_raster = Array.make_matrix 39 45 0

let print_board =
  let r = make_raster in
  draw_board r;
  print_raster r

