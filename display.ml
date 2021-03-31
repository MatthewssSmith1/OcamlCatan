let color_arr = [| ANSITerminal.red; ANSITerminal.green |]

let rec print_row (row : int list) =
  match row with
  | [] -> ()
  | hd :: tl ->
      ANSITerminal.print_string [ color_arr.[hd] ] "██";
      print_row tl

(** print_raster [r] prints [r] (a raster, or rectangular set of
    pyixels) to the terminal *)
let rec print_raster (raster : int list list) =
  match raster with
  | [] -> ()
  | hd :: tl ->
      print_row hd;
      print_raster tl

let print_board = failwith "Unimplemented"
