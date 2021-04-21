open Graphics

module Vec2 = struct
  type vec = float * float

  let vec_of_floats x y : vec = (x, y)

  let vec_of_float a : vec = (a, a)

  let vec_of_ints x y : vec = (float_of_int x, float_of_int y)

  let vec_of_int_tpl (x, y) : vec = (float_of_int x, float_of_int y)

  let zero = vec_of_float 0.0

  let one = vec_of_float 1.0

  let swap ((x, y) : vec) = vec_of_floats y x

  let x_of ((x, y) : vec) = x

  let x_int_of ((x, y) : vec) = int_of_float x

  let y_of ((x, y) : vec) = y

  let y_int_of ((x, y) : vec) = int_of_float y

  let ints_of_vec ((x, y) : vec) = (int_of_float x, int_of_float y)

  let element_wise_op (op : float -> float -> float) v1 v2 : vec =
    let x = op (x_of v1) (x_of v2) in
    let y = op (y_of v1) (y_of v2) in
    (x, y)

  let ( - ) = element_wise_op ( -. )

  let ( + ) = element_wise_op ( +. )

  let ( * ) = element_wise_op ( *. )

  let scale s ((x, y) : vec) : vec = (x *. s, y *. s)
end

open Vec2

let half_rt_3 = sqrt 3.0 /. 2.0

let hex_size = 50.0

let hex_spacing =
  vec_of_floats (half_rt_3 *. 2.0 *. hex_size) (1.5 *. hex_size)

let board_size =
  vec_of_floats (5.0 *. x_of hex_spacing) (31.0 /. 4.0 *. hex_size)

let screen_size = vec_of_ints 960 540

let board_pos =
  screen_size - board_size
  |> scale 0.5
  |> ( + ) (vec_of_floats (x_of hex_spacing *. 0.5) hex_size)

let config =
  " "
  ^ (screen_size |> x_int_of |> string_of_int)
  ^ "x"
  ^ (screen_size |> y_int_of |> string_of_int)
  ^ "+100-100"

let c_bg = rgb 52 143 235

let render () = Graphics.synchronize ()

let color_of_hex = function
  | Types.Desert -> rgb 227 226 182
  | Other (_, res) -> (
      match res with
      | Types.Wood -> rgb 81 125 25
      | Types.Sheep -> rgb 142 217 104
      | Types.Wheat -> rgb 240 173 0
      | Types.Brick -> rgb 156 67 0
      | Types.Ore -> rgb 123 111 131)

let unit_hexagon_coords : Vec2.vec array =
  [|
    (0.0, 1.0);
    (half_rt_3, 0.5);
    (half_rt_3, -0.5);
    (0.0, -1.0);
    (-1.0 *. half_rt_3, -0.5);
    (-1.0 *. half_rt_3, 0.5);
  |]

let fill_hex size pos hex =
  hex |> color_of_hex |> set_color;
  let map_coord (v : vec) = scale size v + pos |> ints_of_vec in
  let vertex_coords = unit_hexagon_coords |> Array.map map_coord in
  vertex_coords |> fill_poly;
  set_color black;
  vertex_coords |> draw_poly

let clear color =
  Graphics.set_color color;
  let w, h = Vec2.ints_of_vec (screen_size - vec_of_ints 1 1) in
  Graphics.fill_rect 0 0 w h

let pixel_of_hex_coords (v : vec) =
  let row_shift =
    if y_int_of v mod 2 == 0 then (0.0, 0.0)
    else (x_of hex_spacing /. -2.0, 0.0)
  in
  (v * hex_spacing) + row_shift

let setup_window () =
  Graphics.open_graph config;
  Graphics.set_window_title "OCaml Catan";
  Graphics.auto_synchronize false;
  clear c_bg

let print_board (board : Board.t) =
  setup_window ();
  for i = 0 to 18 do
    let coords =
      i |> Board.hex_coords |> vec_of_int_tpl |> swap
      |> pixel_of_hex_coords
    in
    let hex = Board.hex_info board i in
    fill_hex hex_size (board_pos + coords) hex
    (* List.iteri (fun i vertex -> draw_vertex raster vertex i coords)
       (Board.hex_to_vertices board i); List.iteri (fun i e -> draw_road
       raster e i coords) (Board.hex_to_edges board i) *)
  done;
  render ();
  Graphics.loop_at_exit [] ignore
