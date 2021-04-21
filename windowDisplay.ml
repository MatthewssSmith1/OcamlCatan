open Graphics

module Vec2 = struct
  type vec = float * float

  let vec_of_floats x y : vec = (x, y)

  let vec_of_float a : vec = (a, a)

  let vec_of_ints x y : vec = (float_of_int x, float_of_int y)

  let vec_of_int a : vec = (float_of_int a, float_of_int a)

  let vec_of_int_tpl (x, y) : vec = (float_of_int x, float_of_int y)

  let zero = vec_of_float 0.

  let one = vec_of_float 1.

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

  let ( -.. ) = element_wise_op ( -. )

  let ( +.. ) = element_wise_op ( +. )

  let ( *.. ) = element_wise_op ( *. )

  let scale s = ( *.. ) (vec_of_float s)

  let scale_x s = ( *.. ) (vec_of_floats s 1.)

  let scale_y s = ( *.. ) (vec_of_floats 1. s)

  (** [rotate a v] is v rotated counterclockwise around the origin by
      [a] radians *)
  let rotate angle ((x, y) : vec) =
    let cos_a = cos angle in
    let sin_a = sin angle in
    let new_x = (x *. cos_a) -. (y *. sin_a) in
    let new_y = (x *. sin_a) +. (y *. cos_a) in
    (new_x, new_y)
end

open Vec2

let sqrt_3 = sqrt 3.

let hex_size = 110.

let hex_spacing = vec_of_floats (sqrt_3 *. hex_size) (1.5 *. hex_size)

let board_size =
  vec_of_floats (5. *. x_of hex_spacing) (31. /. 4. *. hex_size)

let screen_size = vec_of_ints 1220 1080

let board_pos =
  screen_size -.. board_size
  |> scale 0.5
  |> ( +.. ) (vec_of_floats (x_of hex_spacing *. 0.5) hex_size)

let config =
  " "
  ^ (screen_size |> x_int_of |> string_of_int)
  ^ "x"
  ^ (screen_size |> y_int_of |> string_of_int)
  ^ "+700-200"

let bg_color = rgb 52 143 235

let token_color = rgb 230 230 197

let render () = Graphics.synchronize ()

let color_of_hex = function
  | Types.Desert -> rgb 212 212 133
  | Other (_, res) -> (
      match res with
      | Types.Wood -> rgb 81 125 25
      | Types.Sheep -> rgb 142 217 104
      | Types.Wheat -> rgb 240 173 0
      | Types.Brick -> rgb 156 67 0
      | Types.Ore -> rgb 123 111 131)

let draw_string_centered pos text =
  let draw_pos =
    pos -.. (text |> text_size |> vec_of_int_tpl |> scale 0.5)
  in
  moveto (x_int_of draw_pos) (y_int_of draw_pos);
  draw_string text

(** [set_font_size s] sets the size of drawn text to s. Uses a method
    described at https://discuss.ocaml.org/t/graphics-set-font-size/2752
    becuase [Graphics.set_text_size s] does not work. *)
let set_font_size size =
  "-*-fixed-medium-r-semicondensed--"
  ^ (size |> int_of_float |> string_of_int)
  ^ "-*-*-*-*-*-iso8859-1"
  |> set_font

let unit_hexagon_coords : Vec2.vec array =
  [|
    (0., 1.);
    (sqrt_3 /. 2., 0.5);
    (sqrt_3 /. 2., -0.5);
    (0., -1.);
    (sqrt_3 /. -2., -0.5);
    (sqrt_3 /. -2., 0.5);
  |]

let fill_hex pos hex =
  hex |> color_of_hex |> set_color;
  let map_coord (v : vec) = scale hex_size v +.. pos |> ints_of_vec in
  let vertex_coords = unit_hexagon_coords |> Array.map map_coord in
  vertex_coords |> fill_poly;
  set_color black;
  vertex_coords |> draw_poly;
  match Types.number_of_hex hex with
  | 7 -> ()
  | n ->
      set_color token_color;
      fill_circle (x_int_of pos) (y_int_of pos)
        (int_of_float (hex_size /. 3.));
      set_color black;
      set_font_size (hex_size *. 0.4);
      n |> string_of_int |> draw_string_centered pos

let clear color =
  Graphics.set_color color;
  let w, h = Vec2.ints_of_vec (screen_size -.. vec_of_ints 1 1) in
  Graphics.fill_rect 0 0 w h

let coords_of_hex_index i =
  let v = i |> Board.hex_coords |> vec_of_int_tpl |> swap in
  let row_shift =
    if y_int_of v mod 2 == 0 then (0., 0.)
    else (x_of hex_spacing /. -2., 0.)
  in
  let x, y = (v *.. hex_spacing) +.. row_shift in
  (x, (y_of screen_size *. 0.6) -. y)

let setup_window () =
  Graphics.open_graph config;
  Graphics.set_window_title "OCaml Catan";
  Graphics.auto_synchronize false;
  clear bg_color

let add_peices (board : Board.t) =
  let redPlayer = Player.make_player Types.Red in
  let bluePlayer = Player.make_player Types.Blue in
  board
  |> Board.add_settlement redPlayer 13 3
  |> Board.add_road redPlayer 13 3
  |> Board.add_settlement redPlayer 9 1
  |> Board.add_road redPlayer 9 0
  |> Board.add_road redPlayer 9 5
  |> Board.add_settlement bluePlayer 9 4
  |> Board.add_road bluePlayer 9 3
  |> Board.add_settlement bluePlayer 7 1
  |> Board.add_road bluePlayer 7 0

(** [indicies_to_draw i] is a list of vertex/edge indicies which hex at
    index [i] is responsible for drawing. All hexes draw the three
    verticies on their top half, but the bottom 3 verticies are draw
    conditionall for certain hexes to avoid any vertex being drawn
    twice. Hexes are aslo responsible for drawing the same indexed edges
    (roads) as they are for verticies. *)
let indicies_to_draw i =
  [ 5; 0; 1 ]
  @
  match i with
  | 11 -> [ 2 ]
  | 15 -> [ 2 ]
  | 18 -> [ 2; 3 ]
  | 17 -> [ 2; 3; 4 ]
  | 16 -> [ 3; 4 ]
  | 7 -> [ 4 ]
  | 12 -> [ 4 ]
  | _ -> []

let fill_hexes (board : Board.t) =
  for i = 0 to 18 do
    let pos = board_pos +.. coords_of_hex_index i in
    let hex = Board.hex_info board i in
    fill_hex pos hex
  done

let color_of_team_color = function
  | Types.Red -> rgb 194 14 8
  | Types.Orange -> rgb 235 160 21
  | Types.Blue -> rgb 6 83 184
  | Types.White -> white

let unit_triangle_coords : Vec2.vec array =
  [|
    (0., sqrt_3 *. 2. /. 3.); (1., sqrt_3 /. -3.); (-1., sqrt_3 /. -3.);
  |]

let fill_tri pos flipped =
  let map_coord (v : vec) =
    scale (hex_size *. 0.2) v
    |> scale_y (if flipped then -1. else 1.)
    |> ( +.. ) pos |> ints_of_vec
  in
  let verts = Array.map map_coord unit_triangle_coords in
  fill_poly verts;
  set_color black;
  draw_poly verts

let fill_vertex hex_pos dir (vertex : Types.vertex) =
  let pos = scale hex_size unit_hexagon_coords.(dir) +.. hex_pos in
  match vertex with
  | Empty -> ()
  | Settlement c ->
      c |> color_of_team_color |> set_color;
      fill_tri pos (dir mod 2 = 0)
  | City c ->
      c |> color_of_team_color |> set_color;
      fill_circle (x_int_of pos) (y_int_of pos)
        (hex_size *. 0.2 |> int_of_float)

(** [y_of road_size] is the portion of the distance from one vertex to
    another which a road occupies. [x_of road_size] is the width of a
    road in the same absolute units as [y_of road_size]. *)
let road_size = vec_of_floats 0.13 0.6

let half_road_size = scale 0.5 road_size

let unit_edge_coords : Vec2.vec array =
  [|
    half_road_size;
    scale_y (-1.) half_road_size;
    scale (-1.) half_road_size;
    scale_x (-1.) half_road_size;
  |]

let edge_verts hex_pos dir =
  let edge_pos =
    unit_hexagon_coords.(dir) +.. unit_hexagon_coords.((dir + 1) mod 6)
    |> scale (0.5 *. hex_size)
    |> ( +.. ) hex_pos
  in
  let map_coord v =
    v
    |> rotate ((dir - 1 |> float_of_int) *. 3.141592 /. -3.)
    |> scale hex_size |> ( +.. ) edge_pos |> ints_of_vec
  in
  Array.map map_coord unit_edge_coords

let fill_edge hex_pos dir = function
  | Types.Empty -> ()
  | Types.Road c ->
      c |> color_of_team_color |> set_color;
      let verts = edge_verts hex_pos dir in
      fill_poly verts;
      set_color black;
      draw_poly verts

let fill_edges_and_verts (board : Board.t) =
  for i = 0 to 18 do
    let pos = board_pos +.. coords_of_hex_index i in
    let verts = Board.hex_to_vertices board i in
    let edges = Board.hex_to_edges board i in
    List.iter
      (fun dir -> fill_vertex pos dir (List.nth verts dir))
      (indicies_to_draw i);
    List.iter
      (fun dir -> fill_edge pos dir (List.nth edges dir))
      (indicies_to_draw i)
  done

let print_board (board : Board.t) =
  (* for debuggin purposes, this adds peices to the board *)
  let board = add_peices board in
  setup_window ();
  fill_hexes board;
  fill_edges_and_verts board;
  render ();
  Graphics.loop_at_exit [] ignore
