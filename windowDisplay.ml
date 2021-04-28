open Graphics

module Vec2 = struct
  type vec = float * float

  let pi = 3.141592653

  let vec_of_floats x y : vec = (x, y)

  let vec_of_float a : vec = (a, a)

  let vec_of_ints x y : vec = (float_of_int x, float_of_int y)

  let vec_of_int a : vec = (float_of_int a, float_of_int a)

  let vec_of_int_tpl (x, y) : vec = (float_of_int x, float_of_int y)

  let print_vec ((x, y) : vec) =
    print_string
      ("<" ^ string_of_float x ^ ", " ^ string_of_float y ^ ">\n")

  let x_of ((x, y) : vec) = x

  let y_of ((x, y) : vec) = y

  let x_int_of ((x, y) : vec) = int_of_float x

  let y_int_of ((x, y) : vec) = int_of_float y

  let map (f : float -> 'a) ((x, y) : vec) : 'a * 'a = (f x, f y)

  let ints_of_vec = map int_of_float

  let strings_of_vec = map string_of_float

  let int_strings_of_vec =
    map (fun fl -> fl |> int_of_float |> string_of_int)

  let element_wise_op (op : float -> float -> float) v1 v2 : vec =
    let x = op (x_of v1) (x_of v2) in
    let y = op (y_of v1) (y_of v2) in
    (x, y)

  let ( -.. ) = element_wise_op ( -. )

  let ( +.. ) = element_wise_op ( +. )

  let ( *.. ) = element_wise_op ( *. )

  let scale s = map (( *. ) s)

  let scale_x s = ( *.. ) (vec_of_floats s 1.)

  let scale_y s = ( *.. ) (vec_of_floats 1. s)

  let swap ((x, y) : vec) = vec_of_floats y x

  (** [rotate a v] is v rotated counterclockwise around the origin by
      [a] radians *)
  let rotate angle ((x, y) : vec) =
    let cos_a = cos angle in
    let sin_a = sin angle in
    let new_x = (x *. cos_a) -. (y *. sin_a) in
    let new_y = (x *. sin_a) +. (y *. cos_a) in
    (new_x, new_y)

  (** [rotate_degrees a v] is v rotated counterclockwise around the
      origin by [a] degrees *)
  let rotate_degrees angle = rotate (angle /. 180. *. pi)

  let rotate_dir dir = rotate ((dir - 1 |> float_of_int) *. pi /. -3.)

  let distance ((x1, y1) : vec) ((x2, y2) : vec) =
    let xd = x1 -. x2 in
    let yd = y1 -. y2 in
    sqrt ((xd *. xd) +. (yd *. yd))

  let cube_round ((x, y) : vec) =
    let z = -.x -. y in
    let rx = x |> Float.round in
    let ry = y |> Float.round in
    let rz = z |> Float.round in

    let x_diff = Float.abs (rx -. x) in
    let y_diff = Float.abs (ry -. y) in
    let z_diff = Float.abs (rz -. z) in
    let rx =
      if x_diff > y_diff && x_diff > z_diff then -.ry -. rz else rx
    in
    let ry = if y_diff > z_diff then -.rx -. rz else ry in
    (rx, ry)
end

open Vec2

let sqrt_3 = sqrt 3.

let screen_size = vec_of_ints 1620 1080

let hex_size = 110.

let hex_spacing = vec_of_floats (sqrt_3 *. hex_size) (1.5 *. hex_size)

let board_size =
  vec_of_floats (5. *. sqrt_3 *. hex_size) (31. /. 4. *. hex_size)

let board_pos =
  let offset = vec_of_floats (sqrt_3 *. hex_size) (2. *. hex_size) in
  screen_size +.. offset -.. board_size |> scale 0.5

let color_of_resource = function
  | Types.Wood -> rgb 81 125 25
  | Types.Sheep -> rgb 142 217 104
  | Types.Wheat -> rgb 240 173 0
  | Types.Brick -> rgb 156 67 0
  | Types.Ore -> rgb 123 111 131

let color_of_hex = function
  | Types.Desert -> rgb 212 212 133
  | Other (_, r) -> color_of_resource r

let color_of_port = function
  | Types.ThreeToOne -> white
  | Types.TwoToOne r -> color_of_resource r

let draw_string_centered pos text =
  let x, y =
    text |> text_size |> vec_of_int_tpl |> scale 0.5 |> ( -.. ) pos
    |> ints_of_vec
  in
  moveto x y;
  draw_string text

(** [set_font_size s] sets the size of drawn text to s. Uses a method
    referenced at
    https://discuss.ocaml.org/t/graphics-set-font-size/2752 becuase
    [Graphics.set_text_size s] does not work. *)
let set_font_size size =
  "-*-fixed-medium-r-semicondensed--"
  ^ (size |> int_of_float |> string_of_int)
  ^ "-*-*-*-*-*-iso8859-1"
  |> set_font

(** [indicies_to_draw i] is a list of vertex/edge indicies which the hex
    at index [i] is responsible for drawing. All hexes draw the three
    verticies on their top half, but the bottom 3 verticies are draw
    conditionally for certain hexes to avoid any vertex being drawn
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

let unit_hexagon_coords : Vec2.vec array =
  [|
    (0., 1.);
    (sqrt_3 /. 2., 0.5);
    (sqrt_3 /. 2., -0.5);
    (0., -1.);
    (sqrt_3 /. -2., -0.5);
    (sqrt_3 /. -2., 0.5);
  |]

let coords_of_hex_index i =
  let v = i |> Board.hex_coords |> vec_of_int_tpl |> swap in
  let row_shift =
    if y_int_of v mod 2 == 0 then (0., 0.)
    else (x_of hex_spacing /. -2., 0.)
  in
  let x, y = (v *.. hex_spacing) +.. row_shift in
  (x, (y_of screen_size *. 0.6) -. y)

let hex_index_of_mouse_pos pos =
  let x, y = pos -.. board_pos in
  let q = ((sqrt_3 /. 3. *. x) -. (1. /. 3. *. y)) /. hex_size in
  let r = 2. /. 3. *. y /. hex_size in
  vec_of_floats q r

let pos_of_hex_index index = board_pos +.. coords_of_hex_index index

let outline_rect (pos : Vec2.vec) (size : Vec2.vec) =
  let x, y = ints_of_vec pos in
  let w, h = ints_of_vec size in
  fill_rect x y w h;
  set_color black;
  draw_rect x y w h

let outline_poly verts =
  fill_poly verts;
  set_color black;
  draw_poly verts

let outline_ellipse (pos : Vec2.vec) (rad : Vec2.vec) =
  let x, y = ints_of_vec pos in
  let rx, ry = ints_of_vec rad in
  fill_ellipse x y rx ry;
  set_color black;
  draw_ellipse x y rx ry

let fill_robber pos =
  let pos = pos +.. (vec_of_floats (-0.4) (-0.3) |> scale hex_size) in
  rgb 156 156 156 |> set_color;
  outline_ellipse pos (vec_of_floats 0.25 0.12 |> scale hex_size);
  rgb 156 156 156 |> set_color;
  let pos = pos +.. (vec_of_floats 0. 0.25 |> scale hex_size) in
  outline_ellipse pos (vec_of_floats 0.23 0.3 |> scale hex_size);
  rgb 156 156 156 |> set_color;
  let pos = pos +.. (vec_of_floats 0. 0.3 |> scale hex_size) in
  outline_ellipse pos (vec_of_floats 0.21 0.21 |> scale hex_size)

let fill_token pos hex =
  match Types.number_of_hex hex with
  | 7 -> ()
  | n ->
      set_color (rgb 230 230 197);
      fill_circle (x_int_of pos) (y_int_of pos)
        (int_of_float (hex_size /. 3.));
      set_color black;
      set_font_size (hex_size *. 0.4);
      n |> string_of_int |> draw_string_centered pos

let fill_hex pos hex =
  hex |> color_of_hex |> set_color;
  let map_coord (v : vec) = scale hex_size v +.. pos |> ints_of_vec in
  let verts = unit_hexagon_coords |> Array.map map_coord in
  outline_poly verts;
  fill_token pos hex

let fill_hexes (board : Board.t) =
  for i = 0 to 18 do
    let pos = pos_of_hex_index i in
    let hex = Board.hex_info board i in
    fill_hex pos hex
  done

(** maps team colors to their rgb equivalents *)
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
  outline_poly verts

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

(** [y_of road_size] is one half the portion of the distance from one
    vertex to another which a road occupies. [x_of road_size] is one
    half the width of a road in the same absolute units as
    [y_of road_size]. *)
let road_size = vec_of_floats 0.065 0.3

let unit_road_coords : Vec2.vec array =
  [|
    road_size;
    scale_y (-1.) road_size;
    scale (-1.) road_size;
    scale_x (-1.) road_size;
  |]

let edge_center_coord dir =
  unit_hexagon_coords.(dir) +.. unit_hexagon_coords.((dir + 1) mod 6)
  |> scale 0.5

let road_verts hex_pos dir =
  let map_coord v =
    v |> rotate_dir dir
    |> ( +.. ) (edge_center_coord dir)
    |> scale hex_size |> ( +.. ) hex_pos |> ints_of_vec
  in
  Array.map map_coord unit_road_coords

let fill_edge hex_pos dir = function
  | Types.Empty -> ()
  | Types.Road c ->
      c |> color_of_team_color |> set_color;
      let verts = road_verts hex_pos dir in
      outline_poly verts

let port_gap = 0.15

let port_width = 0.6

let port_height = port_width *. sqrt_3 /. 2.

let fill_port hex_pos dir (port : Types.port) =
  port |> color_of_port |> set_color;
  let j = Vec2.vec_of_floats 0. 1. |> Vec2.rotate_degrees (-30.) in
  let i = j |> Vec2.rotate_degrees (-90.) in
  let map_coord vert =
    vert
    |> ( +.. ) (scale port_gap j)
    |> ( +.. ) (edge_center_coord 0)
    |> rotate_degrees ((dir |> float_of_int) *. -60.)
    |> scale hex_size |> ( +.. ) hex_pos |> ints_of_vec
  in
  let verts =
    Array.map map_coord
      [|
        scale (port_width /. 2.) i;
        scale (port_width /. -2.) i;
        scale port_height j;
      |]
  in
  outline_poly verts

let fill_edges_and_verts (board : Board.t) =
  for i = 0 to 18 do
    let pos = pos_of_hex_index i in
    let verts = Board.hex_to_vertices board i in
    let edges = Board.hex_to_edges board i in
    List.iter
      (fun dir -> fill_vertex pos dir (List.nth verts dir))
      (indicies_to_draw i);
    List.iter
      (fun dir -> fill_edge pos dir (List.nth edges dir))
      (indicies_to_draw i)
  done

let clear color =
  Graphics.set_color color;
  let w, h = Vec2.ints_of_vec (screen_size -.. vec_of_ints 1 1) in
  Graphics.fill_rect 0 0 w h

let render () = Graphics.synchronize ()

let rec wait_click_end start_pos : vec option =
  let { mouse_x; mouse_y; button } =
    wait_next_event [ Button_up; Mouse_motion ]
  in
  let pos = vec_of_ints mouse_x mouse_y in
  if distance start_pos pos > 10. then None
  else if button_down () = false then Some pos
  else wait_click_end start_pos

let wait_click_start () : vec option =
  let { mouse_x; mouse_y; button } = wait_next_event [ Button_down ] in
  (mouse_x, mouse_y) |> vec_of_int_tpl |> wait_click_end

let rec wait_next_click () =
  match wait_click_start () with
  | None -> wait_next_click ()
  | Some c -> c

let player_info_size = vec_of_ints 330 90

let ui_bg_color = rgb 235 235 185

let draw_player_info player index =
  let x = screen_size -.. player_info_size |> x_int_of in
  let y = index * y_int_of player_info_size in
  let pos = vec_of_ints x y  in
  let (w, h) = ints_of_vec player_info_size in
  set_color ui_bg_color;
  outline_rect pos player_info_size;
  let padding = 10 in
  let square_size = h - (2 * padding) in
  Player.get_color player |> color_of_team_color |> set_color;
  fill_rect (x + padding) (y + padding) square_size square_size;
  let x, y = (x + h, y + (h / 2)) in
  let pos = vec_of_ints x y in
  let card_size = vec_of_int ((h / 2) - padding) |> scale_x 0.5 in
  rgb 108 154 230 |> set_color;
  outline_rect pos card_size;
  draw_string_centered (pos +.. scale 0.5 card_size) "?";
  draw_string_centered
    (pos +.. (card_size *.. vec_of_floats 0.5 (-0.5)))
    (Player.num_resources player |> string_of_int);
  let x = x + x_int_of card_size + padding in
  let pos = vec_of_ints x y in
  rgb 170 111 214 |> set_color;
  outline_rect pos card_size;
  draw_string_centered (pos +.. (scale 0.5 card_size)) "D";
  draw_string_centered (pos +.. (card_size *.. (vec_of_floats 0.5 (-0.5)))) (Player.num_devs player |> string_of_int)


let add_peices (board : Board.t) =
  fill_port (pos_of_hex_index 0) 4 Types.ThreeToOne;
  fill_port (pos_of_hex_index 1) 5 (Types.TwoToOne Wood);
  fill_port (pos_of_hex_index 2) 0 (Types.TwoToOne Ore);
  let rd = Player.make_player Types.Red in
  let bl = Player.make_player Types.Blue in
  draw_player_info rd 0;
  draw_player_info bl 1;
  board
  |> Board.add_settlement rd 13 3
  |> Board.add_road rd 13 3
  |> Board.add_settlement rd 9 1
  |> Board.add_road rd 9 0 |> Board.add_road rd 9 5
  |> Board.add_settlement bl 9 4
  |> Board.add_road bl 9 3
  |> Board.add_settlement bl 7 1
  |> Board.add_road bl 7 0

let rec print_clicks () =
  let x, y =
    wait_next_click () |> hex_index_of_mouse_pos |> int_strings_of_vec
  in
  print_endline (x ^ ", " ^ y);
  print_clicks ()



let print_board (board : Board.t) =
  let x, y = int_strings_of_vec screen_size in
  Graphics.open_graph (" " ^ x ^ "x" ^ y ^ "+700-200");
  Graphics.set_window_title "OCaml Catan";
  Graphics.auto_synchronize false;
  clear (rgb 52 143 235);
  (* for debuggin purposes, this adds peices to the board *)
  (* let board = add_peices board in *)
  fill_hexes board;
  fill_edges_and_verts board;
  (* let res = 2 in for x = 0 to x_int_of screen_size / res do for y = 0
     to y_int_of screen_size / res do let pixel_pos = vec_of_ints (res *
     x) (res * y) in let unrounded = pixel_pos |> hex_index_of_mouse_pos
     in let rounded = map Float.round unrounded in let dist = abs_float
     (distance unrounded rounded) in set_color (rgb 100 100 (255. *.
     dist |> int_of_float)); fill_rect (x_int_of pixel_pos) (y_int_of
     pixel_pos) res res done done; *)
  (* fill_robber (board |> Board.get_robber |> pos_of_hex_index); *)
  render ();
  (* Graphics.loop_at_exit [] ignore *)
  print_clicks () |> ignore
