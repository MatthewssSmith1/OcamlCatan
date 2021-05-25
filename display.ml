open Graphics

module Vec2 = struct
  type t = float * float

  let pi = 3.141592653

  let zero : t = (0., 0.)

  let one : t = (1., 1.)

  let neg_one : t = (-1., -1.)

  let unit_i : t = (1., 0.)

  let unit_j : t = (0., 1.)

  let vec_of x y : t = (x, y)

  let vec_of_float a : t = (a, a)

  let vec_of_ints x y : t = (float_of_int x, float_of_int y)

  let vec_of_int a : t = (float_of_int a, float_of_int a)

  let vec_of_int_tpl (x, y) : t = (float_of_int x, float_of_int y)

  let print_vec ((x, y) : t) =
    print_string
      ("<" ^ string_of_float x ^ ", " ^ string_of_float y ^ ">\n")

  let x_of ((x, y) : t) = x

  let y_of ((x, y) : t) = y

  let x_int_of ((x, y) : t) = int_of_float x

  let y_int_of ((x, y) : t) = int_of_float y

  let map (f : float -> 'a) ((x, y) : t) : 'a * 'a = (f x, f y)

  let ints_of_vec = map int_of_float

  let strings_of_vec = map string_of_float

  let int_strings_of_vec =
    map (fun fl -> fl |> int_of_float |> string_of_int)

  let rounded_string_of_vec vec =
    let x, y = int_strings_of_vec vec in
    "<" ^ x ^ ", " ^ y ^ ">"

  let element_wise_op (op : float -> float -> float) v1 v2 : t =
    let x = op (x_of v1) (x_of v2) in
    let y = op (y_of v1) (y_of v2) in
    (x, y)

  let ( -.. ) = element_wise_op ( -. )

  let ( +.. ) = element_wise_op ( +. )

  let ( *.. ) = element_wise_op ( *. )

  let scale s = map (( *. ) s)

  let scale_int s = scale (float_of_int s)

  let scale_x s = ( *.. ) (vec_of s 1.)

  let scale_y s = ( *.. ) (vec_of 1. s)

  let scale_xy xs ys vec = vec |> scale_x xs |> scale_y ys

  let swap ((x, y) : t) = vec_of y x

  let add_to_x dx ((x, y) : t) : t = (x +. dx, y)

  let add_to_y dy ((x, y) : t) : t = (x, y +. dy)

  let add_xy x y vec = vec +.. vec_of x y

  (** [rotate a v] is v rotated counterclockwise around the origin by
      [a] radians *)
  let rotate angle ((x, y) : t) =
    let cos_a = cos angle in
    let sin_a = sin angle in
    let new_x = (x *. cos_a) -. (y *. sin_a) in
    let new_y = (x *. sin_a) +. (y *. cos_a) in
    (new_x, new_y)

  (** [rotate_degrees a v] is v rotated counterclockwise around the
      origin by [a] degrees *)
  let rotate_degrees angle = rotate (angle /. 180. *. pi)

  let rotate_dir dir = rotate ((dir - 1 |> float_of_int) *. pi /. -3.)

  let distance ((x1, y1) : t) ((x2, y2) : t) =
    let xd = x1 -. x2 in
    let yd = y1 -. y2 in
    sqrt ((xd *. xd) +. (yd *. yd))

  let magnitude = distance zero

  let cube_round ((x, y) : t) =
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

let window_size = vec_of_ints 1600 900

let window_pos = scale 0.5 (vec_of_ints 1920 1080 -.. window_size)

let hex_size = 90.

let inv_hex_size = 1. /. hex_size

let line_width = 3

let hex_spacing = vec_of sqrt_3 1.5 |> scale hex_size

let board_size = vec_of (5. *. sqrt_3) 8. |> scale hex_size

let draw_string_centered text pos =
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
  try
    "-*-fixed-medium-r-semicondensed--" ^ (size |> string_of_int)
    ^ "-*-*-*-*-*-iso8859-1"
    |> set_font
  with _ -> ()

let set_font_size_float size = size |> int_of_float |> set_font_size

let set_color_rgb r g b = rgb r g b |> set_color

let fill_rect size pos =
  let x, y = ints_of_vec pos in
  let w, h = ints_of_vec size in
  Graphics.fill_rect x y w h

let draw_rect size pos =
  let x, y = ints_of_vec pos in
  let w, h = ints_of_vec size in
  Graphics.draw_rect x y w h

let outline_rect size pos =
  fill_rect size pos;
  set_color black;
  draw_rect size pos

let outline_centered_rect size pos =
  outline_rect size (pos -.. scale 0.5 size)

let outline_poly verts =
  fill_poly verts;
  set_color black;
  draw_poly verts

let unit_star_verts =
  let make_vert i _ =
    Vec2.unit_j
    |> rotate_degrees (36. *. float_of_int i)
    |> scale (float_of_int ((i + 1) mod 2) +. 0.7)
  in
  Array.make 10 () |> Array.mapi make_vert

let outline_star radius pos =
  let map_coord vert =
    vert |> scale radius |> ( +.. ) pos |> ints_of_vec
  in
  unit_star_verts |> Array.map map_coord |> outline_poly

let outline_ellipse rad pos =
  let x, y = ints_of_vec pos in
  let rx, ry = ints_of_vec rad in
  fill_ellipse x y rx ry;
  set_color black;
  draw_ellipse x y rx ry

let outline_circle rad = outline_ellipse (vec_of_float rad)

let set_color_resource = function
  | Types.Wood -> set_color_rgb 81 125 25
  | Types.Sheep -> set_color_rgb 142 217 104
  | Types.Wheat -> set_color_rgb 237 219 24
  | Types.Brick -> set_color_rgb 156 67 0
  | Types.Ore -> set_color_rgb 123 111 131

let set_color_hex = function
  | Types.Desert -> set_color_rgb 212 212 133
  | Other (_, r) -> set_color_resource r

let set_color_port = function
  | Types.ThreeToOne -> set_color white
  | Types.TwoToOne r -> set_color_resource r

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

let unit_hexagon_coords : Vec2.t array =
  [|
    (0., 1.);
    (sqrt_3 /. 2., 0.5);
    (sqrt_3 /. 2., -0.5);
    (0., -1.);
    (sqrt_3 /. -2., -0.5);
    (sqrt_3 /. -2., 0.5);
  |]

let unit_edge_coords : Vec2.t array =
  let edge_center_coord dir () =
    unit_hexagon_coords.(dir) +.. unit_hexagon_coords.((dir + 1) mod 6)
    |> scale 0.5
  in
  Array.make 6 () |> Array.mapi edge_center_coord

let cube_to_axial ((x, _, z) : float * float * float) = (x, z)

let axial_to_cube (q, r) = (q, -.q -. r, r)

let cube_round (x, y, z) =
  let rx = Float.round x in
  let ry = Float.round y in
  let rz = Float.round z in

  let x_diff = Float.abs (rx -. x) in
  let y_diff = Float.abs (ry -. y) in
  let z_diff = Float.abs (rz -. z) in

  if x_diff > y_diff && x_diff > z_diff then (0. -. ry -. rz, ry, rz)
  else if y_diff > z_diff then (rx, 0. -. rx -. rz, rz)
  else (rx, ry, 0. -. rx -. ry)

let hex_round (hex_pos : float * float) =
  hex_pos |> axial_to_cube |> cube_round |> cube_to_axial

let hex_index_of_axial = function
  | -2, 2 -> Some 0
  | -1, 2 -> Some 1
  | 0, 2 -> Some 2
  | -2, 1 -> Some 3
  | -1, 1 -> Some 4
  | 0, 1 -> Some 5
  | 1, 1 -> Some 6
  | -2, 0 -> Some 7
  | -1, 0 -> Some 8
  | 0, 0 -> Some 9
  | 1, 0 -> Some 10
  | 2, 0 -> Some 11
  | -1, -1 -> Some 12
  | 0, -1 -> Some 13
  | 1, -1 -> Some 14
  | 2, -1 -> Some 15
  | 0, -2 -> Some 16
  | 1, -2 -> Some 17
  | 2, -2 -> Some 18
  | _ -> None

let hex_index_of_pixel_pos pos =
  let x, y = pos -.. scale 0.5 window_size in
  let q = ((sqrt_3 /. 3. *. x) -. (1. /. 3. *. y)) /. hex_size in
  let r = 2. /. 3. *. y /. hex_size in
  (q, r) |> hex_round |> ints_of_vec |> hex_index_of_axial

let pos_of_hex_index index =
  let v =
    index |> Board.hex_coords |> vec_of_int_tpl |> swap |> scale_y (-1.)
    |> add_xy (-2.) 2.
  in
  let row_shift =
    if y_int_of v mod 2 == 0 then (0., 0.)
    else (x_of hex_spacing /. -2., 0.)
  in
  (v *.. hex_spacing) +.. row_shift +.. scale 0.5 window_size

let port_gap = 0.15

let port_width = 0.6

let port_height = port_width *. sqrt_3 /. 2.

let fill_port dir hex_pos (port : Types.port) =
  set_color_port port;
  let j = Vec2.unit_j |> Vec2.rotate_degrees (-30.) in
  let i = j |> Vec2.rotate_degrees (-90.) in
  let map_coord vert =
    vert
    |> ( +.. ) (scale port_gap j)
    |> ( +.. ) unit_edge_coords.(0)
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

let fill_robber pos =
  let pos = pos +.. (vec_of (-0.4) (-0.3) |> scale hex_size) in
  (* let pos = pos in *)
  set_color_rgb 156 156 156;
  outline_ellipse (vec_of 0.25 0.12 |> scale hex_size) pos;
  set_color_rgb 156 156 156;
  let pos = pos +.. vec_of 0. (0.25 *. hex_size) in
  outline_ellipse (vec_of 0.23 0.3 |> scale hex_size) pos;
  set_color_rgb 156 156 156;
  let pos = pos +.. (vec_of 0. 0.3 |> scale hex_size) in
  outline_ellipse (vec_of 0.21 0.21 |> scale hex_size) pos

let fill_token hex pos =
  match Types.number_of_hex hex with
  | 7 -> ()
  | n ->
      set_color_rgb 230 230 197;
      outline_circle (hex_size *. 0.3) pos;
      set_color black;
      set_font_size_float (hex_size *. 0.4);
      draw_string_centered (string_of_int n) pos

let fill_hex hex pos =
  set_color_hex hex;
  let map_coord v = scale hex_size v +.. pos |> ints_of_vec in
  let verts = unit_hexagon_coords |> Array.map map_coord in
  outline_poly verts;
  fill_token hex pos

let fill_hexes (board : Board.t) =
  for i = 0 to 18 do
    let pos = pos_of_hex_index i in
    let hex = Board.hex_info board i in
    fill_hex hex pos;
    for j = 0 to 5 do
      match Board.get_port i j board with
      | Some port -> (
          match Board.get_port i ((j + 1) mod 6) board with
          | Some port -> fill_port j pos port
          | _ -> ())
      | _ -> ()
    done
  done

(** maps team colors to their rgb equivalents *)
let color_of_team_color = function
  | Types.Red -> rgb 194 14 8
  | Types.Orange -> rgb 235 160 21
  | Types.Blue -> rgb 20 102 179
  | Types.White -> white

let set_color_team team = team |> color_of_team_color |> set_color

let unit_triangle_coords : Vec2.t array =
  [|
    (0., sqrt_3 *. 2. /. 3.); (1., sqrt_3 /. -3.); (-1., sqrt_3 /. -3.);
  |]

let outline_tri size flipped pos =
  let map_coord v =
    scale size v
    |> scale_y (if flipped then -1. else 1.)
    |> ( +.. ) pos |> ints_of_vec
  in
  let verts = Array.map map_coord unit_triangle_coords in
  outline_poly verts

let fill_vertex (vertex : Types.vertex) dir hex_pos =
  let pos = scale hex_size unit_hexagon_coords.(dir) +.. hex_pos in
  match vertex with
  | Empty -> ()
  | Settlement c ->
      set_color_team c;
      outline_tri (hex_size *. 0.2) (dir mod 2 = 0) pos
  | City c ->
      set_color_team c;
      outline_circle (hex_size *. 0.2) pos

(** [y_of road_size] is one half the portion of the distance from one
    vertex to another which a road occupies. [x_of road_size] is one
    half the width of a road in the same absolute units as
    [y_of road_size] (the distance from one vert to an adjascent one is
    [hex_size]). *)
let road_size = vec_of 0.065 0.3

(** [hex_to_road_dist] is the distance from the center of a hex to the
    closest point on a road adjascent to it *)
let hex_to_road_dist = (sqrt_3 /. 2.) -. (x_of road_size *. 2.)

let unit_road_coords : Vec2.t array =
  [|
    road_size;
    scale_y (-1.) road_size;
    scale (-1.) road_size;
    scale_x (-1.) road_size;
  |]

let road_verts dir hex_pos =
  let map_coord v =
    v |> rotate_dir dir
    |> ( +.. ) unit_edge_coords.(dir)
    |> scale hex_size |> ( +.. ) hex_pos |> ints_of_vec
  in
  Array.map map_coord unit_road_coords

let fill_edge dir hex_pos = function
  | Types.Empty -> ()
  | Types.Road c ->
      set_color_team c;
      let verts = road_verts dir hex_pos in
      outline_poly verts

let fill_edges_and_verts (board : Board.t) =
  for i = 0 to 18 do
    let pos = pos_of_hex_index i in
    let verts = Board.hex_to_vertices board i in
    let edges = Board.hex_to_edges board i in
    List.iter
      (fun dir -> fill_vertex (List.nth verts dir) dir pos)
      (indicies_to_draw i);
    List.iter
      (fun dir -> fill_edge dir pos (List.nth edges dir))
      (indicies_to_draw i)
  done

let clear color =
  Graphics.set_color color;
  fill_rect window_size zero

let render () = Graphics.synchronize ()

let rec wait_click_end start_pos : Vec2.t option =
  let { mouse_x; mouse_y; button } =
    wait_next_event [ Button_up; Mouse_motion ]
  in
  let pos = vec_of_ints mouse_x mouse_y in
  if distance start_pos pos > 10. then None
  else if button_down () = false then Some pos
  else wait_click_end start_pos

let wait_click_start () : Vec2.t option =
  let { mouse_x; mouse_y; button } = wait_next_event [ Button_down ] in
  (mouse_x, mouse_y) |> vec_of_int_tpl |> wait_click_end

let rec wait_next_click () =
  match wait_click_start () with
  | None -> wait_next_click ()
  | Some c -> c

let player_info_size = vec_of_ints 300 130

let ui_bg_color = rgb 235 235 185

let res_card_color = rgb 108 154 230

let dev_card_color = rgb 170 111 214

let draw_card_indicator card_color card_char num size pos =
  set_color card_color;
  outline_rect size pos;
  let card_back_font_size = 35 in
  let number_font_size = 45 in
  set_font_size card_back_font_size;
  draw_string_centered card_char (pos +.. scale 0.5 size);
  set_font_size number_font_size;
  let num_pos = size |> scale_xy 0.5 (-0.5) |> ( +.. ) pos in
  draw_string_centered (string_of_int num) num_pos

let draw_stat
    size
    player_color
    pos
    offset
    num
    (draw_icon : Vec2.t -> unit) =
  let pos = pos +.. offset in
  set_color player_color;
  let icon_pos = size |> scale_xy 0.25 0.5 |> ( +.. ) pos in
  draw_icon icon_pos;
  let num_pos = size |> scale_xy 0.5 0. |> ( +.. ) icon_pos in
  draw_string_centered (string_of_int num) num_pos

let draw_player_info player index =
  let player_color = Player.get_color player |> color_of_team_color in
  let padding = 12 in
  let x = window_size -.. player_info_size |> x_int_of in
  let y = index * (y_int_of player_info_size + padding) in
  let pos = vec_of_ints x y in
  let w, h = ints_of_vec player_info_size in
  (* draw background rect *)
  set_color ui_bg_color;
  fill_rect player_info_size pos;
  (* draw rect on left to show which team's color *)
  set_color player_color;
  fill_rect (vec_of 0.2 1. |> scale_int h) pos;
  (* outline the ui rect *)
  set_color black;
  draw_rect player_info_size pos;
  (* move draw pos to draw indicators *)
  let x, y = (x + padding + (h / 5), y + (h / 2)) in
  let pos = vec_of_ints x y in
  let card_size = vec_of_int ((h / 2) - padding) |> scale_x 0.65 in
  (* draw resource card indicator *)
  let num_res = Player.num_resources player in
  draw_card_indicator res_card_color "?" num_res card_size pos;
  (* move draw pos for next indicator *)
  let x = x + x_int_of card_size + padding in
  let pos = vec_of_ints x y in
  (* draw dev card indicator *)
  let num_devs = Player.num_devs player in
  draw_card_indicator dev_card_color "D" num_devs card_size pos;
  (* move draw pos for stat indicators *)
  let x = x + x_int_of card_size + padding in
  let y = y - (h / 2) + (padding / 2) in
  let pos = vec_of_ints x y in
  (* determine the size of and offset between the stat indicators which
     display victory points and the remaining number of settlements,
     cities, and roads *)
  let stat_w = (x_int_of window_size - x - (padding * 2)) / 2 in
  let stat_h = (h - (padding * 3)) / 2 in
  let stat_size = vec_of_ints stat_w stat_h in
  let stat_offset = stat_size +.. vec_of_int padding in
  let roads, stlmnts, cities = Player.pieces_left player in
  let draw_stat' = draw_stat stat_size player_color pos in
  (* settlement *)
  draw_stat' zero stlmnts (outline_tri (float_of_int h *. 0.1) false);
  (* city *)
  draw_stat'
    (scale_y 0. stat_offset)
    cities
    (outline_circle (float_of_int h *. 0.1));
  (* road *)
  draw_stat' stat_offset roads
    (outline_centered_rect (vec_of 0.07 0.3 |> scale_int h));
  (* victory points *)
  draw_stat stat_size yellow pos
    (scale_x 0. stat_offset)
    (Player.victory_points player)
    (outline_star (float_of_int h *. 0.1));
  ()

let draw_players_ui players =
  let rec helper index = function
    | [] -> ()
    | hd :: tl ->
        draw_player_info hd index;
        helper (index + 1) tl
  in
  helper 0 players

let card_size = vec_of 0.65 1. |> scale 80.

(* distance (pixels) between cards *)
let res_card_gap = x_of card_size /. 3.

(* distance (pixels) between groups of cards (different resource types) *)
let res_group_gap = vec_of (x_of card_size *. 0.8) 0.

let dev_card_gap = scale_xy 1.1 0. card_size

let string_of_dev = function
  | Types.Knight -> "K"
  | Types.RoadBuilding -> "RB"
  | Types.YearOfPlenty -> "YP"
  | Types.Monopoly -> "M"
  | Types.VictoryPoint -> "VP"

let rec draw_devs pos = function
  | [] -> ()
  | (dev, count) :: tl when count <> 0 ->
      for i = 0 to count - 1 do
        set_color dev_card_color;
        let draw_pos = pos +.. scale_int i dev_card_gap in
        outline_rect card_size draw_pos;
        draw_string_centered (string_of_dev dev)
          (draw_pos +.. scale 0.5 card_size)
      done;
      draw_devs (pos +.. scale_int count dev_card_gap) tl
  | _ :: tl -> draw_devs pos tl

let rec draw_res_and_dev pos devs = function
  | [] -> draw_devs (add_to_x (x_of res_group_gap) pos) devs
  | (res, count) :: tl when count <> 0 ->
      for i = 0 to count - 1 do
        set_color_resource res;
        outline_rect card_size
          (pos |> add_to_x (float_of_int i *. res_card_gap))
      done;
      let dx = count - 1 |> float_of_int |> ( *. ) res_card_gap in
      draw_res_and_dev (add_to_x dx pos +.. res_group_gap) devs tl
  | _ :: tl -> draw_res_and_dev pos devs tl

let draw_player_hand pos player =
  let devs = Player.devs_of player in
  let ress = Player.resources_of player in
  draw_res_and_dev pos devs ress

let add_peices (board : Board.t) =
  fill_port 4 (pos_of_hex_index 0) Types.ThreeToOne;
  fill_port 5 (pos_of_hex_index 1) (Types.TwoToOne Wood);
  fill_port 0 (pos_of_hex_index 2) (Types.TwoToOne Ore);
  let rd = Player.make_player Types.Red in
  let bl = Player.make_player Types.Blue in
  board
  |> Board.add_settlement rd 13 3
  |> Board.add_road rd 13 3
  |> Board.add_settlement rd 9 1
  |> Board.add_road rd 9 0 |> Board.add_road rd 9 5
  |> Board.add_settlement bl 9 4
  |> Board.add_road bl 9 3
  |> Board.add_settlement bl 7 1
  |> Board.add_road bl 7 0

let rec next_hex_click () =
  let mouse_pos = wait_next_click () in
  match mouse_pos |> hex_index_of_pixel_pos with
  | Some i ->
      let unit_mouse_pos =
        mouse_pos -.. pos_of_hex_index i |> scale inv_hex_size
      in
      (i, magnitude unit_mouse_pos, unit_mouse_pos)
  | None -> next_hex_click ()

let closest_coord mouse_pos coords =
  let index_with_dist i pos = (i, distance pos mouse_pos) in
  let cmp_dist (_, d1) (_, d2) = Float.compare d1 d2 in
  let sorted_verts = coords |> Array.mapi index_with_dist in
  Array.sort cmp_dist sorted_verts;
  sorted_verts.(0)

let rec next_board_click () =
  (* mouse_pos is relative to the center of the clicked hex, 1 unit is
     hex_size number of pixels (distance from center of hex to vert) *)
  let hex_index, hex_dist, mouse_pos = next_hex_click () in
  let closest = closest_coord mouse_pos in
  let vert_index, vert_dist = closest unit_hexagon_coords in
  let edge_index, edge_dist = closest unit_edge_coords in
  print_endline (string_of_float hex_dist);
  print_endline (string_of_float hex_to_road_dist);
  if vert_dist < 0.18 then Types.CVert (hex_index, vert_index)
  else if hex_dist > hex_to_road_dist then
    Types.CEdge (hex_index, edge_index)
  else Types.CHex hex_index

let rec next_hex_click () =
  match next_board_click () with CHex i -> i | _ -> next_hex_click ()

let rec next_edge_click () =
  match next_board_click () with
  | Types.CEdge (i, dir) -> (i, dir)
  | _ -> next_edge_click ()

let rec next_vert_click () =
  match next_board_click () with
  | Types.CVert (i, dir) -> (i, dir)
  | _ -> next_edge_click ()

let rec print_board_clicks () =
  next_board_click () |> Types.string_of_board_click |> print_endline;
  print_board_clicks ()

let is_window_open () =
  try
    fill_rect Vec2.one Vec2.neg_one;
    true
  with _ -> false

let initialize () =
  Graphics.close_graph ();
  let w, h = int_strings_of_vec window_size in
  let x, y = int_strings_of_vec window_pos in
  Graphics.open_graph (" " ^ w ^ "x" ^ h ^ "+" ^ x ^ "-" ^ y ^ "");
  Graphics.set_window_title "OCaml Catan";
  Graphics.auto_synchronize false;
  Graphics.set_line_width line_width

let print_game (game : Game_state.t) =
  let board = Game_state.game_to_board game in
  clear (rgb 52 143 235);
  (* for debuggin purposes, this adds peices to the board *)
  (* let board = add_peices board in *)
  fill_hexes board;
  fill_edges_and_verts board;
  print_endline (board |> Board.get_robber |> string_of_int);
  board |> Board.get_robber |> pos_of_hex_index |> fill_robber;
  (* vec_of 10. 10. |> fill_robber; *)
  game |> Game_state.game_to_players |> draw_players_ui;
  draw_player_hand (vec_of 20. 20.) (Game_state.current_turn game);

  (* fill_port 4 (pos_of_hex_index 0) Types.ThreeToOne; fill_port 5
     (pos_of_hex_index 1) (Types.TwoToOne Wood); fill_port 0
     (pos_of_hex_index 2) (Types.TwoToOne Ore); *)
  render ()

(* ; print_board_clicks () *)
