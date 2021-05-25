(*******************************************************************************
  TESTING PLAN: Our testing plan is divided into two parts: testing
  through OUnit and play testing of the actual game. The parts of our
  code that we tested automatically were mainly our helper functions
  that we used to implement the game logic. The Board module is a good
  example of this because most of the functions in it create simple
  state changes that are then called repeatedly from other modules.
  Other features though like making sure the game does not crash while
  issuing commands or display incorrect information graphically we found
  easier to test by repeatedly playtesting the game. We used primarily
  black box testing to develop our test cases by determining ourselves
  what the functions should return given their documentations and then
  writing test cases to check our results. We believe this combination
  of unit testing the foundational elements of the project and
  playtesting to make sure they all fit together well allows us to
  ensure the game plays reliably every time. 
  *****************************************************************************)

open OUnit2
open Game_state
open Player
open Board
open Types

(** [cmp_set_like_lists lst1 lst2] compares two lists to see whether
    they are equivalent set-like lists. That means checking two things.
    First, they must both be {i set-like}, meaning that they do not
    contain any duplicates. Second, they must contain the same elements,
    though not necessarily in the same order. *)
let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  && List.length lst2 = List.length uniq2
  && uniq1 = uniq2

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt] to
    pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"

let check_resource_test
    (name : string)
    (resource : Types.resource)
    (player : Player.t)
    (expected_output : int) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (Player.check_resource resource player)
    ~printer:string_of_int

let add_resource_list_test
    (name : string)
    (resources : Types.resource list)
    (player : Player.t)
    (resource : Types.resource)
    (expected_output : int) =
  name >:: fun _ ->
  assert_equal expected_output
    (Player.check_resource resource
       (Player.add_resource_list resources player))
    ~printer:string_of_int

let remove_resource_list_test
    (name : string)
    (resources : Types.resource list)
    (player : Player.t)
    (resource : Types.resource)
    (expected_output : int) =
  name >:: fun _ ->
  assert_equal expected_output
    (Player.check_resource resource
       (Player.remove_resource_list resources player))
    ~printer:string_of_int

let build_road_test
    (name : string)
    (player : Player.t)
    (free : bool)
    (resource : Types.resource)
    (expected_output : int) =
  name >:: fun _ ->
  assert_equal expected_output
    (Player.check_resource resource (Player.place_road player free))
    ~printer:string_of_int

let build_settlement_test
    (name : string)
    (player : Player.t)
    (free : bool)
    (resource : Types.resource)
    (expected_output : int) =
  name >:: fun _ ->
  assert_equal expected_output
    (Player.check_resource resource
       (Player.place_settlement player free))
    ~printer:string_of_int

let build_city_test
    (name : string)
    (player : Player.t)
    (free : bool)
    (resource : Types.resource)
    (expected_output : int) =
  name >:: fun _ ->
  assert_equal expected_output
    (Player.check_resource resource (Player.place_city player free))
    ~printer:string_of_int

let buy_dev_test
    (name : string)
    (player : Player.t)
    (resource : Types.resource)
    (expected_output : int) =
  name >:: fun _ ->
  assert_equal expected_output
    (Player.check_resource resource (Player.buy_dev player Knight))
    ~printer:string_of_int

let two_each =
  add_resource_list
    [ Wood; Wood; Sheep; Sheep; Wheat; Wheat; Brick; Brick; Ore; Ore ]
    (make_player Types.Red)

let three_each =
  add_resource_list [ Wood; Sheep; Wheat; Brick; Ore ] two_each

let player_tests =
  [
    check_resource_test "Check Wood" Types.Wood
      (Player.add_resource Types.Wood 5 (make_player Types.Red))
      5;
    check_resource_test "Check Sheep" Types.Sheep
      (Player.add_resource Types.Sheep 5 (make_player Types.Red))
      5;
    check_resource_test "Check Wheat" Types.Wheat
      (Player.add_resource Types.Wheat 5 (make_player Types.Red))
      5;
    check_resource_test "Check Brick" Types.Brick
      (Player.add_resource Types.Brick 5 (make_player Types.Red))
      5;
    check_resource_test "Check Ore" Types.Ore
      (Player.add_resource Types.Ore 5 (make_player Types.Red))
      5;
    add_resource_list_test "1 Each Wood"
      [ Wood; Sheep; Wheat; Brick; Ore ]
      (make_player Types.Red) Wood 1;
    add_resource_list_test "1 Each Sheep"
      [ Wood; Sheep; Wheat; Brick; Ore ]
      (make_player Types.Red) Sheep 1;
    add_resource_list_test "1 Each Wheat"
      [ Wood; Sheep; Wheat; Brick; Ore ]
      (make_player Types.Red) Wheat 1;
    add_resource_list_test "1 Each Brick"
      [ Wood; Sheep; Wheat; Brick; Ore ]
      (make_player Types.Red) Brick 1;
    add_resource_list_test "1 Each Ore"
      [ Wood; Sheep; Wheat; Brick; Ore ]
      (make_player Types.Red) Ore 1;
    remove_resource_list_test "Remove 1 Each Wood"
      [ Wood; Sheep; Wheat; Brick; Ore ]
      (add_resource_list
         [ Wood; Sheep; Wheat; Brick; Ore ]
         (make_player Types.Red))
      Wood 0;
    remove_resource_list_test "Remove 1 Each Sheep"
      [ Wood; Sheep; Wheat; Brick; Ore ]
      (add_resource_list
         [ Wood; Sheep; Wheat; Brick; Ore ]
         (make_player Types.Red))
      Sheep 0;
    remove_resource_list_test "Remove 1 Each Wheat"
      [ Wood; Sheep; Wheat; Brick; Ore ]
      (add_resource_list
         [ Wood; Sheep; Wheat; Brick; Ore ]
         (make_player Types.Red))
      Wheat 0;
    remove_resource_list_test "Remove 1 Each Brick"
      [ Wood; Sheep; Wheat; Brick; Ore ]
      (add_resource_list
         [ Wood; Sheep; Wheat; Brick; Ore ]
         (make_player Types.Red))
      Brick 0;
    remove_resource_list_test "Remove 1 Each Ore"
      [ Wood; Sheep; Wheat; Brick; Ore ]
      (add_resource_list
         [ Wood; Sheep; Wheat; Brick; Ore ]
         (make_player Types.Red))
      Ore 0;
    build_road_test "Not Free Wood" two_each false Wood 1;
    build_road_test "Not Free Sheep" two_each false Sheep 2;
    build_road_test "Not Free Wheat" two_each false Wheat 2;
    build_road_test "Not Free Brick" two_each false Brick 1;
    build_road_test "Not Free Ore" two_each false Ore 2;
    build_road_test "Free Wood" two_each true Wood 2;
    build_road_test "Free Sheep" two_each true Sheep 2;
    build_road_test "Free Wheat" two_each true Wheat 2;
    build_road_test "Free Brick" two_each true Brick 2;
    build_road_test "Free Ore" two_each true Ore 2;
    build_settlement_test "Not Free Wood" two_each false Wood 1;
    build_settlement_test "Not Free Sheep" two_each false Sheep 1;
    build_settlement_test "Not Free Wheat" two_each false Wheat 1;
    build_settlement_test "Not Free Brick" two_each false Brick 1;
    build_settlement_test "Not Free Ore" two_each false Ore 2;
    build_settlement_test "Free Wood" two_each true Wood 2;
    build_settlement_test "Free Sheep" two_each true Sheep 2;
    build_settlement_test "Free Wheat" two_each true Wheat 2;
    build_settlement_test "Free Brick" two_each true Brick 2;
    build_settlement_test "Free Ore" two_each true Ore 2;
    build_city_test "Not Free Wood" three_each false Wood 3;
    build_city_test "Not Free Sheep" three_each false Sheep 3;
    build_city_test "Not Free Wheat" three_each false Wheat 1;
    build_city_test "Not Free Brick" three_each false Brick 3;
    build_city_test "Not Free Ore" three_each false Ore 0;
    buy_dev_test "Wood" two_each Wood 2;
    buy_dev_test "Sheep" two_each Sheep 1;
    buy_dev_test "Wheat" two_each Wheat 1;
    buy_dev_test "Brick" two_each Brick 2;
    buy_dev_test "Ore" two_each Ore 1;
  ]

let hex_info_test
    (name : string)
    (board : Board.t)
    (hex : int)
    (expected_output : hex) =
  name >:: fun _ -> assert_equal expected_output (hex_info board hex)

let robber_test
    (name : string)
    (board : Board.t)
    (expected_output : int) =
  name >:: fun _ ->
  assert_equal expected_output
    (Board.get_robber board)
    ~printer:string_of_int

let port_test
    (name : string)
    (board : Board.t)
    (hex : int)
    (dir : int)
    (expected_output : port option) =
  name >:: fun _ ->
  assert_equal expected_output (Board.get_port hex dir board)

let board_tests =
  [
    hex_info_test "basic desert" (make_board ()) 9 Desert;
    hex_info_test "basic other" (make_board ()) 0 (Other (10, Ore));
    robber_test "basic robber" (make_board ()) 9;
    robber_test "moved robber" (Board.move_robber (make_board ()) 3) 3;
    port_test "basic three to one" (make_board ()) 0 0 (Some ThreeToOne);
    port_test "basic two to one" (make_board ()) 15 2
      (Some (TwoToOne Brick));
    port_test "basic not port" (make_board ()) 0 5 None;
  ]

let suite =
  "test suite for OcamlCatan"
  >::: List.flatten [ player_tests; board_tests ]

let _ = run_test_tt_main suite
