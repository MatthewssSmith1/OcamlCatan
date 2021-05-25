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

let player_tests = []

let suite =
  "test suite for OcamlCatan" >::: List.flatten [ player_tests ]

let _ = run_test_tt_main suite
