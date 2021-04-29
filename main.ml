let parse_hex_dir_tuple = function
  | [ h; d ] -> (int_of_string h, int_of_string d)
  | _ -> failwith "bad list"

let string_to_resource str =
  Types.(
    match str with
    | "wood" -> Wood
    | "sheep" -> Sheep
    | "wheat" -> Wheat
    | "brick" -> Brick
    | "ore" -> Ore
    | _ -> failwith "bad resource type")

let string_to_color str =
  Types.(
    match str with
    | "red" -> Red
    | "orange" -> Orange
    | "blue" -> Blue
    | "white" -> White
    | _ -> failwith "bad color")

(** take a string of form ["{ore, ore, brick, sheep}"] and returns a
    corresponding resource list*)
let parse_resource_list str =
  String.sub str 1 (String.length str - 2)
  |> String.split_on_char ','
  |> List.map
       String.(
         fun a -> a |> trim |> lowercase_ascii |> string_to_resource)

let parse_trade lst =
  match lst with
  | [ c; o; w ] ->
      (string_to_color c, parse_resource_list o, parse_resource_list w)
  | _ -> failwith "bad trade"

let string_to_dev_card str =
  Types.(
    match str with
    | "knight" -> Knight
    | "roadbuilding" -> RoadBuilding
    | "monopoly" -> Monopoly
    | "yearofplenty" -> YearOfPlenty
    | "victorypoint" -> VictoryPoint
    | _ -> failwith "bad dev card")

let parse_dev_card lst =
  match lst with
  | [ d ] -> d |> String.lowercase_ascii |> string_to_dev_card
  | _ -> failwith "bad dev card"

exception Quit

exception Help

exception Redraw

let command_info =
  [
    "road (r) - builds a road";
    "settlement (s) - builds a settlement";
    "upgrade (u) - builds a upgrade";
    "trade (t) - offers a trade";
    "bank (b) - trades with bank";
    "buydev (bd) - buy development card";
    "usedev (ud) - use development card";
    "end (e) - ends turn";
    "draw (d) - redraw the window (if you closed it)";
    "quit (q) - quits game";
  ]

let print_commands () = print_endline "commands"

let parse_command input =
  let words =
    input
    |> String.split_on_char ' '
    |> List.filter (fun s -> String.length s > 0)
  in

  let words =
    match words with
    | "r" :: tl -> "road" :: tl
    | "s" :: tl -> "settlement" :: tl
    | "u" :: tl -> "upgrade" :: tl
    | "t" :: tl -> "trade" :: tl
    | "b" :: tl -> "bank" :: tl
    | "bd" :: tl -> "buydev" :: tl
    | "ud" :: tl -> "usedev" :: tl
    | "e" :: tl -> "end" :: tl
    | "q" :: tl -> "quit" :: tl
    | "d" :: tl -> "draw" :: tl
    | w -> w
  in

  match words with
  | [ "road"; a; b ] -> Types.BuildRoad (parse_hex_dir_tuple [ a; b ])
  | [ "settlement"; a; b ] ->
      Types.BuildSettlement (parse_hex_dir_tuple [ a; b ])
  | [ "upgrade"; a; b ] ->
      Types.UpgradeCity (parse_hex_dir_tuple [ a; b ])
  | [ "trade"; a; b; c ] -> Types.OfferTrade (parse_trade [ a; b; c ])
  | [ "bank"; a; b ] ->
      Types.BankTrade (parse_resource_list a, parse_resource_list b)
  | [ "buydev" ] -> Types.BuyDevCard
  | [ "usedev"; a ] -> Types.UseDevCard (parse_dev_card [ a ])
  | [ "end" ] -> Types.EndTurn
  (* raises Quit up the whole call stack to main *)
  | "quit" :: _ -> raise Quit
  | "help" :: _ -> raise Help
  | "draw" :: _ -> raise Redraw
  | [] -> failwith "empty command"
  | _ -> failwith "malformed command"

let rec next_state game =
  let err_msg = "invalid command: " in
  let print_err = function
    | Failure str -> print_endline (err_msg ^ str)
    | _ -> print_endline err_msg
  in
  print_string "> ";
  try read_line () |> parse_command |> Game_state.make_move game with
  | Quit -> raise Quit
  | Help ->
      print_commands ();
      next_state game
  | Redraw ->
      Display.initialize ();
      Display.print_game game;
      next_state game
  | f ->
      print_err f;
      next_state game

let main () =
  Random.init (Int.of_float (Unix.time ()));
  let add_p color state = Game_state.add_player state color in

  let game =
    Game_state.make_new_game ()
    |> add_p Types.Red |> add_p Types.Blue |> add_p Types.Orange
    |> add_p Types.White
  in

  let rec turn game =
    let new_state = next_state game in
    Display.print_game new_state;
    turn new_state
  in

  Display.initialize ();
  Display.print_game game;
  print_endline "type 'help' for a list of commands";
  try turn game
  with Quit ->
    ();
    ()

let () = main ()
