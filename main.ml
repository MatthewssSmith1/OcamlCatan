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

let parseCommand input =
  let words =
    input
    |> String.split_on_char ' '
    |> List.filter (fun s -> String.length s > 0)
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
  | [] -> failwith "empty command"
  | _ -> failwith "malformed command"

let rec next_state game =
  let print_err = function
    | Failure str -> print_endline ("invalid command: " ^ str)
    | _ -> print_endline "invalid command"
  in
  print_string "> ";
  try read_line () |> parseCommand |> Game_state.make_move game
  with f ->
    print_err f;
    next_state game

let main () =
  let add_p color state = Game_state.add_player state color in

  let game = Game_state.make_new_game ()
  |> add_p Types.Red |> add_p Types.Blue |> add_p Types.Orange
  |> add_p Types.White in

  let rec turn game =
    let new_state = next_state game in
    WindowDisplay.print_game new_state;
    turn new_state in

  WindowDisplay.print_game game;
  turn game

let () = main ()
