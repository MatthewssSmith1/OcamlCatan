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
    | "yearofplenty" -> YearOfPlenty
    | "monopoly" -> Monopoly
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
  | [ "buydev" ] -> Types.BuyDevCard
  | [ "usedev"; a ] -> Types.UseDevCard (parse_dev_card [ a ])
  | [ "end" ] -> Types.EndTurn
  | [] -> failwith "empty command"
  | _ -> failwith "malformed command"

let main () =
  (*failwith "TODO"*)
  let game = Game_state.make_new_game in
  WindowDisplay.print_board (Game_state.game_to_board game)

let () = main ()
