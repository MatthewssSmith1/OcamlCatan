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
    input |> String.split_on_char ' ' |> List.map String.trim
  in
  match words with
  | h :: t -> (
      Types.(
        match h with
        | "road" -> BuildRoad (parse_hex_dir_tuple t)
        | "settlement" -> BuildSettlement (parse_hex_dir_tuple t)
        | "upgrade" -> UpgradeCity (parse_hex_dir_tuple t)
        | "trade" -> OfferTrade (parse_trade t)
        | "buydev" -> BuyDevCard
        | "usedev" -> UseDevCard (parse_dev_card t)
        | "end" -> EndTurn
        | _ -> failwith "unknown command"))
  | [] -> failwith "empty command"

let main () =
  (*failwith "TODO"*)
  let game = Game_state.make_new_game in
  WindowDisplay.print_board (Game_state.game_to_board game)

let () = main ()
