type t = {
  board : Board.t;
  players : Player.t list;
  devs : Types.devCard list;
  longest_road : (Player.t * int) option;
  largest_army : (Player.t * int) option;
  (*0 for start of game, 1 for start of game part 2, 2 for normal play*)
  phase : int;
}

let start_1_done state =
  List.fold_left
    (fun x y ->
      x
      && Player.num_roads y = 1
      && Player.num_settlements y + Player.num_cities y = 1)
    true state.players

let start_2_done state =
  List.fold_left
    (fun x y ->
      x
      && Player.num_roads y = 2
      && Player.num_settlements y + Player.num_cities y = 2)
    true state.players

let roll_dice () =
  (* Random.init (Int.of_float (Unix.time ())); *)
  let die_1 = Random.int 6 + 1 in
  (* Random.init (Random.int 100); *)
  let die_2 = Random.int 6 + 1 in
  die_1 + die_2

let end_turn game =
  let cycle h t game =
    { game with players = t @ [ Player.end_turn h ] }
  in
  if game.phase = 2 then
    match game.players with [] -> game | h :: t -> cycle h t game
  else if start_2_done game then
    match game.players with
    | [] -> game
    | h :: t ->
        {
          game with
          players = List.rev (t @ [ Player.end_turn h ]);
          phase = 2;
        }
  else if game.phase = 1 then
    match game.players with [] -> game | h :: t -> cycle h t game
  else if start_1_done game then
    match game.players with
    | [] -> game
    | h :: t ->
        {
          game with
          players = List.rev (t @ [ Player.end_turn h ]);
          phase = 1;
        }
  else match game.players with [] -> game | h :: t -> cycle h t game

let next_turn (game : t) = failwith "unimplemented"

let current_turn game =
  match game.players with
  | [] -> failwith "No players found"
  | h :: t -> h

let game_to_board game = game.board

let dev_list () =
  let shuffle list =
    (* Random.init (Int.of_float (Unix.time ())); *)
    let random = List.map (fun c -> (Random.bits (), c)) list in
    let sort = List.sort compare random in
    List.map snd sort
  in
  let rec helper dev amount acc =
    if amount <= 0 then acc else helper dev (amount - 1) (dev :: acc)
  in
  [] |> helper Types.Knight 14
  |> helper Types.VictoryPoint 5
  |> helper Types.YearOfPlenty 2
  |> helper Types.Monopoly 2
  |> helper Types.VictoryPoint 2
  |> shuffle

let make_new_game () =
  {
    board = Board.make_random_board ();
    players = [];
    devs = dev_list ();
    longest_road = None;
    largest_army = None;
    phase = 0;
  }

let game_to_players game = game.players

let add_player game_state color =
  {
    game_state with
    players = Player.make_player color :: game_state.players;
  }

let get_player state color =
  let rec helper list color =
    match list with
    | [] -> failwith "Player not found"
    | h :: t -> if Player.get_color h = color then h else helper t color
  in
  helper state.players color

let replace_player state color player =
  let rec helper list color player =
    match list with
    | [] -> []
    | h :: t ->
        if Player.get_color h = color then player :: t
        else h :: helper t color player
  in
  { state with players = helper state.players color player }

let give_resource state color resource amount =
  let new_player =
    Player.add_resource resource amount (get_player state color)
  in
  replace_player state color new_player

let distribute_resources state roll =
  let hexes = Board.int_to_hex_list (game_to_board state) roll in
  let rec vert_helper state verts resource =
    match verts with
    | [] -> state
    | (Types.Empty : Types.vertex) :: t -> vert_helper state t resource
    | Types.Settlement color :: t ->
        vert_helper (give_resource state color resource 1) t resource
    | Types.City color :: t ->
        vert_helper (give_resource state color resource 2) t resource
  in
  let rec hex_helper state hexes =
    match hexes with
    | [] -> state
    | h :: t -> (
        let verts = Board.hex_to_vertices state.board h in
        let hex_info = Board.hex_info state.board h in
        match hex_info with
        | Desert -> failwith "This should never happen"
        | Other (_, res) ->
            let resource = res in
            vert_helper state verts resource)
  in
  hex_helper state hexes

let build_road state hex dir free =
  try
    let player = current_turn state in
    if
      (state.phase = 0 && Player.num_roads player > 0)
      || (state.phase = 1 && Player.num_roads player > 1)
    then (
      print_string "You can not add additional roads at this time";
      state)
    else
      let color = Player.get_color player in
      let new_board = Board.add_road player hex dir state.board in
      let new_player = Player.place_road player free in
      replace_player { state with board = new_board } color new_player
  with Failure x ->
    print_string x;
    state

let distribute_resource_vertex state hex dir player =
  let board = state.board in
  let hex_list = Board.vert_to_adj_hexes board hex dir in
  let rec helper list player =
    match list with
    | [] -> player
    | Types.Desert :: t -> helper t player
    | Types.Other (_, resource) :: t ->
        helper t (Player.add_resource resource 1 player)
  in
  helper hex_list player

let build_settlement state hex dir free =
  try
    let player = current_turn state in
    if
      state.phase = 0
      && Player.num_settlements player + Player.num_cities player > 0
      || state.phase = 1
         && Player.num_settlements player + Player.num_cities player > 1
    then (
      print_string "You can not add additional settlements at this time";
      state)
    else
      let color = Player.get_color (current_turn state) in
      let new_board =
        Board.add_settlement
          (get_player state color)
          hex dir state.board
      in
      if state.phase = 1 then
        let new_player =
          distribute_resource_vertex state hex dir
            (Player.place_settlement (current_turn state) free)
        in
        replace_player { state with board = new_board } color new_player
      else
        let new_player =
          Player.place_settlement (current_turn state) free
        in
        replace_player { state with board = new_board } color new_player
  with Failure x ->
    print_string x;
    state

let upgrade_city state hex dir free =
  try
    let color = Player.get_color (current_turn state) in
    let new_board =
      Board.upgrade_city (get_player state color) hex dir state.board
    in
    let new_player = Player.place_city (current_turn state) free in
    replace_player { state with board = new_board } color new_player
  with Failure x ->
    print_string x;
    state

let buy_dev_card state =
  match state.devs with
  | [] ->
      print_string "No More Development Cards";
      state
  | h :: t -> (
      try
        let color = Player.get_color (current_turn state) in
        let new_player = Player.buy_dev (current_turn state) h in
        replace_player { state with devs = t } color new_player
      with Failure x ->
        print_string x;
        state)

let accept_trade state color offer request =
  let p1 = current_turn state in
  let p2 = get_player state color in
  try
    let new_p1 =
      p1
      |> Player.add_resource_list request
      |> Player.remove_resource_list offer
    in
    let new_p2 =
      p2
      |> Player.add_resource_list offer
      |> Player.remove_resource_list request
    in
    replace_player
      (replace_player state (Player.get_color p1) new_p1)
      color new_p2
  with Failure x ->
    print_string x;
    state

let make_move state input =
  match input with
  | Types.BuildRoad (hex, dir) ->
      if state.phase = 2 then build_road state hex dir false
      else build_road state hex dir true
  | Types.BuildSettlement (hex, dir) ->
      if state.phase = 2 then build_settlement state hex dir false
      else build_settlement state hex dir true
  | Types.UpgradeCity (hex, dir) -> upgrade_city state hex dir false
  | Types.OfferTrade (color, offer, request) ->
      accept_trade state color offer request
  | Types.BankTrade (pffer, request) -> failwith "Unimplemented"
  | Types.BuyDevCard -> buy_dev_card state
  | Types.UseDevCard dev -> failwith "Unimplemented"
  | EndTurn ->
      let roll = roll_dice () in
      print_endline (string_of_int roll ^ " was rolled");
      distribute_resources (end_turn state) roll
(* end_turn state *)
