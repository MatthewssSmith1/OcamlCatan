type t = {
  board : Board.t;
  players : Player.t list;
  devs : Types.devCard list;
  longest_road : (Player.t * int) option;
  largest_army : (Player.t * int) option;
}

let next_turn game =
  match game.players with
  | [] -> game
  | h :: t -> { game with players = t @ [ Player.end_turn h ] }

let current_turn game =
  match game.players with
  | [] -> failwith "No players found"
  | h :: t -> h

let game_to_board game = game.board

let dev_list () =
  let shuffle list =
    Random.init (Int.of_float (Unix.time ()));
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

let make_new_game =
  {
    board = Board.make_random_board ();
    players = [];
    devs = dev_list ();
    longest_road = None;
    largest_army = None;
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
        else helper t color player
  in
  { state with players = helper state.players color player }

let give_resource state color resource amount =
  let new_player =
    Player.add_resource resource amount (get_player state color)
  in
  replace_player state color new_player

let distribute_resources state input =
  let hexes = Board.int_to_hex_list (game_to_board state) input in
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

let build_road state hex dir =
  try
    let color = Player.get_color (current_turn state) in
    let new_board =
      Board.add_road (get_player state color) hex dir state.board
    in
    let new_player = Player.place_road (current_turn state) in
    replace_player { state with board = new_board } color new_player
  with Failure x ->
    print_string x;
    state

let build_settlement state hex dir =
  try
    let color = Player.get_color (current_turn state) in
    let new_board =
      Board.add_settlement (get_player state color) hex dir state.board
    in
    let new_player = Player.place_settlement (current_turn state) in
    replace_player { state with board = new_board } color new_player
  with Failure x ->
    print_string x;
    state

let upgrade_city state hex dir =
  try
    let color = Player.get_color (current_turn state) in
    let new_board =
      Board.upgrade_city (get_player state color) hex dir state.board
    in
    let new_player = Player.place_city (current_turn state) in
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

let accept_trade state (offer : Types.trade_offer) color =
  let p1 = current_turn state in
  let p2 = get_player state color in
  try
    let new_p1 =
      p1
      |> Player.add_resource_list offer.request
      |> Player.remove_resource_list offer.offer
    in
    let new_p2 =
      p2
      |> Player.add_resource_list offer.offer
      |> Player.remove_resource_list offer.request
    in
    replace_player
      (replace_player state (Player.get_color p1) new_p1)
      color new_p2
  with Failure x ->
    print_string x;
    state
