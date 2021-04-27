type t = {
  board : Board.t;
  players : Player.t list;
  trades : Types.trade_offer list;
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

let make_new_game =
  { board = Board.make_random_board (); players = []; trades = [] }

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

let build_road state color hex dir = state
