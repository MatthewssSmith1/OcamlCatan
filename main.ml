let main () =
  (*failwith "TODO"*)
  let board = Board.make_board () in
  let game = Game_state.make_new_game in
  print_endline (string_of_bool (board = Game_state.game_to_board game));
  Display.print_board (Game_state.game_to_board game)

let () = main ()
