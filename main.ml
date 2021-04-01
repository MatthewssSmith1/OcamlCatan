let main () =
  (*failwith "TODO"*)
  let game = Game_state.make_new_game in
  Display.print_board (Game_state.game_to_board game)

let () = main ()
