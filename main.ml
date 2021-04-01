let main () = (*failwith "TODO"*)
  let game = Game_state.make_new_game in
  Display.print_board (Game_state.get_board (Game_state.add_player game Types.Red))
  

let () = main ()