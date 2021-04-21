open Graphics

let print_board (board : Board.t) =
  (* let r =
    Array.make_matrix 39 45
      { ansi_style = [ on_white ]; content = "  " }
  in
  draw_board r board;
  print_raster r; *)
  Graphics.open_graph " 960x540+100-100";
  Graphics.set_window_title "OCaml Catan";
  Graphics.loop_at_exit [] ignore