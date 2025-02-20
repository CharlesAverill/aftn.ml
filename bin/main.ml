open Argparse
open AFTN.Utils
open AFTN.Map
open AFTN.Game_state
open AFTN.Game_loop
open Filename
open AFTN.Logging

let game_data_path = concat (List.nth Aftn_sites.Sites.game_data 0) "game_data"

let () =
  (* Handle arguments *)
  let args = Argparse.parse_arguments game_data_path in
  _GLOBAL_LOG_LEVEL := args.log_level ;
  _log Log_Debug ("Game data path: " ^ concat game_data_path "game_data") ;
  (* Load map *)
  set_map args.map_path ;
  if args.print_map then
    print_endline (map_file_of_map !game_state.map)
  else (
    (* Print banner *)
    print_endline
      (String.concat "\n"
         (read_file_lines (concat game_data_path "banner.txt")) ) ;
    print_endline "Press enter to continue" ;
    let _ = read_line () in
    setup_game args.n_characters args.use_ash ;
    game_loop ()
  )
