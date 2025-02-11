open Argparse
open AFTN.Utils
open AFTN.Map
open AFTN.Game_state
open Filename

let game_data_path = concat (List.nth Aftn_sites.Sites.game_data 0) "game_data"

let () =
  print_endline (concat game_data_path "game_data") ;
  (* Print banner *)
  print_endline
    (String.concat "\n" (read_file_lines (concat game_data_path "banner.txt"))) ;
  let args = Argparse.parse_arguments game_data_path in
  (* Load map *)
  set_map args.map_path ;
  print_endline (map_file_of_map !game_state.map)
