open Argparse
open AFTN.Utils
open AFTN.Map
open AFTN.Game_state

let () =
  (* Print banner *)
  print_endline
    (String.concat "\n"
       (read_file_lines "/home/charles/Desktop/aftn.ml/game_data/banner.txt") ) ;
  let args = Argparse.parse_arguments () in
  print_endline (map_file_of_map !game_state.map)
