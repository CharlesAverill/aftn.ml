open Argparse
open AFTN.Utils
open AFTN.Map
open AFTN.Game_state
open AFTN.Game_loop
open AFTN.Selection
open Filename
open AFTN.Logging
open Sys

(* SIGINT handler *)
let handle_sigint _ =
  _log Log_Debug "Caught SIGINT, Exiting" ;
  reset_terminal () ;
  exit 0

let game_data_path =
  match Aftn_sites.Sites.game_data with
  | [] ->
      fatal rc_Error "No game data path provided"
  | h :: _ -> concat h "game_data"

(* Manual options selection *)
let select_desktop_options (args : arguments) : arguments =
  Printf.printf "%s\n" "=====GAME OPTIONS=====" ;
  let n_characters = ref 0 in
  while not (1 <= !n_characters && !n_characters <= max_characters) do
    n_characters :=
      get_int (Printf.sprintf "Number of characters? (max %d)" max_characters)
  done ;
  let use_ash = get_bool "Use ash?" in
  {args with n_characters= !n_characters; use_ash}

let () =
  (* Handle SIGINT *)
  Sys.set_signal Sys.sigint (Signal_handle handle_sigint) ;
  (* Handle arguments *)
  let args = Argparse.parse_arguments game_data_path in
  _GLOBAL_LOG_LEVEL := args.log_level ;
  _log Log_Debug ("Game data path: " ^ concat game_data_path "game_data") ;
  (* If started from desktop, do manual arg selection *)
  let args =
    if args.select_options_tui then
      select_desktop_options args
    else
      args
  in
  clear () ;
  (* Load map *)
  set_map args.map_path ;
  if args.print_map then
    Printf.printf "%s\n" (map_file_of_map !game_state.map)
  else (
    (* Print banner *)
    Printf.printf "%s\n"
      (String.concat "\n"
         (read_file_lines (concat game_data_path "banner.txt")) ) ;
    Printf.printf "%s\n" "Press enter to continue" ;
    let _ = read_line () in
    setup_game args.n_characters args.use_ash ;
    game_loop ()
  )
