open Filename
open AFTN.Logging

type arguments =
  { n_players: int
  ; n_characters: int
  ; use_ash: bool
  ; map_path: string
  ; print_map: bool
  ; log_level: log_type }

let parse_arguments (game_data_path : string) : arguments =
  let n_players = ref 1 in
  let n_characters = ref 1 in
  let use_ash = ref false in
  let map_path = ref (concat (concat game_data_path "maps") "default") in
  let print_map = ref false in
  let log_level = ref Log_Debug in
  let speclist =
    [ ("--players", Arg.Set_int n_players, "Number of players (UNSUPPORTED)")
    ; ( "--characters"
      , Arg.Set_int n_characters
      , "Number of characters to play with" )
    ; ("--use-ash", Arg.Set use_ash, "Include Ash for a more challenging game")
    ; ( "--map"
      , Arg.Set_string map_path
      , "Path to an alternative game board. Run `aftn.ml --map-format` to see \
         details" )
    ; ("--print-map", Arg.Set print_map, "Prints the game map and exits")
    ; ( "--log-level"
      , Arg.String
          (fun s ->
            log_level :=
              match String.lowercase_ascii s with
              | "debug" ->
                  Log_Debug
              | "info" ->
                  Log_Info
              | "warning" ->
                  Log_Warning
              | "critical" ->
                  Log_Critical
              | "error" ->
                  Log_Error
              | _ ->
                  fatal rc_ArgError ("Unrecognized log level " ^ s) )
      , "" )
    ; ( "--locate-game-data"
      , Arg.Unit
          (fun _ ->
            Printf.printf "%s\n" game_data_path ;
            exit 0 )
      , "Print location of game data" ) ]
  in
  let usage_msg = "Usage: AFTN" in
  Arg.parse speclist (fun _ -> ()) usage_msg ;
  { n_players= !n_players
  ; n_characters= !n_characters
  ; use_ash= !use_ash
  ; map_path= !map_path
  ; print_map= !print_map
  ; log_level= !log_level }
