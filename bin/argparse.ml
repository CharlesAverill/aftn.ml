open Filename

type arguments =
  { n_players: int
  ; n_characters: int
  ; use_ash: bool
  ; map_path: string
  ; print_map: bool }

let parse_arguments (game_data_path : string) : arguments =
  let n_players = ref 1 in
  let n_characters = ref 1 in
  let use_ash = ref false in
  let map_path = ref (concat (concat game_data_path "maps") "default") in
  let print_map = ref false in
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
    ; ("--print-map", Arg.Set print_map, "Prints the game map and exits") ]
  in
  let usage_msg = "Usage: $PROJECT_NAME -n NUM_ARG" in
  Arg.parse speclist
    (fun n -> print_endline ("Anonymous argument: " ^ n))
    usage_msg ;
  { n_players= !n_players
  ; n_characters= !n_characters
  ; use_ash= !use_ash
  ; map_path= !map_path
  ; print_map= !print_map }
