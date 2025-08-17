type score = {player: string; turns: int; n_characters: int}

type stats =
  { mutable movements: int
  ; mutable rooms_visited: int
  ; mutable items_collected: int
  ; mutable scrap_collected: int
  ; mutable items_dropped: int
  ; mutable scrap_dropped: int
  ; mutable abilities_used: int
  ; mutable items_crafted: int
  ; mutable incinerator_used: int
  ; mutable flashlight_used: int
  ; mutable grapplegun_used: int
  ; mutable prod_used: int
  ; mutable motion_tracker_used: int
  ; mutable cat_carrier_used: int }

let global_stats =
  { movements= 0
  ; rooms_visited= 0
  ; items_collected= 0
  ; scrap_collected= 0
  ; items_dropped= 0
  ; scrap_dropped= 0
  ; abilities_used= 0
  ; items_crafted= 0
  ; incinerator_used= 0
  ; flashlight_used= 0
  ; grapplegun_used= 0
  ; prod_used= 0
  ; motion_tracker_used= 0
  ; cat_carrier_used= 0 }

let print_stats () =
  Printf.printf "=====GAME STATS=====\n" ;
  Printf.printf "Movements: %d\n" global_stats.movements ;
  Printf.printf "Rooms visited: %d\n" global_stats.rooms_visited ;
  Printf.printf "Items collected: %d\n" global_stats.items_collected ;
  Printf.printf "Scrap collected: %d\n" global_stats.scrap_collected ;
  Printf.printf "Items dropped: %d\n" global_stats.items_dropped ;
  Printf.printf "Scrap dropped: %d\n" global_stats.scrap_dropped ;
  Printf.printf "Abilities used: %d\n" global_stats.abilities_used ;
  Printf.printf "Items crafted: %d\n" global_stats.items_crafted ;
  Printf.printf "Incinerator used: %d times\n" global_stats.incinerator_used ;
  Printf.printf "Flashlight used: %d times\n" global_stats.flashlight_used ;
  Printf.printf "Grapple gun used: %d times\n" global_stats.grapplegun_used ;
  Printf.printf "Prod used: %d times\n" global_stats.prod_used ;
  Printf.printf "Motion tracker used: %d times\n"
    global_stats.motion_tracker_used ;
  Printf.printf "Cat carrier used: %d times\n" global_stats.cat_carrier_used

let score_to_csv (s : score) : string =
  Printf.sprintf "%s,%d,%d" s.player s.turns s.n_characters

let read_scores_from_csv (filename : string) : score list =
  if not (Sys.file_exists filename) then
    []
  else
    let ic = open_in filename in
    let rec loop acc =
      match input_line ic with
      | line -> (
        match String.split_on_char ',' line with
        | [player; turns; n_characters] ->
            let turns = int_of_string turns in
            let n_characters = int_of_string n_characters in
            loop ({player; turns; n_characters} :: acc)
        | _ ->
            loop acc )
      | exception End_of_file ->
          acc
    in
    let scores = loop [] in
    close_in ic ; List.rev scores

let write_scores_to_csv (filename : string) (scores : score list) : unit =
  let oc = open_out filename in
  List.iter (fun s -> output_string oc (score_to_csv s ^ "\n")) scores ;
  close_out oc

let sort_scores (scores : score list) : score list =
  List.sort (fun a b -> compare a.turns b.turns) scores

let sort_csv_file (filename : string) : unit =
  let scores = read_scores_from_csv filename in
  let sorted = sort_scores scores in
  write_scores_to_csv filename sorted

let find_score_index (filename : string) (target : score) : int option =
  let scores = read_scores_from_csv filename in
  let rec aux idx = function
    | [] ->
        None
    | s :: rest ->
        if s = target then
          Some idx
        else
          aux (idx + 1) rest
  in
  aux 0 scores

let show_top_n_scores (filename : string) (n : int) : unit =
  let scores = read_scores_from_csv filename |> sort_scores in
  List.iteri
    (fun i s ->
      if i < n then
        Printf.printf "%d. %s - Turns: %d, Characters: %d\n" (i + 1) s.player
          s.turns s.n_characters )
    scores

let append_score_to_csv (filename : string) (s : score) : unit =
  (* Read existing scores *)
  let scores = try read_scores_from_csv filename with _ -> [] in
  (* Add new score and sort *)
  let updated_scores = sort_scores (s :: scores) in
  (* Keep only top 100 *)
  let top_scores =
    if List.length updated_scores > 100 then
      List.take 100 updated_scores
    else
      updated_scores
  in
  (* Overwrite file with top scores *)
  write_scores_to_csv filename top_scores
