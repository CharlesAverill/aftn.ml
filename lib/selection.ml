(** User selections from stdin *)

open Utils

type 'a selection =
  | Back  (** User wants to go back *)
  | Invalid of string option
      (** User gave an invalid selection, optional error string *)
  | Selected of 'a  (** User selected something *)

(** Given a prompt and a list of options, prompt the user to select an option by index via stdin *)
let rec get_int_selection (prompt : string) (options : string list)
    (allow_back : bool) : int option =
  print_endline prompt ;
  let options =
    if allow_back then
      options @ ["Back"]
    else
      options
  in
  let cutoff_height = 7 in
  ( if List.length options <= cutoff_height then
      List.iteri
        (fun i ->
          if allow_back && i = List.length options - 1 then
            Printf.printf "\t b) %s\n"
          else
            (Printf.printf "\t%2d) %s\n") (i + 1) )
        options
    else
      (* Columns of height cutoff_height, rows of length (#options / cutoff_height) *)
      let col_width = List.length options / cutoff_height in
      let rows = transpose (len_partition options cutoff_height) in
      List.iter (fun row -> print_endline (String.concat " - " row)) rows ;
      let max_len =
        List.fold_left
          (fun a o ->
            if String.length o > a then
              String.length o
            else
              a )
          0 options
      in
      for row = 1 to cutoff_height do
        for col = 1 to col_width + 1 do
          let idx = (cutoff_height * (col - 1)) + (row - 1) in
          if allow_back && idx = List.length options - 1 then
            Printf.printf "\t b) %-*s" max_len (List.nth options idx)
          else if idx < List.length options then
            Printf.printf "\t%2d) %-*s" (1 + idx) max_len (List.nth options idx)
        done ;
        print_endline ""
      done ) ;
  let choice = read_line () in
  match int_of_string_opt choice with
  | Some x ->
      if x - 1 < List.length options then
        Some (x - 1)
      else (
        print_endline ("Invalid selection \"" ^ choice ^ "\"") ;
        get_int_selection prompt options allow_back
      )
  | None ->
      if choice = "b" && allow_back then
        None
      else (
        print_endline ("Invalid selection \"" ^ choice ^ "\"") ;
        get_int_selection prompt options allow_back
      )

(** A yes-or-no confirmation with a customizable prompt *)
let rec confirm (prompt : string option) : bool =
  print_endline (match prompt with None -> "Confirm? (y/n)" | Some s -> s) ;
  let choice = String.lowercase_ascii (read_line ()) in
  if choice = "y" then
    true
  else if choice = "n" then
    false
  else
    confirm prompt
