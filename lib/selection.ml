(** User selections from stdin *)

type 'a selection =
  | Back  (** User wants to go back *)
  | Invalid of string option
      (** User gave an invalid selection, optional error string *)
  | Selected of 'a  (** User selected something *)

(** Given a prompt and a list of options, prompt the user to select an option by index via stdin *)
let rec get_int_selection (prompt : string) (options : string list)
    (allow_back : bool) : int option =
  print_endline prompt ;
  List.iteri (fun i -> (Printf.printf "\t%d) %s\n") (i + 1)) options ;
  if allow_back then print_endline "\tb) Back" ;
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
