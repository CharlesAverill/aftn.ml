(** User selections from stdin *)

type 'a selection =
  | Back  (** User wants to go back *)
  | Invalid of string option
      (** User gave an invalid selection, optional error string *)
  | Selected of 'a  (** User selected something *)

(** Given a prompt and a list of options, prompt the user to select an option by index via stdin *)
let rec get_int_selection (prompt : string) (options : string list) :
    int selection =
  print_endline prompt ;
  List.iteri (Printf.printf "\t%d) %s\n") options ;
  print_endline "\tb) Back" ;
  let choice = read_line () in
  match int_of_string_opt choice with
  | Some x ->
      Selected x
  | None ->
      if choice = "b" then
        Back
      else (
        print_endline ("Invalid selection \"" ^ choice ^ "\"") ;
        get_int_selection prompt options
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
