type 'a selection =
  | (* User wants to go back *)
    Back
  (* User gave an invalid selection, optional error string *)
  | Invalid of string option
  (* User selected something *)
  | Selected of 'a

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

let rec confirm (prompt : string option) : bool =
  print_endline (match prompt with None -> "Confirm? (y/n)" | Some s -> s) ;
  let choice = String.lowercase_ascii (read_line ()) in
  if choice = "y" then
    true
  else if choice = "n" then
    false
  else
    confirm prompt
