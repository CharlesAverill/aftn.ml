(** User selections from stdin *)

open Logging
open Unix

type 'a selection =
  | Back  (** User wants to go back *)
  | Invalid of string option
      (** User gave an invalid selection, optional error string *)
  | Selected of 'a  (** User selected something *)

let reset_terminal () =
  tcsetattr stdin TCSANOW {(tcgetattr stdin) with c_echo= true; c_icanon= true} ;
  flush Stdlib.stdout ;
  flush Stdlib.stderr

let read_n_chars n =
  flush Stdlib.stdout ;
  (* Disable echo & canonical mode *)
  let term_io = tcgetattr stdin in
  let new_term_io = {term_io with c_echo= false; c_icanon= false} in
  tcsetattr stdin TCSANOW new_term_io ;
  let buffer = Bytes.create n in
  really_input Stdlib.stdin buffer 0 n ;
  (* Restore original settings *)
  reset_terminal () ;
  buffer

(** Given a prompt and a list of options, prompt the user to select an option by 
    index or matching string via stdin *)
let rec get_int_selection ?(keys : char list = [])
    ?(back_string : string = "Back") (prompt : string) (options : string list)
    (allow_back : bool) : int option =
  if List.exists (fun c -> c = 'b') keys then
    _log Log_Warning "get_int_selection ?keys should not contain 'b'" ;
  if keys != [] && List.length keys != List.length options then
    fatal rc_Error
      (Printf.sprintf
         "get_int_selection ?keys must be same length as options, got %d and %d"
         (List.length keys) (List.length options) )
  else (
    print_endline prompt ;
    let options =
      if allow_back then
        options @ [back_string]
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
              (Printf.printf "\t%s) %s\n")
                ( if keys = [] then
                    Printf.sprintf "%2d" (i + 1)
                  else
                    String.make 1 (List.nth keys i) ) )
          options
      else
        (* Columns of height cutoff_height, rows of length (#options / cutoff_height) *)
        let col_width = List.length options / cutoff_height in
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
              Printf.printf "\t%s) %-*s"
                ( if keys = [] then
                    Printf.sprintf "%2d" (idx + 1)
                  else
                    String.make 1 (List.nth keys idx) )
                max_len (List.nth options idx)
          done ;
          print_endline ""
        done ) ;
    let choice = ref "" in
    while String.trim !choice = "" do
      choice :=
        if keys = [] && List.length options >= 10 then
          read_line ()
        else
          Bytes.to_string (read_n_chars 1)
    done ;
    match int_of_string_opt !choice with
    | Some x ->
        (* Index selection *)
        if x - 1 < List.length options then
          Some (x - 1)
        else (
          print_endline ("Invalid selection \"" ^ !choice ^ "\"") ;
          get_int_selection prompt ~keys ~back_string
            (List.filter (fun s -> s != back_string) options)
            allow_back
        )
    | None -> (
        if
          (* Check for back selection *)
          ( String.lowercase_ascii !choice = "b"
          || String.lowercase_ascii !choice = String.lowercase_ascii back_string
          )
          && allow_back
        then
          None
        else
          (* Check for matching string selection *)
            match
              List.find_opt
                (fun s ->
                  String.lowercase_ascii s = String.lowercase_ascii !choice )
                options
            with
          | None when keys != [] -> (
            match List.find_opt (fun s -> !choice = String.make 1 s) keys with
            | None ->
                print_endline ("Invalid selection \"" ^ !choice ^ "\"") ;
                get_int_selection ~keys ~back_string prompt
                  (List.filter (fun s -> s != back_string) options)
                  allow_back
            | Some x ->
                List.find_index (fun s -> s = x) keys )
          | None ->
              print_endline ("Invalid selection \"" ^ !choice ^ "\"") ;
              get_int_selection ~keys ~back_string prompt
                (List.filter (fun s -> s != back_string) options)
                allow_back
          | Some o ->
              List.find_index (fun s -> s = o) options )
  )

(** A yes-or-no confirmation with a customizable prompt *)
let rec confirm (prompt : string option) : bool =
  print_endline (match prompt with None -> "Confirm? (y/n)" | Some s -> s) ;
  let choice = ref "" in
  while String.trim !choice = "" do
    choice := String.lowercase_ascii (Bytes.to_string (read_n_chars 1))
  done ;
  if !choice = "y" then
    true
  else if !choice = "n" then
    false
  else
    confirm prompt
