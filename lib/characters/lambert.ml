open Character
open Map
open Selection
open Game_state
open Encounter

let lambert_ability (_ : map) (_ : character) (chars : character list) :
    ability_output =
  if List.length !game_state.encounters < 1 then (
    print_endline "No encounters to draw" ;
    {default_ability_output with used_action= false}
  ) else if confirm (Some "Confirm the use of this ability? (y/n)") then (
    let encounter =
      match discard_encounter () with
      | None ->
          (* Never triggered *) Quiet
      | Some x ->
          x
    in
    Printf.printf "Drawn encounter: %s\n" (string_of_encounter encounter) ;
    if confirm (Some "Discard this encounter? (y/n)") then
      default_ability_output
    else (
      replace_encounter () ; default_ability_output
    )
  ) else
    {default_ability_output with used_action= false}

let lambert : character =
  { first_name= "Joan"
  ; last_name= "Lambert"
  ; rank= "Navigator"
  ; max_actions= 4
  ; ability_description=
      Some
        "Spend an action: Look at the upcoming encounter, you may discard it."
  ; ability= Some lambert_ability }
