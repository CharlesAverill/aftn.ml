open Character
open Map
open Selection
open Game_state

let ripley_ability (_ : map) (_ : character) (chars : character list) :
    ability_output =
  match
    get_int_selection "Pick a character to move"
      (List.map
         (fun c ->
           Printf.sprintf "%s at %s\n" c.last_name (locate_character c).name )
         chars )
      true
  with
  | None ->
      {default_ability_output with used_action= false}
  | Some x ->
      let selected_char = List.nth chars x in
      { default_ability_output with
        moved_character_index=
          List.find_index (fun c -> c = selected_char) !game_state.characters }

let ripley : character =
  { first_name= "Ellen"
  ; last_name= "Ripley"
  ; rank= "Warrant Officer"
  ; max_actions= 4
  ; ability_description= Some "Spend an action: Move another crewmember 1 SPACE"
  ; ability= Some ripley_ability }
