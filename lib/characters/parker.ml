open Character
open Map
open Selection
open Game_state

let parker_ability (_ : map) (active : character) (_ : character list) :
    ability_output =
  if confirm (Some "Confirm use of this ability? (y/n)") then (
    set_character_scrap active (1 + get_character_scrap active) ;
    {default_ability_output with can_use_again_this_turn= false}
  ) else
    {default_ability_output with used_action= false}

let parker : character =
  { first_name= "Dennis"
  ; last_name= "Parker"
  ; rank= "Chief Engineer"
  ; max_actions= 4
  ; ability_description=
      Some
        "Spend an action: Add 1 Scrap to your inventory. Use only once per \
         turn."
  ; ability= Some parker_ability }
