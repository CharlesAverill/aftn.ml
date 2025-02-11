open Map

(* Information about the state of the game after an ability is used *)
type ability_output =
  { (* Whether this ability used an action *)
    used_action: bool
  ; (* Whether this ability can be used again this turn *)
    can_use_again_this_turn: bool
  ; (* If a character was moved, then the index into the global character list of the character that was moved *)
    moved_character_index: int option }

let default_ability_output : ability_output =
  {used_action= true; can_use_again_this_turn= true; moved_character_index= None}

(* Character information *)
type character =
  { (* General cosmetic data *)
    first_name: string
  ; last_name: string
  ; rank: string
  ; (* Max # of actions per turn *)
    max_actions: int
  ; (* String representation of optional character ability *)
    ability_description: string option
  ; (* Function that takes in current game state (map, activate character, all characters) and performs an action *)
    ability: (map -> character -> character list -> ability_output) option }
