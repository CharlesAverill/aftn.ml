(** Character information *)

open Map

(** Information about the state of the game after an ability is used *)
type ability_output =
  { used_action: bool  (** Whether this ability used an action *)
  ; can_use_again_this_turn: bool
        (*** Whether this ability can be used again this turn *)
  ; moved_character_index: int option
        (*** If a character was moved, then the index into the global character list of the character that was moved *)
  }

let default_ability_output : ability_output =
  {used_action= true; can_use_again_this_turn= true; moved_character_index= None}

(*** Character information *)
type character =
  { (* General cosmetic data *)
    first_name: string
  ; last_name: string
  ; rank: string
  ; max_actions: int  (** Max # of actions per turn *)
  ; ability_description: string option
        (** [string] representation of optional character ability *)
  ; ability: (map -> character -> character list -> ability_output) option
        (** Function that takes in current game state (map, activate character, all characters) and performs an action *)
  }
