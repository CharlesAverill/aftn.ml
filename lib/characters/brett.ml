open Character
open Map
open Selection
open Game_state

let brett : character =
  { first_name= "Samson"
  ; last_name= "Brett"
  ; rank= "Engineering Tech"
  ; max_actions= 3
  ; ability_description=
      Some
        "Any item that costs 2 or more Scrap has its cost reduced by 1 Scrap. \
         Items don't take an action to craft."
  ; ability= None }
