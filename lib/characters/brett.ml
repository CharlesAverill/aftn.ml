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
        "Items that costs 2 or more scrap has is 1 scrap cheaper. Items don't \
         take an action to craft."
  ; ability= None }
