type item =
  | Incinerator
  | Flashlight
  | GrappleGun
  | ElectricProd
  | MotionTracker
  | CoolantCanister

let string_of_item = function
  | Incinerator ->
      "INCINERATOR"
  | Flashlight ->
      "FLASHLIGHT"
  | GrappleGun ->
      "GRAPPLE GUN"
  | ElectricProd ->
      "ELECTRIC PROD"
  | MotionTracker ->
      "MOTION TRACKER"
  | CoolantCanister ->
      "COOLANT CANISTER"

(** Which indefinite article to use when referring to the item *)
let article_of_item = function
  | Incinerator | ElectricProd ->
      "an"
  | Flashlight | GrappleGun | MotionTracker | CoolantCanister ->
      "a"

let cost_of_item = function
  | Flashlight ->
      2
  | MotionTracker | GrappleGun | ElectricProd ->
      3
  | Incinerator ->
      4
  | CoolantCanister ->
      -1

let craftable_items =
  [Flashlight; MotionTracker; GrappleGun; ElectricProd; Incinerator]
