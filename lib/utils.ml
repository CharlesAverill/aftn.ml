(** Standard utilities *)
open Logging

(** Shuffle a [list] *)
let shuffle d =
  let nd = List.map (fun c -> (Random.bits (), c)) d in
  let sond = List.sort compare nd in
  List.map snd sond

(** Select an item from random in a list *)
let select_random l =
  match l with
  | [] ->
      fatal rc_Error "Tried to select from empty list"
  | _ ->
      List.nth l (Random.int_in_range ~min:0 ~max:(List.length l - 1))

(** Replace in a [list] by index *)
let replacei l pos a =
  List.mapi
    (fun i x ->
      if i = pos then
        a
      else
        x )
    l

(** Replace in a [list] all items that satisfy a ['a -> bool] function *)
let replace l f a =
  List.map
    (fun x ->
      if f x then
        a
      else
        x )
    l

(** Read and return the lines of a file *)
let read_file_lines file : string list =
  let in_ch = open_in file in
  let rec read_line () =
    match try Some (input_line in_ch) with End_of_file -> None with
    | None ->
        []
    | Some line ->
        line :: read_line ()
  in
  read_line ()

(** For bounds [i] and [j], compute the list [i..j], inclusive *)
let range i j =
  let rec aux n acc =
    if n < i then
      acc
    else
      aux (n - 1) (n :: acc)
  in
  aux j []

(** Functional map update *)
let _update f eq x y =
 fun z ->
  if eq x z then
    y
  else
    f z

let update f x y = _update f (fun x y -> x = y) x y

let rec len_partition (l : 'a list) (n : int) : 'a list list =
  if List.length l > n then
    List.take n l :: len_partition (List.drop n l) n
  else
    [l]

let rec transpose l =
  match l with
  | [] ->
      []
  | [] :: l' ->
      transpose l'
  | (x :: xs) :: l' ->
      let first_column =
        x
        :: List.fold_left
             (fun a i -> match i with [] -> a | h :: _ -> h :: a)
             [] l'
      in
      let rest_columns =
        transpose
          ( xs
          :: List.fold_left
               (fun a i -> match i with [] -> a | _ :: t -> t :: a)
               [] l' )
      in
      first_column :: rest_columns
