let shuffle d =
  let nd = List.map (fun c -> (Random.bits (), c)) d in
  let sond = List.sort compare nd in
  List.map snd sond

let replacei l pos a =
  List.mapi
    (fun i x ->
      if i = pos then
        a
      else
        x )
    l

let replace l f a =
  List.map
    (fun x ->
      if f x then
        a
      else
        x )
    l

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

let range i j =
  let rec aux n acc =
    if n < i then
      acc
    else
      aux (n - 1) (n :: acc)
  in
  aux j []

let update f x y =
 fun z ->
  if x = z then
    y
  else
    f z
