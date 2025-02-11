(** Map parsing and utilities *)

open Utils
open Logging

type room =
  { (* Name of this room. If None, this room is a corridor *)
    name: string
  ; (* If the room is a corridor *)
    is_corridor: bool
  ; (* How much scrap is in this room *)
    num_scrap: int
  ; (* Whether the room has an event *)
    has_event: bool
  ; (* Items in the room *)
    (* TODO: change to items *)
    items: unit list
  ; (* Connected rooms *)
    connections: room list
  ; (* Room connected by a ladder *)
    ladder_connection: room option }

(** Get [string] representation of [room] *)
let string_of_room (r : room) =
  Printf.sprintf
    "Name: %s\n\
     Is corridor: %b\n\
     Scrap: %n\n\
     Has event: %b\n\
     Items: TODO\n\
     Connections: %s\n\
     Ladder connection: %s\n"
    r.name r.is_corridor r.num_scrap r.has_event
    (String.concat "," (List.map (fun (r : room) -> r.name) r.connections))
    (match r.ladder_connection with None -> "None" | Some s -> s.name)

let blank_room =
  { name= ""
  ; is_corridor= true
  ; num_scrap= 0
  ; has_event= false
  ; items= []
  ; connections= []
  ; ladder_connection= None }

(** Create new non-corridor [room] *)
let new_room (name : string) : room = {blank_room with name; is_corridor= false}

type map =
  { (* Name of this map *)
    map_name: string
  ; (* Rooms in this map *)
    rooms: room list
  ; player_start_room: room
  ; xeno_start_room: room
  ; ash_start_room: room
  ; (* Rooms that start with scrap *)
    scrap_rooms: room list
  ; (* Rooms that start with an event *)
    event_rooms: room list
  ; (* Rooms that start with coolant *)
    coolant_rooms: room list
  ; (* ASCII representation of map *)
    ascii_map: string option }

(** Get a [string] representation of a [map] that is equivalent to the input map file *)
let map_file_of_map m =
  Printf.sprintf
    "%s\n\
     ---\n\
     %s\n\
     ---\n\
     %d\n\
     ---\n\
     %s\n\
     ---\n\
     %s\n\
     ---\n\
     %s\n\
     ---\n\
     %s\n\
     ---\n\
     %s\n\
     ---\n\
     %s\n"
    (* Map name *)
    m.map_name
    (* Room names *)
    (String.trim
       (List.fold_left
          (fun s (r : room) ->
            if r.is_corridor then
              s
            else
              s
              ^ ( if r = m.player_start_room then
                    "*"
                  else if r = m.xeno_start_room then
                    "&"
                  else if r = m.ash_start_room then
                    "$"
                  else
                    "" )
              ^ r.name ^ "\n" )
          "" m.rooms ) )
    (* Number of corridors *)
    (List.length (List.filter (fun r -> r.is_corridor) m.rooms))
    (* Connections *)
    (String.trim
       (snd
          (List.fold_left
             (fun (seen, s) (r : room) ->
               ( List.fold_left
                   (fun s (a1, a2) ->
                     fun (x1, x2) ->
                      if (x1.name, x2.name) = (a1.name, a2.name) then
                        true
                      else
                        s (x1, x2) )
                   seen
                   (List.fold_left
                      (fun a x -> (r, x) :: (x, r) :: a)
                      [] r.connections )
               , let unseen =
                   List.filter (fun x -> not (seen (x, r))) r.connections
                 in
                 if unseen = [] then
                   s
                 else
                   s ^ r.name ^ ";"
                   ^ String.concat ";"
                       (List.map (fun (r : room) -> r.name) unseen)
                   ^ "\n" ) )
             ((fun _ -> false), "")
             m.rooms ) ) )
    (* Ladders *)
    (String.trim
       (snd
          (List.fold_left
             (fun (seen, s) r ->
               if seen r then
                 (seen, s)
               else
                 match r.ladder_connection with
                 | None ->
                     (seen, s)
                 | Some lc ->
                     ( (fun x ->
                         if x.name = lc.name then
                           true
                         else
                           seen x )
                     , s ^ r.name ^ ";" ^ lc.name ^ "\n" ) )
             ((fun _ -> false), "")
             m.rooms ) ) )
    (* Scrap rooms *)
    (String.concat ";" (List.map (fun (r : room) -> r.name) m.scrap_rooms))
    (* Event rooms *)
    (String.concat ";" (List.map (fun (r : room) -> r.name) m.event_rooms))
    (* Coolant rooms *)
    (String.concat ";" (List.map (fun (r : room) -> r.name) m.coolant_rooms))
    (* ASCII map *)
    ( match m.ascii_map with
    | None ->
        ""
    | Some s ->
        s )

let blank_map =
  { map_name= ""
  ; rooms= []
  ; player_start_room= blank_room
  ; xeno_start_room= blank_room
  ; ash_start_room= blank_room
  ; scrap_rooms= []
  ; event_rooms= []
  ; coolant_rooms= []
  ; ascii_map= None }

(** Find the [room] with a given name in a [map] *)
let find_room_by_name (m : map) (name : string) : room =
  try List.find (fun (r : room) -> r.name = name) m.rooms
  with Not_found -> fatal rc_Error ("Failed to find room \"" ^ name ^ "\"")

(** Determines which phase of map file parsing is ongoing *)
type map_parsing_state =
  | MapName
  | RoomNames
  | NumCorridors
  | RoomConnections
  | Ladders
  | ScrapRooms
  | EventRooms
  | CoolantRooms
  | AsciiMap

let string_of_parsing_state = function
  | MapName ->
      "Map name"
  | RoomNames ->
      "Room names"
  | NumCorridors ->
      "Number of corridors"
  | RoomConnections ->
      "Room connections"
  | Ladders ->
      "Ladders"
  | ScrapRooms ->
      "Scrap rooms"
  | EventRooms ->
      "Event rooms"
  | CoolantRooms ->
      "Coolant rooms"
  | AsciiMap ->
      "ASCII map"

(** Step parsing state forward *)
let advance_parsing_state = function
  | None ->
      None
  | Some x -> (
    match x with
    | MapName ->
        Some RoomNames
    | RoomNames ->
        Some NumCorridors
    | NumCorridors ->
        Some RoomConnections
    | RoomConnections ->
        Some Ladders
    | Ladders ->
        Some ScrapRooms
    | ScrapRooms ->
        Some EventRooms
    | EventRooms ->
        Some CoolantRooms
    | CoolantRooms ->
        Some AsciiMap
    | AsciiMap ->
        None )

(** Read a map file and parse it into a [map] *)
let parse_map_file (map_fn : string) : map =
  let room_idx, player_start_idx, ash_start_idx, xeno_start_idx =
    (ref 0, ref (-1), ref (-1), ref (-1))
  in
  (* We need to reverse the lists after parsing to maintain lockstep with the input file *)
  let pre_reversed : map =
    snd
      (List.fold_left
         (fun (state, m) line ->
           if line = "---" then (
             _log Log_Debug
               ( "Parsing "
               ^
               match advance_parsing_state state with
               | None ->
                   "nothing"
               | Some state ->
                   string_of_parsing_state state ) ;
             (advance_parsing_state state, m)
           ) else
             ( state
             , match state with
               | None ->
                   m
               | Some s -> (
                 match s with
                 | MapName ->
                     {m with map_name= line}
                 | RoomNames ->
                     let line' = String.sub line 1 (String.length line - 1) in
                     let room =
                       match line.[0] with
                       | '*' ->
                           player_start_idx := !room_idx ;
                           new_room line'
                       | '&' ->
                           xeno_start_idx := !room_idx ;
                           new_room line'
                       | '$' ->
                           ash_start_idx := !room_idx ;
                           new_room line'
                       | _ ->
                           new_room line
                     in
                     room_idx := !room_idx + 1 ;
                     {m with rooms= room :: m.rooms}
                 | NumCorridors ->
                     List.fold_left
                       (fun m i ->
                         { m with
                           rooms=
                             { blank_room with
                               name= string_of_int i
                             ; is_corridor= true }
                             :: m.rooms } )
                       m
                       (range 1 (int_of_string line))
                 | RoomConnections -> (
                   match String.split_on_char ';' line with
                   | [] ->
                       m
                   | room_name :: connections ->
                       let conns_to_add =
                         List.map
                           (fun c ->
                             ( find_room_by_name m room_name
                             , find_room_by_name m c ) )
                           connections
                         @ List.map
                             (fun c ->
                               ( find_room_by_name m c
                               , find_room_by_name m room_name ) )
                             connections
                       in
                       List.iter
                         (fun ((r1, r2) : room * room) ->
                           print_endline (r1.name ^ " -> " ^ r2.name) )
                         conns_to_add ;
                       List.fold_left
                         (fun m ((r1, r2) : room * room) ->
                           let old_r1_connections =
                             List.filter
                               (fun (x : room) -> x.name != r2.name)
                               (find_room_by_name m r1.name).connections
                           in
                           { m with
                             rooms=
                               replace m.rooms
                                 (fun r -> r.name = r1.name)
                                 {r1 with connections= old_r1_connections @ [r2]}
                           } )
                         m conns_to_add )
                 | Ladders -> (
                   match String.split_on_char ';' line with
                   | [first; second] ->
                       let first, second =
                         (find_room_by_name m first, find_room_by_name m second)
                       in
                       { m with
                         rooms=
                           replace
                             (replace m.rooms
                                (fun r -> r.name = first.name)
                                {first with ladder_connection= Some second} )
                             (fun r -> r.name = second.name)
                             {second with ladder_connection= Some first} }
                   | _ ->
                       _log Log_Warning
                         ("Failed to parse ladders line: \"" ^ line ^ "\"") ;
                       m )
                 | ScrapRooms -> (
                   match String.split_on_char ';' line with
                   | [] ->
                       m
                   | room_names ->
                       { m with
                         scrap_rooms= List.map (find_room_by_name m) room_names
                       } )
                 | EventRooms -> (
                   match String.split_on_char ';' line with
                   | [] ->
                       m
                   | room_names ->
                       { m with
                         event_rooms= List.map (find_room_by_name m) room_names
                       } )
                 | CoolantRooms -> (
                   match String.split_on_char ';' line with
                   | [] ->
                       m
                   | room_names ->
                       { m with
                         coolant_rooms=
                           List.map (find_room_by_name m) room_names } )
                 | AsciiMap ->
                     { m with
                       ascii_map=
                         ( match m.ascii_map with
                         | None ->
                             Some line
                         | Some s ->
                             Some (s ^ "\n" ^ line) ) } ) ) )
         (Some MapName, blank_map) (read_file_lines map_fn) )
  in
  { pre_reversed with
    rooms= List.rev pre_reversed.rooms
  ; player_start_room=
      List.nth pre_reversed.rooms
        (List.length pre_reversed.rooms - !player_start_idx - 1)
  ; ash_start_room=
      List.nth pre_reversed.rooms
        (List.length pre_reversed.rooms - !ash_start_idx - 1)
  ; xeno_start_room=
      List.nth pre_reversed.rooms
        (List.length pre_reversed.rooms - !xeno_start_idx - 1) }
