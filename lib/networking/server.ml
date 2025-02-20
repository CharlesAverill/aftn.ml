open Unix
open Mutex

let clients = ref []

let clients_mutex = Mutex.create ()

let broadcast_message sender_sock msg =
  lock clients_mutex ;
  List.iter
    (fun sock ->
      if sock <> sender_sock then
        try ignore (write_substring sock msg 0 (String.length msg))
        with _ -> () )
    !clients ;
  unlock clients_mutex

let handle_client client_sock =
  let buffer = Bytes.create 1024 in
  try
    while true do
      let bytes_read = read client_sock buffer 0 1024 in
      if bytes_read = 0 then raise End_of_file ;
      let msg = Bytes.sub_string buffer 0 bytes_read in
      Printf.printf "Received: %s\n%!" msg ;
      broadcast_message client_sock msg
    done
  with End_of_file | Unix_error _ ->
    Printf.printf "Client disconnected.\n%!" ;
    lock clients_mutex ;
    clients := List.filter (fun s -> s <> client_sock) !clients ;
    unlock clients_mutex ;
    close client_sock

let start_server port =
  let sock = socket PF_INET SOCK_STREAM 0 in
  let addr = ADDR_INET (inet_addr_any, port) in
  bind sock addr ;
  listen sock 10 ;
  Printf.printf "Broadcast echo server listening on port %d\n%!" port ;
  while true do
    let client_sock, _ = accept sock in
    lock clients_mutex ;
    clients := client_sock :: !clients ;
    unlock clients_mutex ;
    Printf.printf "New client connected.\n%!" ;
    let _ = Thread.create handle_client client_sock in
    ()
  done
