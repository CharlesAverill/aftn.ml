open Unix
open Thread

let read_from_server sock =
  let buffer = Bytes.create 1024 in
  try
    while true do
      let bytes_read = read sock buffer 0 1024 in
      if bytes_read = 0 then (
        Printf.printf "Server disconnected.\n%!" ;
        Stdlib.exit 0
      ) ;
      let msg = Bytes.sub_string buffer 0 bytes_read in
      Printf.printf "\r[Broadcast] %s\n> %!" msg
    done
  with End_of_file | Unix_error _ ->
    Printf.printf "Connection closed.\n%!" ;
    Stdlib.exit 0

let write_to_server sock =
  try
    while true do
      print_string "> " ;
      flush Stdlib.stdout ;
      let line = read_line () in
      ignore (write_substring sock (line ^ "\n") 0 (String.length line + 1))
    done
  with End_of_file -> Printf.printf "Goodbye!\n%!"

let start_client addr port =
  let sock = socket PF_INET SOCK_STREAM 0 in
  let server_addr = ADDR_INET (inet_addr_of_string addr, port) in
  connect sock server_addr ;
  Printf.printf "Connected to %s:%d\n%!" addr port ;
  ignore (create read_from_server sock) ;
  write_to_server sock ;
  close sock
