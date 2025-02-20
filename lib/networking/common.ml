let port = ref 2122

(* Serialize and send *)
let send_obj sock obj =
  let data = Marshal.to_bytes obj [Marshal.Closures] in
  let length = Bytes.create 4 in
  EndianBytes.LittleEndian.set_int32 length 0 (Int32.of_int (Bytes.length data)) ;
  ignore (Unix.write sock length 0 4) ;
  ignore (Unix.write sock data 0 (Bytes.length data))

(* Receive and deserialize *)
let recv_obj sock =
  let length_buf = Bytes.create 4 in
  ignore (Unix.read sock length_buf 0 4) ;
  let length = Int32.to_int (EndianBytes.LittleEndian.get_int32 length_buf 0) in
  let data = Bytes.create length in
  ignore (Unix.read sock data 0 length) ;
  Marshal.from_bytes data 0
