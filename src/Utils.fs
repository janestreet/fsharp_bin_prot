namespace Bin_prot

module Utils =

  open Common
  open Type_class

  let size_header_length = 8
  let bin_write_size_header = Write.bin_write_int_64bit
  let bin_read_size_header = Read.bin_read_int_64bit

  let bin_dump header writer v =
    let buf, pos, pos_len =
      let v_len = writer.size v in

      if header then
        (let tot_len = v_len + size_header_length in
         let buf = create_buf tot_len in
         let pos = bin_write_size_header buf 0 (int64 (v_len)) in
         buf, pos, pos + v_len)
      else
        (let buf = create_buf v_len in buf, 0, v_len)

    let pos = writer.write buf pos v in

    if pos = pos_len then
      buf
    else
      failwith "Bin_prot.Utils.bin_dump: size changed during writing"

  let bin_read_stream max_size read reader =
    let buf = create_buf size_header_length
    read buf 0 size_header_length
    let pos_ref = ref 0
    let len = bin_read_size_header buf pos_ref |> int32

    match max_size with
    | Some max_size when len > max_size ->
      failwith (
        Printf.sprintf
          "Bin_prot.Utils.bin_read_stream: size exceeds max_size: %d > %d"
          len
          max_size
      )
    | _ ->
      let buf =
        if len > size_header_length then
          create_buf len
        else
          buf in

      read buf 0 len
      pos_ref := 0

      let res = reader.read buf pos_ref in

      if !pos_ref = len then
        res
      else
        (let msg =
          Printf.sprintf
            "Bin_prot.Utils.bin_read_stream: protocol lied about length of value: expected %d, received %d"
            len
            !pos_ref

         failwith msg)
