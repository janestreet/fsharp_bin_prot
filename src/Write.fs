namespace Bin_prot

module Write =

  open Buffer
  open Common
  open System

  type 'a writer = buf -> pos -> 'a -> pos
  type ('a, 'b) writer1 = 'a writer -> 'b writer
  type ('a, 'b, 'c) writer2 = 'a writer -> ('b, 'c)writer1
  type ('a, 'b, 'c, 'd) writer3 = 'a writer -> ('b, 'c, 'd)writer2

  (*$ open Bin_prot_cinaps $*)

  let code_NEG_INT8 = byte (*$ Code.char NEG_INT8 *) '\xff' (*$*)

  let code_INT16 = byte (*$ Code.char INT16 *) '\xfe' (*$*)

  let code_INT32 = byte (*$ Code.char INT32 *) '\xfd' (*$*)

  let code_INT64 = byte (*$ Code.char INT64 *) '\xfc' (*$*)

  let unsafe_set16be (buf : buf) pos x =
    Buffers.Binary.BinaryPrimitives.WriteInt16BigEndian(buf.WritableSlice(pos, 2), x)

  let unsafe_set32be (buf : buf) pos x =
    Buffers.Binary.BinaryPrimitives.WriteInt32BigEndian(buf.WritableSlice(pos, 4), x)

  let unsafe_set64be (buf : buf) pos x =
    Buffers.Binary.BinaryPrimitives.WriteInt64BigEndian(buf.WritableSlice(pos, 8), x)

  let unsafe_set16le (buf : buf) pos x =
    Buffers.Binary.BinaryPrimitives.WriteInt16LittleEndian(buf.WritableSlice(pos, 2), x)

  let unsafe_set32le (buf : buf) pos x =
    Buffers.Binary.BinaryPrimitives.WriteInt32LittleEndian(buf.WritableSlice(pos, 4), x)

  let unsafe_set64le (buf : buf) pos x =
    Buffers.Binary.BinaryPrimitives.WriteInt64LittleEndian(buf.WritableSlice(pos, 8), x)

  let bin_write_unit buf pos () =
    assert_pos pos
    check_pos buf pos
    buf.Set pos 0uy
    pos + 1

  let bin_write_bool buf pos b =
    assert_pos pos
    check_pos buf pos
    buf.Set pos (if b then 1uy else 0uy)
    pos + 1

  let all_bin_write_small_int buf pos (n : int64) =
    check_pos buf pos
    buf.Set pos (byte n)
    pos + 1

  let all_bin_write_neg_int8 buf pos (n : int64) =
    let next = pos + 2 in
    check_next buf next
    buf.Set pos code_NEG_INT8
    buf.Set(pos + 1) (byte n)
    next

  let all_bin_write_int16 buf pos (n : int64) =
    let next = pos + 3 in
    check_next buf next
    buf.Set pos code_INT16
    unsafe_set16le buf (pos + 1) (int16 n)
    next

  let inline all_bin_write_int32 buf pos (n : int64) =
    let next = pos + 5 in
    check_next buf next
    buf.Set pos code_INT32
    unsafe_set32le buf (pos + 1) (int32 n)
    next

  let inline all_bin_write_int64 buf pos n =
    let next = pos + 9 in
    check_next buf next
    buf.Set pos code_INT64
    unsafe_set64le buf (pos + 1) n
    next

  let bin_write_char buf pos (c : char) =
    assert_pos pos
    check_pos buf pos
    buf.Set pos (byte c)
    pos + 1

  let bin_write_int64 buf pos (n : int64) =
    assert_pos pos

    if n >= 0L then
      if n < 0x00000080L then
        all_bin_write_small_int buf pos n
      else if n < 0x00008000L then
        all_bin_write_int16 buf pos n
      else if n >= (1L <<< 31) then
        all_bin_write_int64 buf pos n
      else
        all_bin_write_int32 buf pos n
    else if n >= -0x00000080L then
      all_bin_write_neg_int8 buf pos n
    else if n >= -0x00008000L then
      all_bin_write_int16 buf pos n
    else if n < -(1L <<< 31) then
      all_bin_write_int64 buf pos n
    else
      all_bin_write_int32 buf pos n

  let bin_write_nat0 buf pos nat0 =
    assert_pos pos
    let n = int64 nat0

    if n < 0x00000080L then
      all_bin_write_small_int buf pos n
    else if n < 0x00010000L then
      all_bin_write_int16 buf pos n
    else if n >= (1L <<< 32) then
      failwith "dotnet-bin-prot doesn't support nat0 > int32.MaxValue"
    else
      all_bin_write_int32 buf pos n

  let bin_write_string buf pos str =
    let len = String.length str
    let plen = Nat0.unsafe_of_int len
    let new_pos = bin_write_nat0 buf pos plen
    let next = new_pos + len
    check_next buf next
    (* TODO: optimize for small strings *)
    let byte_array = Array.map (fun char -> byte char) (str.ToCharArray())
    buf.CopyMemory(byte_array, new_pos, len)
    next

  let bin_write_float buf pos (x : float) =
    assert_pos pos
    let next = pos + 8
    check_next buf next
    unsafe_set64le buf pos (BitConverter.DoubleToInt64Bits x)
    next

  let bin_write_int32 buf pos n = bin_write_int64 buf pos (int64 n)

  let bin_write_ref (bin_write_el : 'a writer) buf pos r = bin_write_el buf pos !r

  let bin_write_lazy (bin_write_el : 'a writer) buf pos (lv : 'a Lazy) =
    let v = lv.Force() in bin_write_el buf pos v

  let bin_write_option (bin_write_el : 'a writer) buf pos =
    function
    | None -> bin_write_bool buf pos false
    | Some v -> let next = bin_write_bool buf pos true in bin_write_el buf next v

  let bin_write_pair (bin_write_a : 'a writer) (bin_write_b : 'b writer) buf pos (a, b) =
    let next = bin_write_a buf pos a in bin_write_b buf next b

  let bin_write_triple
    (bin_write_a : 'a writer)
    (bin_write_b : 'b writer)
    (bin_write_c : 'c writer)
    buf
    pos
    (a, b, c)
    =
    let next1 = bin_write_a buf pos a in
    let next2 = bin_write_b buf next1 b in
    bin_write_c buf next2 c

  let bin_write_list (bin_write_el : 'a writer) buf pos lst =
    let rec loop els_pos =
      function
      | [] -> els_pos
      | h :: t -> let new_els_pos = bin_write_el buf els_pos h in loop new_els_pos t

    let len = Nat0.unsafe_of_int (List.length lst) in
    let els_pos = bin_write_nat0 buf pos len in
    loop els_pos lst

  let bin_write_array_loop bin_write_el buf els_pos n ar =
    let els_pos_ref = ref els_pos in

    for i = 0 to n - 1 do
      els_pos_ref
      := bin_write_el buf !els_pos_ref (Array.get ar i)

    !els_pos_ref

  let bin_write_array (bin_write_el : 'a writer) buf pos ar =
    let n = Array.length ar in
    let pn = Nat0.unsafe_of_int n in
    let els_pos = bin_write_nat0 buf pos pn in
    bin_write_array_loop bin_write_el buf els_pos n ar

  let bin_write_variant_int buf pos x =
    assert_pos pos
    let next = pos + 4
    check_next buf next
    unsafe_set32le buf pos ((x <<< 1) ||| 1l)
    next

  let bin_write_int_8bit buf pos (n : int) =
    assert_pos pos
    check_pos buf pos
    (* This should be sbyte but byte and sbyte can be converted between each other freely *)
    buf.Set pos (byte n)
    pos + 1

  let bin_write_int_16bit buf pos (n : int) =
    assert_pos pos
    let next = pos + 2
    check_next buf next
    unsafe_set16le buf pos (int16 n)
    next

  let bin_write_int_32bit buf pos (n : int64) =
    assert_pos pos
    let next = pos + 4
    check_next buf next
    unsafe_set32le buf pos (int32 n)
    next

  let bin_write_int_64bit buf pos n =
    assert_pos pos
    let next = pos + 8
    check_next buf next
    unsafe_set64le buf pos n
    next

  let bin_write_network16_int buf pos (n : int64) =
    assert_pos pos
    let next = pos + 2
    check_next buf next
    unsafe_set16be buf pos (int16 n)
    next

  let bin_write_network32_int buf pos (n : int64) =
    assert_pos pos
    let next = pos + 4
    check_next buf next
    unsafe_set32be buf pos (int32 n)
    next

  let bin_write_network64_int buf pos n =
    assert_pos pos
    let next = pos + 8
    check_next buf next
    unsafe_set64be buf pos n
    next
