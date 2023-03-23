namespace Bin_prot

module Read =
  (* Read_ml: reading values from the binary protocol using (mostly) OCaml. *)

  (* Note: the code is this file is carefully written to avoid unnecessary allocations. When
    touching this code, be sure to run the benchmarks to check for regressions. *)

  open Common
  open System

  type 'a reader = buf -> pos_ref -> 'a
  type ('a, 'b) reader1 = 'a reader -> 'b reader
  type ('a, 'b, 'c) reader2 = 'a reader -> reader1<'b, 'c>
  type ('a, 'b, 'c, 'd) reader3 = 'a reader -> reader2<'b, 'c, 'd>

  module Char =
    let code c = int32 c

  let unsafe_get8_signed (buf : buf) pos = let c = buf.Item pos in sbyte c

  (*$ open Bin_prot_cinaps $*)
  let code_NEG_INT8 = byte (*$ Code.char NEG_INT8 *) '\xff' (*$*)

  let code_INT16 = byte (*$ Code.char INT16 *) '\xfe' (*$*)

  let code_INT32 = byte (*$ Code.char INT32 *) '\xfd' (*$*)

  let code_INT64 = byte (*$ Code.char INT64 *) '\xfc' (*$*)

  let unsafe_get16be_unsigned (buf : buf) pos =
    System.Buffers.Binary.BinaryPrimitives.ReadUInt16BigEndian(buf.Slice(pos, 2))

  let unsafe_get32be (buf : buf) pos =
    System.Buffers.Binary.BinaryPrimitives.ReadInt32BigEndian(buf.Slice(pos, 4))

  let unsafe_get64be (buf : buf) pos =
    System.Buffers.Binary.BinaryPrimitives.ReadInt64BigEndian(buf.Slice(pos, 8))

  let unsafe_get16le_unsigned (buf : buf) pos =
    System.Buffers.Binary.BinaryPrimitives.ReadUInt16LittleEndian(buf.Slice(pos, 2))

  let unsafe_get32le (buf : buf) pos =
    System.Buffers.Binary.BinaryPrimitives.ReadInt32LittleEndian(buf.Slice(pos, 4))

  let unsafe_get64le (buf : buf) pos =
    System.Buffers.Binary.BinaryPrimitives.ReadInt64LittleEndian(buf.Slice(pos, 8))

  let unsafe_get16le_signed (buf : buf) pos =
    System.Buffers.Binary.BinaryPrimitives.ReadInt16LittleEndian(buf.Slice(pos, 2))

  let bin_read_unit buf pos_ref =
    let pos = safe_get_pos buf pos_ref in
    assert_pos pos

    if buf.Item pos = 0uy then
      pos_ref := pos + 1
    else
      raise_read_error ReadError.Unit_code pos

  let bin_read_bool buf pos_ref =
    let pos = safe_get_pos buf pos_ref in
    assert_pos pos

    match buf.Item pos with
    | 0uy ->
      pos_ref := pos + 1
      false
    | 1uy ->
      pos_ref := pos + 1
      true
    | _ -> raise_read_error ReadError.Bool_code pos

  let safe_bin_read_neg_int8 buf pos_ref pos =
    let next = pos + 1
    check_next buf next
    let n = unsafe_get8_signed buf pos

    if n >= 0y then
      raise_read_error ReadError.Neg_int8 !pos_ref

    pos_ref := next
    n

  let safe_bin_read_int16 buf pos_ref pos =
    let next = pos + 2 in
    check_next buf next
    pos_ref := next
    (* Can be above next line (no errors possible with 16bit).
      This should improve the generated code. *)
    unsafe_get16le_signed buf pos

  let safe_bin_read_int32 buf pos_ref pos =
    let next = pos + 4 in
    check_next buf next
    pos_ref := next
    (* No error possible either. *)
    unsafe_get32le buf pos

  let safe_bin_read_int64 buf pos_ref pos =
    let next = pos + 8 in
    check_next buf next
    pos_ref := next
    (* No error possible either. *)
    unsafe_get64le buf pos

  let safe_bin_read_int32_as_int64 buf pos_ref pos =
    let next = pos + 4
    check_next buf next
    let n = unsafe_get32le buf pos
    let n = int64 n
    pos_ref := next
    n

  let safe_bin_read_nat0_16 buf pos_ref pos =
    let next = pos + 2 in
    check_next buf next
    pos_ref := next
    Nat0.unsafe_of_int (unsafe_get16le_unsigned buf pos |> int)

  let safe_bin_read_nat0_32 buf pos_ref pos =
    let mask_32bit = int32 0xffff_ffffL in
    let next = pos + 4 in
    check_next buf next
    pos_ref := next

    let n = unsafe_get32le buf pos in

    if n >= 0 then
      Nat0.unsafe_of_int n
    else
      (* Erase the upper bits that were set to 1 during the int32 -> int conversion. *)
      Nat0.unsafe_of_int (n &&& mask_32bit)

  let bin_read_nat0 buf pos_ref =
    let pos = safe_get_pos buf pos_ref in
    assert_pos pos

    match buf.Item pos with
    | ch when ch <= 0x7fuy ->
      pos_ref := pos + 1
      Nat0.unsafe_of_int (int32 ch)
    | v when v = code_INT16 -> safe_bin_read_nat0_16 buf pos_ref (pos + 1)
    | v when v = code_INT32 -> safe_bin_read_nat0_32 buf pos_ref (pos + 1)
    | v when v = code_INT64 ->
      (* 2021-04-12: dotnet-bin-prot doesn't support sizes >= Int32.MaxValue,
        as arrays in .net cannot be allocated with >= Int32.MaxValue elements.
        We have considered using a large array overlay for when we need that. *)
      raise_read_error ReadError.Nat0_overflow pos
    | _ -> raise_read_error ReadError.Nat0_code pos

  let bin_read_string buf pos_ref =
    let start_pos = !pos_ref
    let len = bin_read_nat0 buf pos_ref

    if len > Int32.MaxValue then
      raise_read_error ReadError.String_too_long start_pos

    let pos = !pos_ref
    let next = pos + len
    check_next buf next
    pos_ref := next
    String(Array.map (fun byte -> char byte) (buf.Slice(pos, len).ToArray()))

  let bin_read_char buf pos_ref =
    let pos = safe_get_pos buf pos_ref in
    assert_pos pos
    pos_ref := pos + 1
    buf.Item pos |> char

  let bin_read_int64 buf pos_ref =
    let pos = safe_get_pos buf pos_ref in
    assert_pos pos

    match buf.Item pos with
    | ch when ch <= 0x7fuy ->
      pos_ref := pos + 1
      int64 ch
    | v when v = code_NEG_INT8 ->
      safe_bin_read_neg_int8 buf pos_ref (pos + 1)
      |> int64
    | v when v = code_INT16 -> safe_bin_read_int16 buf pos_ref (pos + 1) |> int64
    | v when v = code_INT32 -> safe_bin_read_int32_as_int64 buf pos_ref (pos + 1)
    | v when v = code_INT64 -> safe_bin_read_int64 buf pos_ref (pos + 1)
    | _ -> raise_read_error ReadError.Int_code pos

  let bin_read_float buf pos_ref =
    let pos = safe_get_pos buf pos_ref
    assert_pos pos
    let next = pos + 8
    check_next buf next
    pos_ref := next
    (* No error possible either. *)
    BitConverter.Int64BitsToDouble(unsafe_get64le buf pos)

  let bin_read_int32 buf pos_ref =
    let pos = safe_get_pos buf pos_ref in
    assert_pos pos

    match buf.Item pos with
    | ch when ch <= 0x7fuy ->
      pos_ref := pos + 1
      Char.code ch
    | v when v = code_NEG_INT8 -> int32 (safe_bin_read_neg_int8 buf pos_ref (pos + 1))
    | v when v = code_INT16 -> int32 (safe_bin_read_int16 buf pos_ref (pos + 1))
    | v when v = code_INT32 -> safe_bin_read_int32 buf pos_ref (pos + 1)
    | _ -> raise_read_error ReadError.Int32_code pos

  let bin_read_ref (bin_read_el : 'a reader) buf pos_ref =
    let el = bin_read_el buf pos_ref in ref el

  let bin_read_Lazy (bin_read_el : 'a reader) buf pos_ref =
    let el = bin_read_el buf pos_ref in lazy (el)

  let bin_read_option (bin_read_el : 'a reader) buf pos_ref =
    let pos = safe_get_pos buf pos_ref in
    assert_pos pos

    match buf.Item pos with
    | 0uy ->
      pos_ref := pos + 1
      None
    | 1uy ->
      pos_ref := pos + 1
      let el = bin_read_el buf pos_ref
      Some el
    | _ -> raise_read_error ReadError.Option_code pos

  let bin_read_pair (bin_read_a : 'a reader) (bin_read_b : 'b reader) buf pos_ref =
    let a = bin_read_a buf pos_ref in
    let b = bin_read_b buf pos_ref in
    a, b

  let bin_read_triple
    (bin_read_a : 'a reader)
    (bin_read_b : 'b reader)
    (bin_read_c : 'c reader)
    buf
    pos_ref
    =
    let a = bin_read_a buf pos_ref in
    let b = bin_read_b buf pos_ref in
    let c = bin_read_c buf pos_ref in
    a, b, c

  let bin_read_n_rev_list bin_read_el buf pos_ref len =
    let rec loop n acc =
      if n = 0 then
        acc
      else
        loop (n - 1) (bin_read_el buf pos_ref :: acc)

    loop len []

  let bin_read_list_with_max_len max_len (bin_read_el : 'a reader) buf pos_ref =
    let len = bin_read_nat0 buf pos_ref

    if len > max_len then
      raise_read_error
        (ReadError.List_too_long {| len = len; max_len = max_len |})
        !pos_ref

    let rev_lst = bin_read_n_rev_list bin_read_el buf pos_ref len
    List.rev rev_lst

  let bin_read_list bin_read_el buf pos_ref =
    bin_read_list_with_max_len Int32.MaxValue bin_read_el buf pos_ref

  let bin_read_array (bin_read_el : 'a reader) buf pos_ref =
    let start_pos = !pos_ref in
    let len = bin_read_nat0 buf pos_ref in

    if len = 0 then
      [||]
    else
      (if len > Int32.MaxValue then
         raise_read_error ReadError.Array_too_long start_pos

       let first = bin_read_el buf pos_ref
       let res = Array.create len first

       for i = 1 to len - 1 do
         let el = bin_read_el buf pos_ref in Array.set res i el

       res)

  let bin_read_variant_int buf pos_ref =
    let pos = !pos_ref
    assert_pos pos
    let next = pos + 4
    check_next buf next
    let n = unsafe_get32le buf pos |> int64
    (* [n] must contain an integer already encoded, i.e. [n = 2 * k + 1]. *)
    if n &&& 1L = 0L then
      raise (Read_error(ReadError.Variant_tag, pos))
    else
      (
       (* We shift it by one bit to the right se we get back [2 * k + 1] in the end. *)
       pos_ref := next
       int32 (n >>> 1))

  let bin_read_int_8bit buf pos_ref =
    let pos = safe_get_pos buf pos_ref
    assert_pos pos
    pos_ref := pos + 1
    let n = buf.Item pos
    int32 n

  let bin_read_int_16bit buf pos_ref =
    let pos = !pos_ref
    assert_pos pos
    let next = pos + 2
    check_next buf next
    pos_ref := next
    let n = unsafe_get16le_unsigned buf pos
    int32 n

  let bin_read_int_32bit buf pos_ref =
    let pos = !pos_ref
    assert_pos pos
    let next = pos + 4
    check_next buf next
    pos_ref := next
    let n = unsafe_get32le buf pos
    int64 n

  let bin_read_int_64bit buf pos_ref =
    let pos = !pos_ref
    assert_pos pos
    let next = pos + 8
    check_next buf next
    pos_ref := next
    unsafe_get64le buf pos

  let bin_read_network16_int buf pos_ref =
    let pos = !pos_ref
    assert_pos pos
    let next = pos + 2
    check_next buf next
    pos_ref := next
    let n = unsafe_get16be_unsigned buf pos
    int64 n

  let bin_read_network32_int buf pos_ref =
    let pos = !pos_ref
    assert_pos pos
    let next = pos + 4
    check_next buf next
    pos_ref := next
    let n = unsafe_get32be buf pos
    int64 n

  let bin_read_network64_int buf pos_ref =
    let pos = !pos_ref
    assert_pos pos
    let next = pos + 8
    check_next buf next
    pos_ref := next
    unsafe_get64be buf pos

  let __bin_read_unit__ (_buf : buf) (pos_ref : pos_ref) (_vint : int) : unit =
    Common.raise_variant_wrong_type "unit" !pos_ref


  let __bin_read_bool__ (_buf : buf) (pos_ref : pos_ref) (_vint : int) : bool =
    Common.raise_variant_wrong_type "bool" !pos_ref


  let __bin_read_string__ (_buf : buf) (pos_ref : pos_ref) (_vint : int) : string =
    Common.raise_variant_wrong_type "string" !pos_ref


  let __bin_read_char__ (_buf : buf) (pos_ref : pos_ref) (_vint : int) : char =
    Common.raise_variant_wrong_type "char" !pos_ref


  let __bin_read_float__ (_buf : buf) (pos_ref : pos_ref) (_vint : int) : float =
    Common.raise_variant_wrong_type "float" !pos_ref


  let __bin_read_float_array__
    (_buf : buf)
    (pos_ref : pos_ref)
    (_vint : int)
    : float array =
    Common.raise_variant_wrong_type "float_array" !pos_ref


  let __bin_read_int32__ (_buf : buf) (pos_ref : pos_ref) (_vint : int) : int32 =
    Common.raise_variant_wrong_type "int32" !pos_ref


  let __bin_read_int64__ (_buf : buf) (pos_ref : pos_ref) (_vint : int) : int64 =
    Common.raise_variant_wrong_type "int64" !pos_ref


  let __bin_read_ref__
    (_f : 'a reader)
    (_buf : buf)
    (pos_ref : pos_ref)
    (_vint : int)
    : 'a ref =
    Common.raise_variant_wrong_type "ref" !pos_ref


  let __bin_read_lazy_t__
    (_f : 'a reader)
    (_buf : buf)
    (pos_ref : pos_ref)
    (_vint : int)
    : 'a Lazy =
    Common.raise_variant_wrong_type "lazy" !pos_ref


  let __bin_read_Lazy__
    (_f : 'a reader)
    (_buf : buf)
    (pos_ref : pos_ref)
    (_vint : int)
    : 'a Lazy =
    Common.raise_variant_wrong_type "Lazy" !pos_ref


  let __bin_read_option__
    (_f : 'a reader)
    (_buf : buf)
    (pos_ref : pos_ref)
    (_vint : int)
    : 'a option =
    Common.raise_variant_wrong_type "option" !pos_ref


  let __bin_read_list__
    (_f : 'a reader)
    (_buf : buf)
    (pos_ref : pos_ref)
    (_vint : int)
    : 'a list =
    Common.raise_variant_wrong_type "list" !pos_ref


  let __bin_read_array__
    (_f : 'a reader)
    (_buf : buf)
    (pos_ref : pos_ref)
    (_vint : int)
    : 'a array =
    Common.raise_variant_wrong_type "array" !pos_ref
