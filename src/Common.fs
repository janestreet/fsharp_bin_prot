namespace Bin_prot

module Common =

  open Buffer

  type pos = int

  exception Buffer_short
  exception No_variant_match

  module ReadError =
    type t =
      | Neg_int8
      | Int_code
      | Int_overflow
      | Nat0_code
      | Nat0_overflow
      | Int32_code
      | Int64_code
      | Unit_code
      | Bool_code
      | Option_code
      | String_too_long
      | Variant_tag
      | Array_too_long
      | List_too_long of {| len : int; max_len : int |}
      | Sum_tag of string
      | Variant of string
      | Variant_wrong_type of string
      | Silly_type of string
      | Empty_type of string

    let to_string =
      function
      | Neg_int8 -> "Neg_int8"
      | Int_code -> "Int_code"
      | Int_overflow -> "Int_overflow"
      | Nat0_code -> "Nat0_code"
      | Nat0_overflow -> "Nat0_overflow"
      | Int32_code -> "Int32_code"
      | Int64_code -> "Int64_code"
      | Unit_code -> "Unit_code"
      | Bool_code -> "Bool_code"
      | Option_code -> "Option_code"
      | String_too_long -> "String_too_long"
      | Variant_tag -> "Variant_tag"
      | Array_too_long -> "Array_too_long"
      | List_too_long list_too_long ->
        sprintf "List_too_long / %d (max %d)" list_too_long.len list_too_long.max_len
      | Sum_tag loc -> "Sum_tag / " ^ loc
      | Variant loc -> "Variant / " ^ loc
      | Variant_wrong_type loc -> "Variant_wrong_type / " ^ loc
      | Silly_type loc -> "Silly_type / " ^ loc
      | Empty_type loc -> "Empty_type / " ^ loc


  exception Read_error of ReadError.t * pos
  exception Empty_type of string


  let raise_read_error read_error pos = raise (Read_error(read_error, pos))

  let raise_variant_wrong_type name pos =
    raise (Read_error(ReadError.Variant_wrong_type name, pos))

  let array_bound_error () = invalidArg "pos" "index out of bounds"

  (* Buffers *)

  type pos_ref = pos ref
  type buf = Buffer<byte>

  let create_buf (len : int) = Buffer<byte>(len)

  let assert_pos pos = if pos < 0 then array_bound_error ()

  let check_pos (buf : buf) pos =
    if pos >= buf.Length then
      raise Buffer_short

  let safe_get_pos buf pos_ref =
    let pos = !pos_ref in
    check_pos buf pos
    pos

  let check_next (buf : Buffer<byte>) next = assert (next <= buf.Length)

  let (+) v1 v2 = v1 + v2
