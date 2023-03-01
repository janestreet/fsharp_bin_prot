namespace Bin_prot

module Size =

  (* Size: compute size of values in the binary protocol. *)

  module Maximum =
    let bin_size_unit = 1
    let bin_size_bool = 1
    let bin_size_char = 1
    let bin_size_int_nat0 = 9
    let bin_size_int_negative = 9
    let bin_size_int64 = max bin_size_int_nat0 bin_size_int_negative
    let bin_size_float = 8
    let bin_size_int32 = 5
    let bin_size_nat0 = bin_size_int_nat0
    let bin_size_variant_int = 4
    let bin_size_int_8bit = 1
    let bin_size_int_16bit = 2
    let bin_size_int_32bit = 4
    let bin_size_int_64bit = 8
    let bin_size_network16_int = 2
    let bin_size_network32_int = 4
    let bin_size_network64_int = 8

  module Minimum =
    let bin_size_unit = Maximum.bin_size_unit
    let bin_size_bool = Maximum.bin_size_bool
    let bin_size_char = Maximum.bin_size_char
    let bin_size_int_nat0 = 1
    let bin_size_int_negative = 2
    let bin_size_int64 = min bin_size_int_nat0 bin_size_int_negative
    let bin_size_float = Maximum.bin_size_float
    let bin_size_int32 = bin_size_int64
    let bin_size_nat0 = 1
    let bin_size_ref = 1
    let bin_size_lazy_t = 1
    let bin_size_option = 1
    let bin_size_pair = 1 + 1
    let bin_size_triple = 1 + 1 + 1
    let bin_size_len = bin_size_nat0
    let bin_size_list = bin_size_len
    let bin_size_array = bin_size_len
    let bin_size_string = bin_size_len
    let bin_size_variant_int = Maximum.bin_size_variant_int
    let bin_size_int_8bit = Maximum.bin_size_int_8bit
    let bin_size_int_16bit = Maximum.bin_size_int_16bit
    let bin_size_int_32bit = Maximum.bin_size_int_32bit
    let bin_size_int_64bit = Maximum.bin_size_int_64bit
    let bin_size_network16_int = Maximum.bin_size_network16_int
    let bin_size_network32_int = Maximum.bin_size_network32_int
    let bin_size_network64_int = Maximum.bin_size_network64_int

  type 'a sizer = 'a -> int
  type ('a, 'b) sizer1 = 'a sizer -> 'b sizer
  type ('a, 'b, 'c) sizer2 = 'a sizer -> sizer1<'b, 'c>
  type ('a, 'b, 'c, 'd) sizer3 = 'a sizer -> sizer2<'b, 'c, 'd>

  let bin_size_unit () = 1
  let bin_size_bool (_ : bool) = 1

  let bin_size_int_nat0 (n : int64) =
    if n < 0x00000080L then
      1
    else if n < 0x00008000L then
      3
    else if n (* 0x80000000 *) >= (1L <<< 31) then
      9
    else
      5

  let bin_size_int_negative (n : int64) =
    if n >= -0x00000080L then
      2
    else if n >= -0x00008000L then
      3
    else if n (* -0x80000000 *) < -(1L <<< 31) then
      9
    else
      5

  let bin_size_char (c : char) =
    if int c >>> 8 <> 0 then
      failwith "dotnet chars are 16 bits but ocaml only supports 1 byte chars"
    else
      1

  let bin_size_int64 (n : int64) =
    if n >= 0L then
      bin_size_int_nat0 n
    else
      bin_size_int_negative n

  let bin_size_nat0 nat0 =
    let n = nat0 in

    if n < 0x00000080 then
      1
    else if n < 0x00010000 then
      3
    else if int64 (n) >= (1L <<< 32) (* 0x100000000 *) then
      failwith "dotnet-bin-prot doesn't support nat0 > int32.MaxValue"
    else
      5

  let bin_size_string_or_bytes len =
    let plen = Nat0.unsafe_of_int len in
    let size_len = bin_size_nat0 plen in
    size_len + len

  let bin_size_string str = bin_size_string_or_bytes (String.length str)

  let bin_size_float (_ : float) = 8

  let bin_size_int32 (n : int32) = bin_size_int64 (int64 n)

  let bin_size_ref (bin_size_el : 'a sizer) r = bin_size_el !r
  let bin_size_lazy_t (bin_size_el : 'a sizer) (lv : Lazy<'a>) = bin_size_el (lv.Force())

  let bin_size_option bin_size_el =
    function
    | None -> 1
    | Some v -> 1 + bin_size_el v

  let bin_size_pair bin_size_a bin_size_b (a, b) = bin_size_a a + bin_size_b b

  let bin_size_triple bin_size_a bin_size_b bin_size_c (a, b, c) =
    bin_size_a a + bin_size_b b + bin_size_c c

  let bin_size_list bin_size_el lst =
    let rec loop len =
      function
      | [] -> len
      | h :: t -> loop (len + bin_size_el h) t

    let len = Nat0.unsafe_of_int (List.length lst) in
    let size_len = bin_size_nat0 len in
    loop size_len lst

  let bin_size_len len =
    let plen = Nat0.unsafe_of_int len in bin_size_nat0 plen

  let bin_size_float_array (ar : float []) =
    let len = Array.length ar in bin_size_len len + (8 * len)

  let bin_size_array_loop bin_size_el ar total_len n =
    let total_len_ref = ref total_len in

    for i = 0 to n - 1 do
      let el = Array.get ar i in total_len_ref := !total_len_ref + bin_size_el el

    !total_len_ref

  let bin_size_array (bin_size_el : 'a sizer) ar =
    let n = Array.length ar in
    let total_len = bin_size_len n in
    bin_size_array_loop bin_size_el ar total_len n

  let bin_size_variant_int (_ : int32) = 4
  let bin_size_int_8bit (_ : int) = 1
  let bin_size_int_16bit (_ : int) = 2
  let bin_size_int_32bit (_ : int64) = 4
  let bin_size_int_64bit (_ : int64) = 8
  let bin_size_network16_int (_ : int64) = 2
  let bin_size_network32_int (_ : int64) = 4
  let bin_size_network64_int (_ : int64) = 8
