namespace Bin_prot

module Size =
  (** Compute size of values in the binary protocol. *)

  open Common

  type 'a sizer = 'a -> int
  type ('a, 'b) sizer1 = 'a sizer -> 'b sizer
  type ('a, 'b, 'c) sizer2 = 'a sizer -> ('b, 'c)sizer1
  type ('a, 'b, 'c, 'd) sizer3 = 'a sizer -> ('b, 'c, 'd)sizer2

  val bin_size_unit : unit sizer
  val bin_size_bool : bool sizer
  val bin_size_string : string sizer
  val bin_size_char : char sizer
  val bin_size_int64 : int64 sizer
  val bin_size_float : float sizer
  val bin_size_int32 : int32 sizer
  val bin_size_nat0 : Nat0.t sizer
  val bin_size_ref : ('a, 'a ref)sizer1
  val bin_size_lazy_t : ('a, 'a Lazy)sizer1
  val bin_size_option : ('a, 'a option)sizer1
  val bin_size_pair : ('a, 'b, 'a * 'b)sizer2
  val bin_size_triple : ('a, 'b, 'c, 'a * 'b * 'c)sizer3
  val bin_size_list : ('a, 'a list)sizer1
  val bin_size_array : ('a, 'a array)sizer1
  val bin_size_variant_int : int sizer

  (** 8 and 16 bit use int32 as they're used for variant constructor id in OCaml
      and if we use int64 we need the 'L' suffix *)
  val bin_size_int_8bit : int sizer
  val bin_size_int_16bit : int sizer
  val bin_size_int_32bit : int64 sizer
  val bin_size_int_64bit : int64 sizer
  val bin_size_network16_int : int64 sizer
  val bin_size_network32_int : int64 sizer
  val bin_size_network64_int : int64 sizer

  (* Provide the maximum sizes for fields which do not depend upon an array/vector/matrix
    length, choosing the size required for the largest architecture.  This allows for the
    most conservative estimation of space required. *)
  module Maximum =
    val bin_size_unit : int
    val bin_size_bool : int
    val bin_size_char : int
    val bin_size_int64 : int
    val bin_size_float : int
    val bin_size_int32 : int
    val bin_size_nat0 : int
    val bin_size_variant_int : int
    val bin_size_int_8bit : int
    val bin_size_int_16bit : int
    val bin_size_int_32bit : int
    val bin_size_int_64bit : int
    val bin_size_network16_int : int
    val bin_size_network32_int : int
    val bin_size_network64_int : int

  (* Provide absolute minimum sizes for fields, choosing [0] for the lengths of any
    arrays/vectors/matrices. *)
  module Minimum =
    val bin_size_unit : int
    val bin_size_bool : int
    val bin_size_string : int
    val bin_size_char : int
    val bin_size_int64 : int
    val bin_size_float : int
    val bin_size_int32 : int
    val bin_size_nat0 : int
    val bin_size_ref : int
    val bin_size_lazy_t : int
    val bin_size_option : int
    val bin_size_pair : int
    val bin_size_triple : int
    val bin_size_list : int
    val bin_size_array : int
    val bin_size_variant_int : int
    val bin_size_int_8bit : int
    val bin_size_int_16bit : int
    val bin_size_int_32bit : int
    val bin_size_int_64bit : int
    val bin_size_network16_int : int
    val bin_size_network32_int : int
    val bin_size_network64_int : int
